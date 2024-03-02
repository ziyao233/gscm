module Main where

import Control.Monad
import Control.Monad.Trans (liftIO, MonadIO)
import Control.Monad.Trans.Class
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO (readFile)
import System.Environment (getArgs)
import Data.Either
import Data.IORef
import qualified Data.Map as M
import Data.Tuple (uncurry)
import Data.List (foldl1')

data SExpr = SAtom String
           | SList [SExpr]
           | SNumber Int
           | SString String
           | SBool Bool
           | SPrim PrimFunc
           | SFunc [String] SExpr Env
           | SNull

instance Show SExpr where
  show (SAtom xs)     = xs
  show (SList xs)     = "(" ++ (unwords $ map show xs) ++ ")"
  show (SNumber x)    = show x
  show (SString xs)   = "\"" ++ xs ++ "\""
  show (SBool True)   = "#t"
  show (SBool False)  = "#f"
  show (SPrim _)      = "<primitive>"
  show SNull          = "(null)"
  show (SFunc x _ _)  = "lambda " ++ (show $ SList $ map SAtom x)

spaces :: Parser ()
spaces = skipMany1 $ oneOf "\r\n\t "

parseAtom :: Parser SExpr
parseAtom = do
  x <- start
  xs <- many $ start <|> digit
  return $ case (x : xs) of
    "#t"     -> SBool True
    "#f"     -> SBool False
    xs'      -> SAtom xs'
  where start = letter <|> (oneOf "!$|#*+-/:<=>?@^_~")

parseList :: Parser SExpr
parseList = do
  char '('
  optional spaces
  list <- sepEndBy parseSExpr spaces
  char ')'
  return $ SList list

parseNumber :: Parser SExpr
parseNumber = fmap (SNumber . read) $ many1 digit

parseString :: Parser SExpr
parseString = do
  char '"'
  s <- many $ noneOf "\""
  char '"'
  return $ SString s

parseQuoted :: Parser SExpr
parseQuoted = do
  char '\''
  x <- parseSExpr
  return $ SList [SAtom "quote", x]

parseSExpr :: Parser SExpr
parseSExpr =  parseAtom
          <|> parseNumber
          <|> parseString
          <|> parseQuoted
          <|> parseList

data EvalError = ArgMismatch String
               | IllegalStruct String
               | OtherError String
               | UnboundVar String
               | AlreadyDefined String
               | TypeMismatch String
instance Show EvalError where
  show (ArgMismatch m) = "Argument(s) mismatch: " ++ m
  show (IllegalStruct m) = "Illegal Struct: " ++ m
  show (UnboundVar m) = "Unbound variable: " ++ m
  show (AlreadyDefined m) = "Variable already defined: " ++ m
  show (TypeMismatch m) = "Type Mismatch: " ++ m
  show (OtherError m) = m

newtype EitherT a m b = EitherT { runEitherT :: m (Either a b) }
instance Monad m => Monad (EitherT a m) where
  x >>= f = EitherT $ do
    v <- runEitherT x
    case v of
      Left l      -> return $ Left l
      Right r     -> runEitherT $ f r
instance Monad m => Applicative (EitherT a m) where
  pure = EitherT . return . Right
  (<*>) = ap
instance Monad m => Functor (EitherT a m) where
  fmap = liftM
instance MonadTrans (EitherT a) where
  lift = EitherT . (liftM Right)
instance MonadIO (EitherT a IO) where
  liftIO = lift

type ThrowEError = Either EvalError

type IOThrowEError = EitherT EvalError IO
liftThrowEError :: ThrowEError a -> IOThrowEError a
liftThrowEError = either (EitherT . return . Left) (return)

type Env = IORef (M.Map String SExpr)
defaultEnv :: IO Env
defaultEnv = newIORef primitivesMap

extractNum :: SExpr -> ThrowEError Int
extractNum (SNumber n) = return n
extractNum _ = Left $ ArgMismatch "expect number"
numBinOp :: (Int -> Int -> Int) -> [SExpr] -> ThrowEError SExpr
numBinOp f as = do
  ns <- mapM extractNum as
  return $ SNumber $ foldl1' f ns

errOneArg = Left $ ArgMismatch "expect one argument"
pIsNumber [SNumber _] = return $ SBool True
pIsNumber [_] = return $ SBool False
pIsNumber _ = errOneArg
pIsBoolean [SBool _] = return $ SBool True
pIsBoolean [_] = return $ SBool False
pIsBoolean _ = errOneArg
pIsString [SString _] = return $ SBool True
pIsString [_] = return $ SBool False
pIsString _ = errOneArg
pIsList [SList _] = return $ SBool True
pIsList [_] = return $ SBool False
pIsList _ = errOneArg
pAdd = numBinOp (+)
pSub = numBinOp (-)
pMul = numBinOp (*)
pDiv = numBinOp div
pMod = numBinOp mod

numCmpOp :: (Int -> Int -> Bool) -> [SExpr] -> ThrowEError SExpr
numCmpOp f as = do
  ns <- mapM extractNum as
  if (length ns) /= 2
  then Left $ ArgMismatch "expect (number, number)"
  else return $ SBool $ f (ns !! 0) (ns !! 1)

pEq = numCmpOp (==)
pGt = numCmpOp (>)
pLt = numCmpOp (<)
pGe = numCmpOp (>=)
pLe = numCmpOp (<=)

pStringAppend [SString a, SString b] = return $ SString $ a ++ b
pStringAppend _ = Left $ ArgMismatch "expect (string, string)"

pIsNull [SList []] = return $ SBool True
pIsNull [_] = return $ SBool False
pIsNull _ = Left $ ArgMismatch "expect (any)"
pCons [v, SList l] = return $ SList (v:l)
pCons _ = Left $ ArgMismatch "expect (any, list)"
pCar [SList (x:_)] = return x
pCar [SList []] = Left $ OtherError "car is applied to a null list"
pCar _ = Left $ ArgMismatch "expect (list)"
pCdr [SList (_:xs)] = return $ SList xs
pCdr [SList []] = Left $ OtherError "cdr is applied to a null list"
pCdr _ = Left $ ArgMismatch "expect (list)"

pDisplay [v] = liftIO $ (putStr (show v)) >> (return $ SBool True)
pDisplay _ = liftThrowEError $ Left $ ArgMismatch "expect (any)"
pRead [] = do
  s <- liftIO $ getLine
  case parse parseSExpr "<input>" s of
    Left e  -> liftThrowEError $ Left $ OtherError $ show e
    Right e -> return e
pRead _ = liftThrowEError $ Left $ ArgMismatch "expect ()"
pReadLine [] = liftIO $ (getLine >>= return . SString)
pReadLine _ = liftThrowEError $ Left $ ArgMismatch "expect ()"
pWriteLine [SString s] = liftIO $ (putStrLn s) >> (return $ SBool True)
pWriteLine _ = liftThrowEError $ Left $ ArgMismatch "expect (string)"
pNewline [] = liftIO $ (putChar '\n') >> (return $ SBool True)
pNewLine _ = liftThrowEError $ Left $ ArgMismatch "expect ()"

type PrimFunc = [SExpr] -> IOThrowEError SExpr
type PurePrimFunc = [SExpr] -> ThrowEError SExpr
purePrimitivesList :: [(String, PurePrimFunc)]
purePrimitivesList =
  [
    ("number?", pIsNumber),
    ("boolean?", pIsBoolean),
    ("string?", pIsString),
    ("list?", pIsList),
    ("+", pAdd),
    ("-", pSub),
    ("*", pMul),
    ("/", pDiv),
    ("mod", pMod),
    ("=", pEq),
    (">", pGt),
    ("<", pLt),
    (">=", pGe),
    ("<=", pLe),
    ("string-append", pStringAppend),
    ("null?", pIsNull),
    ("cons", pCons),
    ("car", pCar),
    ("cdr", pCdr)
  ]
liftPurePrimFunc :: PurePrimFunc -> PrimFunc
liftPurePrimFunc = (liftThrowEError .)
primitivesList :: [(String, PrimFunc)]
primitivesList =
  [
    ("display", pDisplay),
    ("read", pRead),
    ("read-line", pReadLine),
    ("write-line", pWriteLine),
    ("newline", pNewline)
  ] ++ pures
  where pures = map (\(n, f) -> (n, liftPurePrimFunc f)) purePrimitivesList
primitivesMap :: M.Map String SExpr
primitivesMap = M.map SPrim $ M.fromList primitivesList

isBound :: Env -> String -> IO Bool
isBound envRef n = do
  env <- readIORef envRef
  return $ M.member n env

getVar :: Env -> String -> IOThrowEError SExpr
getVar envRef n = do
  env <- liftIO $ readIORef envRef
  liftThrowEError $ case M.lookup n env of
    Nothing -> Left $ UnboundVar n
    Just v  -> return v

defVar :: Env -> String -> SExpr -> IOThrowEError SExpr
defVar envRef n v = do
  env <- liftIO $ readIORef envRef
  b <- liftIO $ isBound envRef n
  if b
  then liftThrowEError $ Left $ AlreadyDefined n
  else liftIO $ (return $ M.insert n v env) >>= writeIORef envRef >> return v

bindArgs :: Env -> [String] -> [SExpr] -> IOThrowEError Env
bindArgs envRef ns vs = do
  env <- liftIO $ readIORef envRef
  newEnv <- foldM setTo env (zip ns vs)
  liftIO $ newIORef newEnv
  where setTo m (k, v) = return $ M.alter (const $ Just v) k m

evalSFunc :: Env -> SExpr -> [SExpr] -> IOThrowEError SExpr
evalSFunc envRef (SFunc ns body fEnv) as
 | (length ns) /= (length as) = liftThrowEError $ Left $
     ArgMismatch $ "want " ++ (show $ length ns) ++ " arguments"
 | otherwise = do
     newEnvRef <- bindArgs fEnv ns as
     evalValue newEnvRef body

evalFunc :: Env -> SExpr -> [SExpr] -> IOThrowEError SExpr
evalFunc env f as = case f of
  SPrim p         -> p as
  f@(SFunc _ _ _) -> evalSFunc env f as
  v               -> liftThrowEError $ Left $
                       IllegalStruct $ (show v) ++ " is not a valid function"

evalValue :: Env -> SExpr -> IOThrowEError SExpr
evalValue env (SAtom s) = getVar env s
evalValue _ v@(SNumber _) = return v
evalValue _ v@(SString _) = return v
evalValue _ v@(SBool _) = return v
evalValue _ (SList [SAtom "quote", v]) = return v
evalValue env (SList [SAtom "def", SAtom n, v]) =
  evalValue env v >>= defVar env n >> return SNull
evalValue env (SList ((SAtom "def"):_)) =
  liftThrowEError $ Left $ IllegalStruct "definition"
evalValue env (SList [SAtom "if", cond, t, f]) =
  do r <- evalValue env cond
     case r of
       SBool False -> evalValue env f
       otherwise   -> evalValue env t
evalValue env (SList ((SAtom "if"):_)) =
  liftThrowEError $ Left $ IllegalStruct "if"
evalValue env (SList [SAtom "lambda", SList as@((SAtom _):_), body]) =
  return $ SFunc (map toString as) body env
  where toString (SAtom s) = s
evalValue env (SList ((SAtom "lambda"):_)) =
  liftThrowEError $ Left $ IllegalStruct "lambda"
evalValue env (SList ((SAtom "begin"):xs)) = do
  e <- liftIO $ readIORef env
  newEnv <- liftIO $ newIORef e
  if not $ and $ map isList xs
  then liftThrowEError $ Left $ IllegalStruct "begin"
  else mapM (evalValue newEnv) xs >>= return . last
  where isList (SList _) = True
        isList _ = False
evalValue env (SList xs) = do
  xs' <- mapM (evalValue env) xs
  evalFunc env (head xs') (tail xs')
evalValue _ _ = liftThrowEError $ Left $ IllegalStruct "illegal struct"

eval :: Env -> String -> IO (ThrowEError SExpr)
eval env src =
  case parse parseSExpr "schemesrc" src of
    Left e  -> return $ Left $ OtherError $ show e
    Right v -> runEitherT (evalValue env v)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env src = eval env src >>= putStrLn . either show show

doRepl :: Env -> IO ()
doRepl envRef = do
  line <- getLine
  if line == ":exit" || line == "\EOT"
  then return ()
  else evalAndPrint envRef line >> doRepl envRef

repl :: IO ()
repl = defaultEnv >>= doRepl

evalFile :: String -> IO ()
evalFile path = do
  src <- readFile path
  r <- defaultEnv >>= flip eval src
  case r of
    Left e    -> putStrLn $ show e
    otherwise -> return ()

main = do
  args <- getArgs
  case args of
    []    -> repl
    (x:_) -> evalFile x
