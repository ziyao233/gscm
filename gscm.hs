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
import Data.List (foldl1')

data SExpr = SAtom String
           | SList [SExpr]
           | SNumber Int
           | SString String
           | SBool Bool
           | SPrim PrimFunc

instance Show SExpr where
  show (SAtom xs)     = xs
  show (SList xs)     = "(" ++ (unwords $ map show xs) ++ ")"
  show (SNumber x)    = show x
  show (SString xs)   = "\"" ++ xs ++ "\""
  show (SBool True)   = "#t"
  show (SBool False)  = "#f"
  show (SPrim _)      = "<primitive>"

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
  list <- sepBy parseSExpr spaces
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
               | UnknownError
               | UnboundVar String
               | AlreadyDefined String
instance Show EvalError where
  show (ArgMismatch m) = "Argument(s) mismatch: " ++ m
  show (IllegalStruct m) = "Illegal Struct: " ++ m
  show (UnboundVar m) = "Unbound variable: " ++ m
  show (AlreadyDefined m) = "Variable already defined: " ++ m
  show (UnknownError) = "Unknown error"

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

type IOEither = EitherT EvalError IO
liftEither :: Either EvalError a -> IOEither a
liftEither = either (EitherT . return . Left) (return)

type Env = IORef (M.Map String SExpr)
defaultEnv :: IO Env
defaultEnv = newIORef primitivesMap

extractNum :: SExpr -> Either EvalError Int
extractNum (SNumber n) = return n
extractNum _ = Left $ ArgMismatch "expect number"
numBinOp :: (Int -> Int -> Int) -> [SExpr] -> Either EvalError SExpr
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
pStringAppend [SString a, SString b] = return $ SString $ a ++ b
pStringAppend _ = Left $ ArgMismatch "expect (string, string)"

type PrimFunc = [SExpr] -> Either EvalError SExpr
primitivesList :: [(String, PrimFunc)]
primitivesList =
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
    ("string-append", pStringAppend)
  ]
primitivesMap :: M.Map String SExpr
primitivesMap = M.map SPrim $ M.fromList primitivesList

isBound :: Env -> String -> IO Bool
isBound envRef n = do
  env <- readIORef envRef
  return $ M.member n env

getVar :: Env -> String -> IOEither SExpr
getVar envRef n = do
  env <- liftIO $ readIORef envRef
  liftEither $ case M.lookup n env of
    Nothing -> Left $ UnboundVar n
    Just v  -> return v

defVar :: Env -> String -> SExpr -> IOEither SExpr
defVar envRef n v = do
  env <- liftIO $ readIORef envRef
  b <- liftIO $ isBound envRef n
  if b
  then liftEither $ Left $ AlreadyDefined n
  else liftIO $ (return $ M.insert n v env) >>= writeIORef envRef >> return v

evalFunc :: Env -> String -> [SExpr] -> IOEither SExpr
evalFunc env f as = do
  as' <- mapM (evalValue env) as
  v <- getVar env f
  liftEither $ case v of
    SPrim p -> p as'
    _       -> Left $ IllegalStruct $ f ++ " is not a valid function"

evalValue :: Env -> SExpr -> IOEither SExpr
evalValue env (SAtom s) = getVar env s
evalValue _ v@(SNumber _) = return v
evalValue _ v@(SString _) = return v
evalValue _ v@(SBool _) = return v
evalValue _ (SList [SAtom "quote", v]) = return v
evalValue env (SList [SAtom "def", SAtom n, v]) =
  evalValue env v >>= defVar env n
evalValue env (SList ((SAtom "def"):_)) =
  liftEither $ Left $ IllegalStruct "definition"
evalValue env (SList ((SAtom f):as)) = evalFunc env f as
evalValue env (SList ((SPrim p):as)) = mapM (evalValue env) as >>= liftEither . p
evalValue _ _ = liftEither $ Left $ IllegalStruct "illegal struct"

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env src =
  case parse parseSExpr "schemesrc" src of
    Left e  -> putStrLn $ show e
    Right v -> runEitherT (evalValue env v) >>= return . either show show >>= putStrLn

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
  defaultEnv >>= flip evalAndPrint src

main = do
  args <- getArgs
  case args of
    []    -> repl
    (x:_) -> evalFile x
