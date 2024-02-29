module Main where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO (readFile)
import System.Environment (getArgs)
import Data.Either
import Data.Map (Map, fromList, lookup)
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
               | UnbindedVar String
instance Show EvalError where
  show (ArgMismatch m) = "Argument(s) mismatch: " ++ m
  show (IllegalStruct m) = "Illegal Struct: " ++ m
  show (UnbindedVar m) = "Unbinded variable: " ++ m
  show (UnknownError) = "Unknown error"

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
primitivesMap :: Map String ([SExpr] -> Either EvalError SExpr)
primitivesMap = fromList primitivesList

getPrim :: String -> Maybe PrimFunc
getPrim f = Data.Map.lookup f primitivesMap

evalFunc :: String -> [SExpr] -> Either EvalError SExpr
evalFunc f as = do
  as' <- mapM evalValue as
  case getPrim f of
    Nothing -> Left $ UnbindedVar f
    Just p  -> p as'

evalValue :: SExpr -> Either EvalError SExpr
evalValue (SAtom s) = case getPrim s of
                        Nothing -> Left $ UnbindedVar s
                        Just p  -> return $ SPrim p
evalValue v@(SNumber _) = return v
evalValue v@(SString _) = return v
evalValue v@(SBool _) = return v
evalValue (SList [SAtom "quote", v]) = return v
evalValue (SList ((SAtom f):as)) = evalFunc f as
evalValue (SList ((SPrim p):as)) = mapM evalValue as >>= p
evalValue _ = Left $ IllegalStruct "illegal struct"

eval :: String -> IO ()
eval src =
  case parse parseSExpr "schemesrc" src of
    Left e  -> putStrLn $ show e
    Right v -> putStrLn $ either show show $ evalValue v

repl :: IO ()
repl = do
  line <- getLine
  if line == ":exit" || line == "\EOT"
  then return ()
  else (eval line) >> repl

evalFile :: String -> IO ()
evalFile path = (readFile path) >>= eval

main = do
  args <- getArgs
  case args of
    []    -> repl
    (x:_) -> evalFile x
