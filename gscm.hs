module Main where

import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO (readFile)
import System.Environment (getArgs)
import Data.Map (Map, fromList, (!))

data SExpr = SAtom String
           | SList [SExpr]
           | SNumber Int
           | SString String
           | SBool Bool

instance Show SExpr where
  show (SAtom xs)     = xs
  show (SList xs)     = "(" ++ (unwords $ map show xs) ++ ")"
  show (SNumber x)    = show x
  show (SString xs)   = "\"" ++ xs ++ "\""
  show (SBool True)   = "#t"
  show (SBool False)  = "#f"

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

pIsNumber [SNumber _] = SBool True
pIsNumber _ = SBool False
pIsBoolean [SBool _] = SBool True
pIsBoolean _ = SBool False
pIsString [SString _] = SBool True
pIsString _ = SBool False
pIsList [SList _] = SBool True
pIsList _ = SBool False

primitivesList :: [(String, [SExpr] -> SExpr)]
primitivesList =
  [
    ("number?", pIsNumber),
    ("boolean?", pIsBoolean),
    ("string?", pIsString),
    ("list?", pIsList)
  ]
primitivesMap :: Map String ([SExpr] -> SExpr)
primitivesMap = fromList primitivesList

evalFunc :: String -> [SExpr] -> SExpr
evalFunc f as =
  (primitivesMap ! f) as'
  where as' = map evalValue as

evalValue :: SExpr -> SExpr
evalValue v@(SAtom _) = v
evalValue v@(SNumber _) = v
evalValue v@(SString _) = v
evalValue v@(SBool _) = v
evalValue (SList [SAtom "quote", v]) = v
evalValue (SList ((SAtom f):as)) = evalFunc f as

eval :: String -> IO ()
eval src =
  case parse parseSExpr "schemesrc" src of
    Left e  -> putStrLn $ show e
    Right v -> putStrLn $ show $ evalValue v

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
