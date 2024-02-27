import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)

data SExpr = SAtom String
           | SList [SExpr]
           | SDottedList [SExpr] SExpr
           | SNumber Int
           | SString String
           | SBool Bool

instance Show SExpr where
  show (SAtom xs)     = xs
  show (SList xs)     = "(" ++ (unwords $ map show xs) ++ ")"
  show (SDottedList xs x)
                      = "(" ++ (unwords $ map show xs) ++ " . " ++
                        (show x) ++ ")"
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

parseDottedList :: Parser SExpr
parseDottedList = do
  char '('
  list <- endBy parseSExpr spaces
  last <- char '.' >> spaces >> parseSExpr
  char ')'
  return $ SDottedList list last

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
          <|> (try parseList <|> parseDottedList)
