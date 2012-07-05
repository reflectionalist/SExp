module SExp
  ( Name
  , SExp(..)
  , parseSExp, serializeSExp )
where


import Prelude
import Text.Parsec
import Text.Parsec.String
import Control.Monad
import Data.Char


type Name = String

data SExp
  = Atom Name
  | List [SExp]

parseSExp :: Parser SExp
parseSExp = parseAtom
       <|> do char '(' >> spaces
              e <- parseList
              spaces >> char ')'
              return e

serializeSExp :: SExp -> String
serializeSExp = show

instance Show SExp where
  show (Atom s)  = s
  show (List []) = "()"
  show (List es) = "(" ++ showList es ++ ")"
    where showList [e]      = show e
          showList (e : es) = show e ++ " " ++ showList es

parseAtom :: Parser SExp
parseAtom = liftM Atom . many1 $ atomChar

parseList :: Parser SExp
parseList = do
  es <- sepEndBy parseSExp spaces1
  return $ List es

atomChar :: Parser Char
atomChar = satisfy $ \c -> not (isSpace c || elem c "()")

spaces1 :: Parser ()
spaces1 = skipMany1 space

