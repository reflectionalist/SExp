module SExp
  ( Name
  , SExp(..)
  , readSExp )
where


import Prelude hiding (readList)
import Text.Parsec
import Text.Parsec.String
import Control.Monad
import Data.Char


type Name = String

data SExp
  = Atom Name
  | List [SExp]

readSExp :: Parser SExp
readSExp = readAtom
       <|> do char '(' >> spaces
              e <- readList
              spaces >> char ')'
              return e

instance Show SExp where
  show (Atom s)  = s
  show (List []) = "()"
  show (List es) = "(" ++ showList es ++ ")"
    where showList [e]      = show e
          showList (e : es) = show e ++ " " ++ showList es

readAtom :: Parser SExp
readAtom = liftM Atom . many1 $ atomChar

readList :: Parser SExp
readList = do
  es <- sepEndBy readSExp spaces1
  return $ List es

atomChar :: Parser Char
atomChar = satisfy $ \c -> not (isSpace c || elem c "()")

spaces1 :: Parser ()
spaces1 = skipMany1 space

