module SExp
  ( Name
  , SExp(..)
  , parseSExp, parseAtom, parseList
  , serializeSExp
  , hReadSExp, readSExp )
where


import Text.Parsec
import Text.Parsec.String
import Data.Char
import System.IO


type Name = String

data SExp
  = Atom Name
  | List [SExp]

instance Show SExp where
  show = serializeSExp


parseSExp :: Parser SExp
parseSExp = parseAtom
        <|> parseList

parseAtom :: Parser SExp
parseAtom = do
  name <- many1 atomChar
  return $ Atom name

parseList :: Parser SExp
parseList = do
  char '(' >> spaces
  sexps <- sepEndBy parseSExp spaces1
  spaces >> char ')'
  return $ List sexps

atomChar :: Parser Char
atomChar = satisfy $ \c -> not (isSpace c || elem c "()")

spaces1 :: Parser ()
spaces1 = skipMany1 space

serializeSExp :: SExp -> String
serializeSExp (Atom name)  = name
serializeSExp (List [])    = "()"
serializeSExp (List sexps) = "(" ++ serializeSExps sexps ++ ")"
  where serializeSExps [sexp]         = serializeSExp sexp
        serializeSExps (sexp : sexps) = serializeSExp sexp ++ " " ++ serializeSExps sexps


hReadSExp :: Handle -> Handle -> IO String
hReadSExp ihd ohd = hReadSE ihd ohd 0 0 ""
  where hReadSE ihd ohd cnt lvl str = do
          c <- hGetChar ihd
          case c of
            '\DEL' | str == "" -> hReadSE ihd ohd cnt lvl str
                   | otherwise -> do hPutStr ohd "\b \b"
                                     case head str of
                                       '('           -> hReadSE ihd ohd (cnt - 1) (lvl + 1) (tail str)
                                       ')'           -> hReadSE ihd ohd (cnt - 1) (lvl - 1) (tail str)
                                       c | isSpace c -> hReadSE ihd ohd cnt lvl (tail str)
                                         | otherwise -> hReadSE ihd ohd (cnt - 1) lvl (tail str)
            '\n' | cnt /= 0 && lvl == 0
                               -> return (reverse str)
            '('                -> hReadSE ihd ohd (cnt + 1) (lvl - 1) (c : str)
            ')'                -> hReadSE ihd ohd (cnt + 1) (lvl + 1) (c : str)
            _ | isSpace c      -> hReadSE ihd ohd cnt lvl
                                        $ if str == "" || isSpace (head str)
                                             then str
                                             else c : str
              | otherwise      -> hReadSE ihd ohd (cnt + 1) lvl (c : str)

readSExp :: IO String
readSExp = hReadSExp stdin stdout

