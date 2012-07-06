module SExp
  ( Name
  , SExp(..)
  , parseSExp, serializeSExp
  , hReadSExp, readSExp )
where


import Prelude
import Text.Parsec
import Text.Parsec.String
import Control.Monad
import Data.Char
import System.IO


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

serializeSExp :: SExp -> String
serializeSExp = show

instance Show SExp where
  show (Atom s)  = s
  show (List []) = "()"
  show (List es) = "(" ++ showList es ++ ")"
    where showList [e]      = show e
          showList (e : es) = show e ++ " " ++ showList es


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

