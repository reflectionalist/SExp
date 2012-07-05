module Read
  ( hReadSExp, readSExp )
where


import Data.Char
import System.IO


-- get a character without echoing it
hGetCh :: Handle -> IO Char
hGetCh hdl = do
  hSetEcho hdl False
  c <- hGetChar hdl
  hSetEcho hdl True
  return c

hReadSExp :: Handle -> Handle -> IO String
hReadSExp ihd ohd = hReadSE ihd ohd 0 0 ""
  where hReadSE ihd ohd cnt lvl str = do
          c <- hGetCh ihd
          case c of
            '\DEL' | str == "" -> hReadSE ihd ohd cnt lvl str
                   | otherwise -> do hPutChar ohd '\b'
                                     case head str of
                                       '('           -> hReadSE ihd ohd (cnt - 1) (lvl + 1) (tail str)
                                       ')'           -> hReadSE ihd ohd (cnt - 1) (lvl - 1) (tail str)
                                       c | isSpace c -> hReadSE ihd ohd cnt lvl (tail str)
                                         | otherwise -> hReadSE ihd ohd (cnt - 1) lvl (tail str)
            '\n' | cnt /= 0 && lvl == 0
                               -> do hPutChar ohd c
                                     return (reverse str)
            '('                -> do hPutChar ohd c
                                     hReadSE ihd ohd (cnt + 1) (lvl - 1) (c : str)
            ')'                -> do hPutChar ohd c
                                     hReadSE ihd ohd (cnt + 1) (lvl + 1) (c : str)
            _ | isSpace c      -> do hPutChar ohd c
                                     hReadSE ihd ohd cnt lvl 
                                          $ if str == "" || isSpace (head str)
                                               then str
                                               else c : str
              | otherwise      -> do hPutChar ohd c
                                     hReadSE ihd ohd (cnt + 1) lvl (c : str)

readSExp :: IO String
readSExp = hReadSExp stdin stdout

