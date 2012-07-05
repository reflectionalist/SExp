module Read
  ( readSExp )
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

readSExp :: Handle -> IO String
readSExp hdl = readSE hdl 0 0 ""
  where readSE hdl cnt lvl str = do
          c <- hGetCh hdl
          case c of
            '\DEL' | str == "" -> readSE hdl cnt lvl str
                   | otherwise -> do putChar '\b'
                                     case head str of
                                       '('           -> readSE hdl (cnt - 1) (lvl + 1) (tail str)
                                       ')'           -> readSE hdl (cnt - 1) (lvl - 1) (tail str)
                                       c | isSpace c -> readSE hdl cnt lvl (tail str)
                                         | otherwise -> readSE hdl (cnt - 1) lvl (tail str)
            '\n' | cnt /= 0 && lvl == 0
                               -> do putChar c
                                     return (reverse str)
            '('                -> do putChar c
                                     readSE hdl (cnt + 1) (lvl - 1) (c : str)
            ')'                -> do putChar c
                                     readSE hdl (cnt + 1) (lvl + 1) (c : str)
            _ | isSpace c      -> do putChar c
                                     readSE hdl cnt lvl 
                                          $ if str == "" || isSpace (head str)
                                               then str
                                               else c : str
              | otherwise      -> do putChar c
                                     readSE hdl (cnt + 1) lvl (c : str)

