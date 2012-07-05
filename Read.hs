module Read
where


import Data.Char
import System.IO


hGetCh :: Handle -> IO Char
hGetCh hdl = do
  hSetEcho hdl False
  c <- hGetChar hdl
  hSetEcho hdl True
  return c

readSExp :: Handle -> IO String
readSExp hdl = readSE hdl True 0 ""
  where readSE hdl out lvl str = do
          if not (str == "") && (out && lvl == 0)
             then return (reverse str)
             else do c <- hGetCh hdl
                     if isSpace c
                        then do putChar c
                                readSE hdl (if out then out else True) lvl (c : str)
                        else case c of
                               '\DEL' -> do putStr "\b\b"
                                            readSE
                               '(' -> readSE hdl True (lvl - 1) (c : str)
                               ')' -> readSE hdl True (lvl + 1) (c : str)
                               _   -> readSE hdl False lvl (c : str)

readAtom :: Handle -> IO String
readAtom hdl = readAt hdl ""
  where readAt hdl str = do
          c <- hLookAhead hdl
          if isSpace c || c `elem` "()"
             then return (reverse str)
             else readAt hdl (c : str)


getCh :: IO Char
getCh =  do hSetEcho stdin False
            c <- getChar
            hSetEcho stdin True
            return c

readLine :: IO String
readLine =  f ""
            where
              f xs =  do c <- getCh
                         handle xs c
              handle [] '\DEL' = f ""
              handle xs '\DEL' = do putChar '\b'
                                    f (init xs)
              handle xs c      = do putChar c
                                    f (xs ++ [c])

