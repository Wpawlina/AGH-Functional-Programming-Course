--operacje na plikach

-- import System.Environment
-- import System.IO

-- main = do
--   (inFileName:outFileName:_) <- getArgs
--   inHdlr <- openFile inFileName ReadMode
--   outHdlr <- openFile outFileName WriteMode
--   inpStr <- hGetContents inHdlr
--   hPutStr outHdlr inpStr
--   hClose inHdlr
--   hClose outHdlr


-- import System.Environment

-- main = do
--   (inFileName:outFileName:_) <- getArgs
--   inpStr <- readFile inFileName
--   writeFile outFileName inpStr


import System.Environment
import qualified Data.ByteString as BStr

-- main = do
--   (inFileName:outFileName:_) <- getArgs
--   inpBStr <- BStr.readFile inFileName
--   BStr.writeFile outFileName inpBStr


import System.IO

main :: IO ()
main = do
    (fileName:_) <- getArgs
    handle <- openFile fileName ReadMode 
    count <- readLoop 0 handle
    putStrLn $ "Number of lines: " ++ show count
    hClose handle                         -- Zamknij plik

readLoop ::  Int -> Handle -> IO Int
readLoop c handle = do
    eof <- hIsEOF handle                  -- Sprawdź, czy to koniec pliku
    if eof
        then return c
        else do
            line <- hGetLine handle
            putStrLn line
            readLoop (c+1) handle