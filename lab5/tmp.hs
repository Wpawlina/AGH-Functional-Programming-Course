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

main = do
  (inFileName:outFileName:_) <- getArgs
  inpBStr <- BStr.readFile inFileName
  BStr.writeFile outFileName inpBStr