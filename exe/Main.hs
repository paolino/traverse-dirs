module Main where

import FileSystem.Traverse
    ( countFiles
    , printOnlyExceptions
    , printOnlyFiles
    )
import System.Environment (getArgs)

main :: IO ()
main = do
    roots <- getArgs
    printOnlyFiles roots
    printOnlyExceptions roots
    countFiles roots >>= print
