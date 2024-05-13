module Main where

import qualified Control.Bandwidth as Bandwidth
import FileSystem.Traverse
    ( countFiles
    , printOnlyExceptions
    , printOnlyFiles
    )
import System.Environment (getArgs)

main1 :: IO ()
main1 = do
    roots <- getArgs
    printOnlyFiles roots
    printOnlyExceptions roots
    countFiles roots >>= print

main :: IO ()
main = Bandwidth.test
