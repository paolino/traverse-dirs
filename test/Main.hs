module Main (main) where

import Control.Pipe
    ( breakPipe
    , feedbackPipe
    , filterInput
    , filterPipe
    , fromListSource
    , monoidSink
    , reinforcePipe
    , takePipe
    , takeUntilPipe
    , toListSink
    , uniqueOutPipe
    )
import Data.Functor.Identity (runIdentity)
import Data.Monoid (Sum (..))
import FileSystem.Traverse (Tried (..), TriedAny (..), traverseDirs)
import System.Directory (createDirectory, createDirectoryLink)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (describe, hspec, it)
import Test.Hspec.Expectations (Expectation, shouldBe)

main :: IO ()
main = hspec $ do
    describe "pipes" $ do
        it "monoidSink" $ test_monoidSink
        it "filterPipe" $ test_filterPipe
        it "uniqueOutPipe" $ test_uniqueOutPipe
        it "takePipe" $ test_takePipe
        it "takeUntilPipe" $ test_takeUntilPipe
        it "breakPipe" $ test_breakPipe
        it "feedbackPipe" $ test_feedbackPipe
        it "filterInput" $ test_filterInput
    describe "traverseDirs" $ do
        it "traverseDirs" $ do
            withSystemTempDirectory "traverse-dir" $ \dir -> do
                putStrLn $ "Temp dir: " ++ dir
                touch $ dir </> "0"
                touch $ dir </> "2"
                let sub = dir </> "1"
                newDir sub
                touch $ sub </> "5"
                touch $ sub </> "4"
                touch $ sub </> "3"
                link dir $ sub </> "parent" -- loop test
                files <- toListSink $ traverseDirs [dir]
                files
                    `shouldBe` [ success $ dir </> "0"
                               , success $ sub </> "3"
                               , success $ sub </> "4"
                               , success $ sub </> "5"
                               , success $ dir </> "2"
                               ]

success :: FilePath -> TriedAny
success = TriedFile . Succeeded

touch :: FilePath -> IO ()
touch path = writeFile path ""

newDir :: FilePath -> IO ()
newDir path = createDirectory path

link :: FilePath -> FilePath -> IO ()
link = createDirectoryLink

test_monoidSink :: Expectation
test_monoidSink =
    let
        p = runIdentity . monoidSink . fromListSource
     in
        p (Sum <$> [1, 2, 3, 4, 5]) `shouldBe` Sum @Int 15

test_filterPipe :: Expectation
test_filterPipe =
    let
        p =
            runIdentity
                . toListSink
                . filterPipe (not . (==) "Tom")
                . fromListSource
     in
        p ["Tom", "Jerry", "Tom", "Tom", "Jerry"] `shouldBe` ["Jerry", "Jerry"]

test_uniqueOutPipe :: Expectation
test_uniqueOutPipe =
    let
        p = runIdentity . toListSink . uniqueOutPipe . fromListSource
     in
        p [1, 2, 3, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9]
            `shouldBe` [1 :: Int, 2, 3, 4, 5, 6, 7, 8, 9]

test_takePipe :: Expectation
test_takePipe =
    let
        p = runIdentity . toListSink . takePipe 5 . fromListSource
     in
        p [1, 2, 3, 4, 5, 6, 7, 8, 9] `shouldBe` [1 :: Int, 2, 3, 4, 5]

test_takeUntilPipe :: Expectation
test_takeUntilPipe =
    let
        p = runIdentity . toListSink . takeUntilPipe (< 5) . fromListSource
     in
        p [1, 2, 3, 4, 5, 6, 7, 8, 9] `shouldBe` [1 :: Int, 2, 3, 4]

test_breakPipe :: Expectation
test_breakPipe =
    let
        p = runIdentity . toListSink . breakPipe (== 5) . fromListSource
     in
        p [1, 2, 3, 4, 5, 6, 7, 8, 9] `shouldBe` [1 :: Int, 2, 3, 4, 5]

test_feedbackPipe :: Expectation
test_feedbackPipe =
    let
        p =
            runIdentity
                . toListSink
                . feedbackPipe
                . reinforcePipe (+)
                . fromListSource
     in
        p [1, 2, 3, 4, 5] `shouldBe` [1 :: Int, 3, 6, 10, 15]

test_filterInput :: Expectation
test_filterInput =
    let
        p =
            runIdentity
                . toListSink
                . feedbackPipe
                . filterInput even
                . reinforcePipe (+)
                . fromListSource
     in
        p [1, 2, 3, 4, 5] `shouldBe` [1 :: Int, 2, 5, 4, 9]
