{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}

module FileSystem.Traverse where

import Control.Exception (IOException, handle)
import Control.Pipe
    ( Pipe (..)
    , filterMaybePipe
    , foldSink
    , mapPipe
    , putStrLnSink
    , runPipe
    , scanInputPipe
    )
import Data.List (sort)
import Data.Set qualified as Set
import System.Directory (canonicalizePath, doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import Prelude

-- | Differenciate between a path that succeeded in being used in IO and a path
-- that failed with an IOException.
data Tried
    = Failed FilePath IOException
    | Succeeded FilePath
    deriving (Show, Eq)

-- | Differenciate between a file and a directory that was tried in IO.
data TriedAny = TriedFile Tried | TriedDir Tried
    deriving (Show, Eq)

-- | Extract the path and the exception from a 'TriedAny' element.
triedFailed :: TriedAny -> Maybe (FilePath, IOException)
triedFailed (TriedFile (Failed fp e)) = Just (fp, e)
triedFailed (TriedDir (Failed fp e)) = Just (fp, e)
triedFailed _ = Nothing

-- | Extract the path from a 'TriedAny' element.
triedSuccess :: TriedAny -> Maybe FilePath
triedSuccess (TriedFile (Succeeded fp)) = Just fp
triedSuccess (TriedDir (Succeeded fp)) = Just fp
triedSuccess _ = Nothing

-- | Try to run an IO computation that compute the resto of the pipe.
-- If an IOException is thrown, the exception is caught and the path is
-- returned as a 'TriedPath' with the exception.
-- The rest of the pipe argument is only used in case of exception as the
-- continuation of the 'TriedPath' element.
handleIOException
    :: FilePath
    -- ^ Path that is being tried
    -> (FilePath -> IOException -> e)
    -- ^ Function to build the exception in case of IOException
    -> Pipe IO i e
    -- ^ rest of the pipe in case of exception
    -> IO (Pipe IO i e)
    -- ^ Action that returns the pipe or throws an exception
    -> IO (Pipe IO i e)
handleIOException x failed s = handle $ \e -> pure $ Element (failed x e) s

data Input = NewDir FilePath | NextFile

-- | Output the content of the directories in the list of roots + the content
-- of the rest of the directories received as input.
-- The output is a 'TriedPath' element with the exception if an IOException
-- is thrown when listing the directory.
-- Paths listed are always pushed in the output with no exceptions
-- Expect a lexicografical scan of the directories where "/a/b" is listed before
-- "/b".
listDir
    :: [FilePath]
    -- ^ Roots of the directories to be scanneed
    -> Pipe IO Input Tried
listDir = go . sort
  where
    go rest = Feedback $ \case
        NewDir path -> Effect
            $ handleIOException path Failed (go rest)
            $ do
                files <- sort <$> listDirectory path
                pure $ go $ fmap (path </>) files <> rest
        NextFile -> case rest of
            [] -> End
            (p : ps) -> Element (Succeeded p) (go ps)

-- | A pipe that feeds back all the directories found in the output of the argument.
-- If the path produce an IOException, the path is returned as a failed 'TriedFile'
-- If the output is a failure it's passed over as a failed 'TriedDir' element.
-- If the output is a success, it's passed over as a success 'TriedFile' element.
sourceDirs
    :: Pipe IO Input Tried
    -> Pipe IO i TriedAny
sourceDirs = no
  where
    go
        :: Input
        -> Pipe IO Input Tried
        -> Pipe IO i TriedAny
    go l = \case
        End -> End
        Effect m -> Effect $ no <$> m
        Element (Succeeded path) s ->
            Effect
                $ handling path s
                $ do
                    cPath <- canonicalizePath path
                    isDir <- doesDirectoryExist cPath
                    pure
                        $ if isDir
                            then go (NewDir cPath) s
                            else
                                Element (TriedFile $ Succeeded cPath)
                                    $ no s
        Element d s -> Element (TriedDir d) $ no s
        Feedback f -> go l $ f l

    handling path s =
        handleIOException path (\fp e -> TriedFile $ Failed fp e) $ no s
    no = go NextFile

-- | A pipe that filters out the duplicates in the input.
uniqueInput :: Pipe IO Input o -> Pipe IO Input o
uniqueInput = scanInputPipe f mempty
  where
    f s (NewDir x) =
        if Set.member x s
            then (s, NextFile)
            else (Set.insert x s, NewDir x)
    f s NextFile = (s, NextFile)

traverseDirs :: [FilePath] -> Pipe IO i TriedAny
traverseDirs roots =
    sourceDirs
        $ uniqueInput
        $ listDir roots

printOnlyExceptions :: [FilePath] -> IO ()
printOnlyExceptions roots =
    putStrLnSink
        $ mapPipe show
        $ filterMaybePipe triedFailed
        $ traverseDirs roots

printOnlyFiles :: [FilePath] -> IO ()
printOnlyFiles roots =
    runPipe putStrLn (pure NextFile)
        $ filterMaybePipe triedSuccess
        $ traverseDirs roots

countFiles :: [FilePath] -> IO Int
countFiles roots =
    foldSink (\c _ -> succ c) 0
        $ filterMaybePipe triedSuccess
        $ traverseDirs roots
