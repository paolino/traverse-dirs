{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
module Dirs where

import Prelude
import System.Directory
import System.FilePath

import Control.Exception (IOException , handle)
import qualified Data.Set as Set

data S m a = E | M (m (S m a )) | N a (S m a )

unfoldS :: Monad m => c -> (c -> m (Maybe (a, c))) -> S m a
unfoldS c f = M $ f c >>= \case
  Nothing -> pure E
  Just (a, c') -> pure $ N a (unfoldS c' f)

foldMS :: Monad m => (c -> a -> m c) -> c -> S m a -> m c
foldMS _ c E = pure c
foldMS g !c (M m) = m >>= foldMS g c
foldMS g !c (N a k) = g c a >>= \c' -> foldMS g c' k

printS :: Show a => S IO a -> IO ()
printS = foldMS (\_ x -> print x) ()

generateFiles :: FilePath -> S IO FilePath
generateFiles path = unfoldS ([path], mempty) go
 where
    go ([],_ ) = pure Nothing
    go ((p:ps), knowns)
        | p `Set.member` knowns = go (ps, knowns)
        | otherwise = handle (\(_ :: IOException) -> go (ps, knowns)) $ do
            isDir <- doesDirectoryExist p
            if isDir
                then do
                    files <- fmap (p </>) <$> listDirectory p
                    go $ (files <> ps, Set.insert p knowns)
                else
                    pure $ Just (p, (ps, Set.insert p knowns))

countFiles :: FilePath -> IO Int
countFiles = foldMS (\c _ -> pure $ c + 1) 0 . generateFiles

test :: FilePath -> IO ()
test = printS . generateFiles