{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Control.Pipe where

import Control.Monad.Fix (fix)
import Data.Functor.Identity (Identity (..))
import Data.Monoid (Sum (..))
import Data.Set qualified as Set
import Prelude

default (Int)

-- | A datatype to generate a sequence of values of type @o@
-- , possibly interleaved with effects in @m@.
-- The continuations are additionally feeded with values of type @i@.
data Output m i o = End | Effect (m (Pipe m i o)) | Element o (Pipe m i o)

-- | A datatype to represent the input values of type @i@. Because we expect to
-- produce outputs even when no inputs are available, we allow for empty input.
data Input i = Input i | NoInput

-- | A function that feeds the input to the continuation.
withInput :: Input i -> (i -> a) -> a -> a
withInput (Input i) f _ = f i
withInput NoInput _ x = x

-- | A datatype that control the generation of values of type @o@
-- with an input of type @i@. To get the next output value, it's necessary to
-- provide an input value of type @i@ or we can just send an empty signal with
-- @NoInput@.
data Pipe m i o = Pipe (Input i -> Output m i o)

-- | An infinite source of values of type @o@.
repeatSource :: o -> Pipe m i o
repeatSource x = Pipe $ \_ -> Element x $ repeatSource x

-- | An infinite source of values of type @o@ from a seed value.
unfoldSource :: (i -> (o, i)) -> i -> Pipe m i o
unfoldSource f x = Pipe $ \_ -> case f x of
    (o, i) -> Element o $ unfoldSource f i

-- | A finite source of values of type @o@.
fromListSource :: [o] -> Pipe m i o
fromListSource [] = Pipe $ \_ -> End
fromListSource (x : xs) = Pipe $ \_ -> Element x $ fromListSource xs

-- | A finite source of values of type @o@ from a seed value.
unfoldFiniteSource :: (i -> Maybe (o, i)) -> i -> Pipe m i o
unfoldFiniteSource f x = Pipe $ \_ -> case f x of
    Nothing -> End
    Just (o, i) -> Element o $ unfoldFiniteSource f i

-- | A source that outputs line from stdin
stdinSource :: Pipe IO i String
stdinSource = Pipe $ \_ -> Effect $ do
    line <- getLine
    pure $ Pipe $ \_ -> Element line stdinSource

-- | A sink that outputs line to stdout
putStrLnSink :: Pipe IO i String -> IO ()
putStrLnSink = foldMSink (const putStrLn) ()

-- | Not Tom
cat :: IO ()
cat = putStrLnSink stdinSource

-- | A sink that collects the values of type @o@ into a list.
toListSink :: Monad m => Pipe m i o -> m [o]
toListSink (Pipe f) = case f NoInput of
    End -> pure []
    Effect m -> m >>= toListSink
    Element x s -> (x :) <$> toListSink s

-- | A sink that burns the values of type @o@ into the effects. Generalize putStrLnSink.
sink :: Monad m => (o -> m ()) -> Pipe m i o -> m ()
sink f (Pipe g) = case g NoInput of
    End -> pure ()
    Effect m -> m >>= sink f
    Element x s -> f x >> sink f s

-- | A sink that folds the values of type @o@ into a single value of type @c@.
foldSink :: Monad m => (c -> o -> c) -> c -> Pipe m i o -> m c
foldSink g !c (Pipe f) = case f NoInput of
    End -> pure c
    Effect m -> m >>= foldSink g c
    Element x s -> foldSink g (g c x) s

-- | A sink that folds the values of type @o@ with its monoid
monoidSink :: (Monad m, Monoid c) => Pipe m i c -> m c
monoidSink = foldSink mappend mempty

test_monoidSink :: Bool
test_monoidSink =
    let
        p = runIdentity . monoidSink . fromListSource
    in
        p (Sum <$> [1, 2, 3, 4, 5]) == Sum @Int 15

-- | A sink that folds the values of type @o@ into a single value of type @c@
-- with a folding monadic function.
foldMSink :: Monad m => (c -> o -> m c) -> c -> Pipe m i o -> m c
foldMSink g !c (Pipe f) = case f NoInput of
    End -> pure c
    Effect m -> m >>= foldMSink g c
    Element x s -> g c x >>= \c' -> foldMSink g c' s

-- | A pipe element that changes the output type
mapPipe :: Monad m => (o -> o') -> Pipe m i o -> Pipe m i o'
mapPipe f (Pipe g) = Pipe $ \r -> case g r of
    End -> End
    Effect m -> Effect $ mapPipe f <$> m
    Element x s -> Element (f x) $ mapPipe f s

-- | A pipe element that filters out the values of type @o@ that maps to @Nothing@
-- and keeps the values of type @o'@ that appear in @Just@.
filterMaybePipe :: Monad m => (o -> Maybe o') -> Pipe m r o -> Pipe m r o'
filterMaybePipe p (Pipe f) = Pipe $ \r -> case f r of
    End -> End
    Effect m -> Effect $ filterMaybePipe p <$> m
    Element x s ->
        case p x of
            Just y -> Element y $ filterMaybePipe p s
            Nothing -> Effect . pure $ filterMaybePipe p s

-- | A pipe element that filters in the values of type @o@ that satisfy a predicate.
filterPipe :: Monad m => (o -> Bool) -> Pipe m r o -> Pipe m r o
filterPipe p = filterMaybePipe $ \x -> if p x then Just x else Nothing

-- catNoTom :: IO ()
-- catNoTom = sink putStrLn $ filterPipe (_ . (==) "Tom") $ stdinSource

test_filterPipe :: Bool
test_filterPipe =
    let
        p =
            runIdentity
                . toListSink
                . filterPipe (not . (==) "Tom")
                . fromListSource
    in
        p ["Tom", "Jerry", "Tom", "Tom", "Jerry"] == ["Jerry", "Jerry"]

-- | A pipe element that filter out duplicate output
uniqueOutPipe :: (Monad m, Ord o) => Pipe m r o -> Pipe m r o
uniqueOutPipe = go mempty
  where
    go ks (Pipe f) = Pipe $ \r -> case f r of
        End -> End
        Effect m -> Effect $ go ks <$> m
        Element x s ->
            if Set.member x ks
                then Effect . pure $ go ks s
                else Element x $ go (Set.insert x ks) s

test_uniqueOutPipe :: Bool
test_uniqueOutPipe =
    let
        p = runIdentity . toListSink . uniqueOutPipe . fromListSource
    in
        p [1, 2, 3, 1, 2, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9]
            == [1 :: Int, 2, 3, 4, 5, 6, 7, 8, 9]

-- | A pipe element that cuts off the output after a certain number of elements
takePipe :: Monad m => Int -> Pipe m r o -> Pipe m r o
takePipe n (Pipe f) = Pipe $ \r -> case f r of
    End -> End
    Effect m -> Effect $ takePipe n <$> m
    Element x s ->
        if n <= 0
            then End
            else Element x $ takePipe (n - 1) s

test_takePipe :: Bool
test_takePipe =
    let
        p = runIdentity . toListSink . takePipe 5 . fromListSource
    in
        p [1, 2, 3, 4, 5, 6, 7, 8, 9] == [1 :: Int, 2, 3, 4, 5]

-- | A pipe element that take only the first elements that satisfy a predicate
takeUntilPipe :: Monad m => (o -> Bool) -> Pipe m r o -> Pipe m r o
takeUntilPipe p (Pipe f) = Pipe $ \r -> case f r of
    End -> End
    Effect m -> Effect $ takeUntilPipe p <$> m
    Element x s ->
        if p x
            then Element x $ takeUntilPipe p s
            else End

test_takeUntilPipe :: Bool
test_takeUntilPipe =
    let
        p = runIdentity . toListSink . takeUntilPipe (< 5) . fromListSource
    in
        p [1, 2, 3, 4, 5, 6, 7, 8, 9] == [1 :: Int, 2, 3, 4]

-- | A pipe that breaks after the first element that satisfies a predicate
breakPipe :: Monad m => (o -> Bool) -> Pipe m r o -> Pipe m r o
breakPipe p (Pipe f) = Pipe $ \r -> case f r of
    End -> End
    Effect m -> Effect $ breakPipe p <$> m
    Element x s ->
        if p x
            then Element x $ Pipe $ \_ -> End
            else Element x $ breakPipe p s

test_breakPipe :: Bool
test_breakPipe =
    let
        p = runIdentity . toListSink . breakPipe (== 5) . fromListSource
    in
        p [1, 2, 3, 4, 5, 6, 7, 8, 9] == [1 :: Int, 2, 3, 4, 5]

-- | A pipe that sends back the output
feedbackPipe :: Monad m => Pipe m o o -> Pipe m i o
feedbackPipe = ($ NoInput) $ fix $ \go mi -> \(Pipe f) -> Pipe $ \_ ->
    case f mi of
        End -> End
        Effect m -> Effect $ go mi <$> m
        Element x s -> Element x $ go (Input x) s

-- | A pipe that adds the input to the output
reinforcePipe :: Monad m => (o -> i -> o) -> Pipe m i o -> Pipe m i o
reinforcePipe f = go
  where
    go (Pipe p) = Pipe $ \i -> case p i of
        End -> End
        Effect m -> Effect $ go <$> m
        Element x s -> Element (withInput i (f x) x) $ go s

test_feedbackPipe :: Bool
test_feedbackPipe =
    let
        p =
            runIdentity
                . toListSink
                . feedbackPipe
                . reinforcePipe (+)
                . fromListSource
    in
        p [1, 2, 3, 4, 5] == [1 :: Int, 3, 6, 10, 15]

-- | A pipe that filters out invalid input
filterInput :: Monad m => (i -> Bool) -> Pipe m i o -> Pipe m i o
filterInput p (Pipe f) = Pipe $ \i -> input $ case i of
    NoInput -> NoInput
    Input x ->
        if p x
            then Input x
            else NoInput
  where
    input what = changePipe (filterInput p) $ f what

test_filterInput :: Bool
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
        p [1, 2, 3, 4, 5] == [1 :: Int, 2, 5, 4, 9]

-- | A pipe that filters out input by checking it against a state folded on the input
filterFoldingInput
    :: Monad m
    => (c -> i -> (c, Bool))
    -> c
    -> Pipe m i o
    -> Pipe m i o
filterFoldingInput p = go
  where
    go c (Pipe f) = Pipe $ \case
        NoInput -> pass c $ f NoInput
        Input x -> case p c x of
            (c', False) -> pass c' $ f NoInput
            (c', True) -> pass c' $ f $ Input x
    pass c = changePipe $ go c

changePipe
    :: Functor m
    => (Pipe m i1 o -> Pipe m i2 o)
    -> Output m i1 o
    -> Output m i2 o
changePipe _ End = End
changePipe f (Effect m) = Effect $ f <$> m
changePipe f (Element x s) = Element x $ f s

-- data FilePathWithException
--     = FilePathWithException FilePath (Maybe IOException)
--     deriving (Show)

-- isException :: FilePathWithException -> Bool
-- isException (FilePathWithException _ Nothing) = False
-- isException _ = True

-- handleIOException
--     :: FilePath
--     -> Pipe m i FilePathWithException
--     -> IO (Pipe m i FilePathWithException)
--     -> IO (Pipe m i FilePathWithException)
-- handleIOException x s = handle $ \e -> pure . Pipe $ \_ ->
--     Element (FilePathWithException x $ Just e) s

-- generateFiles :: Pipe IO (Maybe FilePath) FilePath
-- generateFiles = go []
--   where
--     go rest = Pipe $ \case
--         Just path -> Effect $ do
--             files <- listDirectory path
--             pure $ go $ fmap (path </>) files <> rest
--         Nothing -> case rest of
--             [] -> End
--             (p : ps) -> Element p (go ps)

-- listDir :: Pipe IO (Maybe FilePath) FilePathWithException
-- listDir = go []
--   where
--     go rest = Pipe $ \case
--         Just path -> Effect $ handleIOException path (go rest) $ do
--             files <- listDirectory path
--             pure $ go $ fmap (path </>) files <> rest
--         Nothing -> case rest of
--             [] -> End
--             (p : ps) -> Element (FilePathWithException p Nothing) (go ps)

-- feedbackNoDive :: Maybe FilePath -> Pipe IO (Maybe FilePath) FilePath -> Pipe IO () FilePath
-- feedbackNoDive l (Pipe f) = Pipe $ \_ -> case f l of
--     End -> End
--     Effect m -> Effect $ feedbackNoDive Nothing <$> m
--     Element x s -> Element x $ feedbackNoDive Nothing s

-- feedbackDive :: Maybe FilePath -> Pipe IO (Maybe FilePath) FilePath -> Pipe IO () FilePath
-- feedbackDive l (Pipe f) = Pipe $ \_ -> case f l of
--     End -> End
--     Effect m -> Effect $ feedbackDive Nothing <$> m
--     Element x s -> Effect $ do
--         cx <- canonicalizePath x
--         isDir <- doesDirectoryExist cx
--         pure
--             $ if isDir
--                 then feedbackDive (Just cx) s
--                 else Pipe $ \_ -> Element cx $ feedbackDive Nothing s

-- sourceDirs
--     :: Maybe FilePath
--     -> Pipe IO (Maybe FilePath) FilePathWithException
--     -> Pipe IO () FilePathWithException
-- sourceDirs l (Pipe f) = Pipe $ \_ -> case f l of
--     End -> End
--     Effect m -> Effect $ no <$> m
--     Element (FilePathWithException x Nothing) s -> Effect $ handleIOException x (no s) $ do
--         cx <- canonicalizePath x
--         isDir <- doesDirectoryExist cx
--         pure
--             $ if isDir
--                 then yes cx s
--                 else Pipe $ \_ -> Element (FilePathWithException cx Nothing) $ no s
--     Element (FilePathWithException x (Just e)) s ->
--         Element (FilePathWithException x $ Just e) $ no s
--   where
--     yes = sourceDirs . Just
--     no = sourceDirs Nothing

-- uniqueSource :: Ord o => Pipe IO (Maybe o) b -> Pipe IO (Maybe o) b
-- uniqueSource = go mempty
--   where
--     go ks (Pipe f) = Pipe $ \case
--         Nothing -> block
--         Just x ->
--             if Set.member x ks
--                 then block
--                 else onS (go $ Set.insert x ks) $ f (Just x)
--       where
--         block = onS (go ks) $ f Nothing

-- onS :: Functor m => (Pipe m i o -> Pipe m i' o) -> Output m i o -> Output m i' o
-- onS _ End = End
-- onS c (Effect m) = Effect $ c <$> m
-- onS c (Element x s) = Element x $ c s

-- foldS :: Monad m => (c -> o -> c) -> c -> Pipe m () o -> m c
-- foldS g !c (Pipe f) = case f () of
--     End -> pure c
--     Effect m -> m >>= foldS g c
--     Element x s -> (\c' -> g c' x) <$> foldS g c s

-- foldMS :: Monad m => (o -> m ()) -> Pipe m () o -> m ()
-- foldMS g (Pipe f) = case f () of
--     End -> pure ()
--     Effect m -> m >>= foldMS g
--     Element x s -> g x >> foldMS g s

-- printS :: Show o => Pipe IO () o -> IO ()
-- printS = foldMS print

-- countS :: Monad m => Pipe m () o -> m Int
-- countS = foldS (\c _ -> c + 1) 0

-- filterS :: Monad m => (o -> Bool) -> Pipe m r o -> Pipe m r o
-- filterS p (Pipe f) = Pipe $ \r -> case f r of
--     End -> End
--     Effect m -> Effect $ filterS p <$> m
--     Element x s -> if p x then Element x $ filterS p s else Effect . pure $ filterS p s

-- traverseDir :: FilePath -> Pipe IO () FilePathWithException
-- traverseDir path =
--     sourceDirs (Just path)
--         $ uniqueSource
--         $ listDir

-- printOnlyExceptions :: FilePath -> IO ()
-- printOnlyExceptions path =
--     printS
--         $ filterS isException
--         $ traverseDir path

-- printOnlyFiles :: FilePath -> IO ()
-- printOnlyFiles path =
--     printS
--         $ filterS (not . isException)
--         $ traverseDir path

-- countFiles :: FilePath -> IO Int
-- countFiles path =
--     countS
--         $ filterS (not . isException)
--         $ traverseDir path
