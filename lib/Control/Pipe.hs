{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Control.Pipe where

import Data.Set qualified as Set
import Prelude

default (Int)

-- | A datatype to generate a sequence of values of type @o@
-- , possibly interleaved with effects in @m@.
-- The continuations are additionally feeded with values of type @i@.
data Pipe m i o
    = End
    | Effect (m (Pipe m i o))
    | Element o (Pipe m i o)
    | Feedback (Input i -> Pipe m i o)

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
-- data Pipe m i o = Pipe (Input i -> Pipe m i o)

-- | An infinite source of values of type @o@.
repeatSource :: o -> Pipe m i o
repeatSource x = Element x $ repeatSource x

-- | An infinite source of values of type @o@ from a seed value.
unfoldSource :: (i -> (o, i)) -> i -> Pipe m i o
unfoldSource f x = case f x of
    (o, i) -> Element o $ unfoldSource f i

-- | A finite source of values of type @o@.
fromListSource :: [o] -> Pipe m i o
fromListSource [] = End
fromListSource (x : xs) = Element x $ fromListSource xs

-- | A finite source of values of type @o@ from a seed value.
unfoldFiniteSource :: (i -> Maybe (o, i)) -> i -> Pipe m i o
unfoldFiniteSource f x = case f x of
    Nothing -> End
    Just (o, i) -> Element o $ unfoldFiniteSource f i

-- | A source that outputs line from stdin
stdinSource :: Pipe IO i String
stdinSource = Effect $ do
    line <- getLine
    pure $ Element line stdinSource

-- | A sink that outputs line to stdout
putStrLnSink :: Pipe IO i String -> IO ()
putStrLnSink = foldMSink (const putStrLn) ()

-- | Not Tom
cat :: IO ()
cat = putStrLnSink stdinSource

-- | A sink that collects the values of type @o@ into a list.
toListSink :: (Monad m) => Pipe m i o -> m [o]
toListSink = \case
    End -> pure []
    Effect m -> m >>= toListSink
    Element x s -> (x :) <$> toListSink s
    Feedback f -> toListSink $ f NoInput

-- | A sink that burns the values of type @o@ into the effects. Generalize putStrLnSink.
sink :: (Monad m) => (o -> m ()) -> Pipe m i o -> m ()
sink f = \case
    End -> pure ()
    Effect m -> m >>= sink f
    Element x s -> f x >> sink f s
    Feedback g -> sink f $ g NoInput

-- | A sink that folds the values of type @o@ into a single value of type @c@.
foldSink :: (Monad m) => (c -> o -> c) -> c -> Pipe m i o -> m c
foldSink g !c = \case
    End -> pure c
    Effect m -> m >>= foldSink g c
    Element x s -> foldSink g (g c x) s
    Feedback f -> foldSink g c (f NoInput)

-- | A sink that folds the values of type @o@ with its monoid
monoidSink :: (Monad m, Monoid c) => Pipe m i c -> m c
monoidSink = foldSink mappend mempty

-- | A sink that folds the values of type @o@ into a single value of type @c@
-- with a folding monadic function.
foldMSink :: (Monad m) => (c -> o -> m c) -> c -> Pipe m i o -> m c
foldMSink g !c = \case
    End -> pure c
    Effect m -> m >>= foldMSink g c
    Element x s -> g c x >>= \c' -> foldMSink g c' s
    Feedback f -> foldMSink g c $ f NoInput

-- | A pipe element that changes the output type
mapPipe :: (Monad m) => (o -> o') -> Pipe m i o -> Pipe m i o'
mapPipe f = \case
    End -> End
    Effect m -> Effect $ mapPipe f <$> m
    Element x s -> Element (f x) $ mapPipe f s
    Feedback g -> Feedback $ mapPipe f . g

-- | A pipe element that filters out the values of type @o@ that maps to @Nothing@
-- and keeps the values of type @o'@ that appear in @Just@.
filterMaybePipe :: (Monad m) => (o -> Maybe o') -> Pipe m r o -> Pipe m r o'
filterMaybePipe p = \case
    End -> End
    Effect m -> Effect $ filterMaybePipe p <$> m
    Element x s ->
        case p x of
            Just y -> Element y $ filterMaybePipe p s
            Nothing -> Effect . pure $ filterMaybePipe p s
    Feedback f -> Feedback $ filterMaybePipe p . f

-- | A pipe element that filters in the values of type @o@ that satisfy a predicate.
filterPipe :: (Monad m) => (o -> Bool) -> Pipe m r o -> Pipe m r o
filterPipe p = filterMaybePipe $ \x -> if p x then Just x else Nothing

-- | A pipe element that filter out duplicate output
uniqueOutPipe :: (Monad m, Ord o) => Pipe m r o -> Pipe m r o
uniqueOutPipe = go mempty
  where
    go ks = \case
        End -> End
        Effect m -> Effect $ go ks <$> m
        Element x s ->
            if Set.member x ks
                then Effect . pure $ go ks s
                else Element x $ go (Set.insert x ks) s
        Feedback f -> Feedback $ go ks . f

-- | A pipe element that cuts off the output after a certain number of elements
takePipe :: (Monad m) => Int -> Pipe m r o -> Pipe m r o
takePipe n = \case
    End -> End
    Effect m -> Effect $ takePipe n <$> m
    Element x s ->
        if n <= 0
            then End
            else Element x $ takePipe (n - 1) s
    Feedback f -> Feedback $ takePipe n . f

-- | A pipe element that take only the first elements that satisfy a predicate
takeUntilPipe :: (Monad m) => (o -> Bool) -> Pipe m r o -> Pipe m r o
takeUntilPipe p = \case
    End -> End
    Effect m -> Effect $ takeUntilPipe p <$> m
    Element x s ->
        if p x
            then Element x $ takeUntilPipe p s
            else End
    Feedback f -> Feedback $ takeUntilPipe p . f

-- | A pipe that breaks after the first element that satisfies a predicate
breakPipe :: (Monad m) => (o -> Bool) -> Pipe m r o -> Pipe m r o
breakPipe p = \case
    End -> End
    Effect m -> Effect $ breakPipe p <$> m
    Element x s ->
        if p x
            then Element x End
            else Element x $ breakPipe p s
    Feedback f -> Feedback $ breakPipe p . f

-- | A pipe that sends back the output
feedbackPipe :: (Monad m) => Pipe m o o -> Pipe m i o
feedbackPipe = go NoInput
  where
    go mi = \case
        End -> End
        Effect m -> Effect $ go mi <$> m
        Element x s -> Element x $ go (Input x) s
        Feedback f -> go mi $ f mi

-- | A pipe that adds the input to the output
reinforcePipe :: (Monad m) => (o -> i -> o) -> Pipe m i o -> Pipe m i o
reinforcePipe f = go
  where
    go = \case
        End -> End
        Effect m -> Effect $ go <$> m
        Element x s -> Feedback $ \mi' -> Element (withInput mi' (f x) x) $ go s
        Feedback g -> Feedback $ go . g

-- | A pipe that filters out invalid input
filterInput :: (Monad m) => (i -> Bool) -> Pipe m i o -> Pipe m i o
filterInput p = filterFoldingInput (\() i -> ((), p i)) ()

-- | A pipe that filters out input by checking it against a state folded on the input
filterFoldingInput
    :: (Monad m)
    => (c -> i -> (c, Bool))
    -> c
    -> Pipe m i o
    -> Pipe m i o
filterFoldingInput p = go
  where
    go c = \case
        End -> End
        Effect m -> Effect $ go c <$> m
        Element x s -> Element x $ go c s
        Feedback f -> Feedback $ \case
            NoInput -> go c $ f NoInput
            Input i ->
                let (c', b) = p c i
                 in if b
                        then go c' $ f $ Input i
                        else go c' $ f NoInput
