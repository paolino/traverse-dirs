{-

This is a hypothetical issue about simplifying the composition of overproducing
or underconsuming components.

We aim to write a function that throttles the flow of elements between a source
and a sink without blocking the source.

We envision that the user of this function have
- a fast source of type 'Source m src' and
- a slow sink of type 'Sink m snk'.
- a pipe rewriter of type 'forall i. Pipe m i src -> Pipe m i snk'.

'src' is the type of the elements produced by the source and enter downstream in
the pipe transformation.

'snk' is the type of the elements consumed by the sink and exit downstream in
the pipe transformation.

Because it's our responsibility to pull from the source, and push to the sink,
the user might provide a decent buffer in both the source and the sink. However,
if the source is faster, the sink will eventually overflow, regardless of the
buffer size. OTOH if we avoid the sink overflow by not pulling from the source,
we would overflow the source buffer.

The approach chosen for the function `throttler` is to consume the source as
quickly as possible by dropping elements while the sink is slowly processing,
and passing actual elements to the sink only when it is ready to consume.

The complete success is for `throttler` to require a buffer of size 1 in the
sink and the source, which is obtainable because it's implemention will be
deterministic (no IO)
-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Control.Bandwidth where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM
    ( atomically
    , isFullTBQueue
    , lengthTBQueue
    , newTBQueueIO
    , readTBQueue
    , writeTBQueue
    )
import Control.Exception (SomeException, handle)
import Control.Monad (forever, when)
import Control.Monad.Cont (ContT (..), evalContT)
import Control.Monad.Fix (fix)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Pipe
    ( Pipe (..)
    , mapPipe
    , runPipe
    , takePipe
    , unfoldMSource_
    )
import FileSystem.Traverse (Input (NextFile))

-- | A source of values of type @a@ in the monad @m@.
newtype Source m a = Source
    { pull :: m a
    }

-- | A sink of values of type @a@ in the monad @m@.
-- It has a buffer of elements that can be queried for its size
data Sink m a = Sink
    { push :: a -> m ()
    , fullness :: m Bool
    }

amendPush :: (Monad m) => (a -> m ()) -> Sink m a -> Sink m a
amendPush f s = s{push = \x -> f x >> push s x}

-- | A pipe runner that will throttle the flow of elements between a source and a sink
-- without blocking the source
throttler
    :: (Monad m)
    => Source m src
    -- ^ Source to receive elements from
    -> Sink m snk
    -- ^ Sink to send elements to
    -> (forall i. Pipe m i src -> Pipe m i snk)
    -- ^ pipe transformation, that will process the data
    -> m ()
throttler source sink pipe =
    throttlingSink sink $ pipe $ throttledSource source

-- | A control signal to throttle the flow of elements
data Throttle = Throttle | Run
    deriving (Show)

-- | A pipe layer that consumes its component as fast as possible, but discard
-- elements when receiving 'Pause' in the feedback
throttledSource :: (Monad m) => Source m o -> Pipe m Throttle o
throttledSource Source{pull} = fix
    $ \go -> Feedback
        $ \i -> Effect $ do
            x <- pull
            pure $ case i of
                Throttle -> go
                Run -> Element x go

-- | A pipe that sends elements to a sink and send 'Pause' in the feedback
-- when the elements in the buffer of the sink reach the threshold
-- All elements received are also replicated into the output
throttlingSink
    :: (Monad m)
    => Sink m o
    -- ^ sink to send elements to
    -> Pipe m Throttle o
    -- ^ pipe of o that is controlled by Pause
    -> m ()
throttlingSink Sink{push, fullness} = runPipe push fullnessTest
  where
    fullnessTest = do
        n <- fullness
        pure $ if n then Throttle else Run

--- Testing our work ---

{-
We create implementations of the 'Source' and 'Sink' values for the test.

- The 'ExplodingBuffer' is a buffer that explodes if full. In a real app, we'd
    use the same bounded channel as here but without the exploding condition. They
    would silently drop elements when full and block the pipeline.

- The 'slowExplodingSink' consumes elements at a fixed rate and the
    'fastExplodingSourceOfInts' produces increasing numbers at a fixed rate.

- The 'test' function creates the 'Sink' and 'Source' values and runs a pipeline
    that consumes elements from the source and sends them to the sink.

- The pipeline is throttled by the 'throttler' function that sends 'Pause'
    signals to the source when the sink is full.
-}

-- | A queue that will explode if the buffer is full
data ExplodingBuffer a = ExplodingBuffer
    { explodingPush :: a -> IO ()
    -- ^ push an element to the buffer or explode
    , explodingPull :: IO a
    -- ^ pull an element from the buffer
    , explodingFullness :: IO Bool
    -- ^ will signal if the buffer fullness has reached some threshold
    }

-- | Create a new 'ExplodingBuffer' with a limit on the number of elements
newExplodingBuffer
    :: String
    -- ^ error message when the queue explodes
    -> Int
    -- ^ limit on the number of elements
    -> IO (ExplodingBuffer a)
newExplodingBuffer msg limit = do
    buffer <- newTBQueueIO $ fromIntegral limit
    pure
        ExplodingBuffer
            { explodingPush = \o -> do
                full <- atomically $ isFullTBQueue buffer
                when full $ error msg
                atomically $ writeTBQueue buffer o
            , explodingPull = atomically $ readTBQueue buffer
            , explodingFullness = do
                elements <- atomically $ lengthTBQueue buffer
                pure $ 2 * elements > fromIntegral limit
            }

-- | A sink that consumes elements at a fixed (slow) rate and
-- has an internal buffer of limit elements. It will explode if the buffer is full
slowExplodingSink
    :: ExplodingBuffer o
    -> Int
    -- ^ Delay in milliseconds between element consumption
    -> ContT r IO (Sink IO o)
slowExplodingSink ExplodingBuffer{..} delay = do
    _ <- ContT
        $ withAsync
        $ forever
        $ do
            _ <- explodingPull
            threadDelay delay
    pure
        $ Sink
            { push = explodingPush
            , fullness = explodingFullness
            }

-- | A source that produce increasing numbers at a fixed (fast) rate and
-- has an internal buffer of limit elements. It will explode if the buffer is full
fastExplodingSourceOfInts
    :: ExplodingBuffer Int -> Int -> ContT r IO (Source IO Int)
fastExplodingSourceOfInts ExplodingBuffer{..} delay = do
    _ <- ContT
        $ withAsync
        $ ($ 0) . fix
        $ \loop x -> do
            explodingPush x
            threadDelay $ delay
            loop $! succ x
    pure $ Source explodingPull

newStringExplodingSink :: Int -> Int -> ContT r IO (Sink IO String)
newStringExplodingSink bufferSize delay = do
    buffer <- liftIO $ newExplodingBuffer "sink buffer exploded" bufferSize
    amendPush putStrLn <$> slowExplodingSink buffer delay

newIntExplodingSource :: Int -> Int -> ContT r IO (Source IO Int)
newIntExplodingSource bufferSize delay = do
    buffer <- liftIO $ newExplodingBuffer "source buffer exploded" bufferSize
    fastExplodingSourceOfInts buffer delay

-- 1s , 1 element per second
sinkDelay :: Int
sinkDelay = 1000000

-- 1ms , 1000 elements per second , barely achievable with
-- the current implementation
sourceDelay :: Int
sourceDelay = 1000

-- a user application , doesn't matter what it does, but it cannot touch the @i@
applicationPipe :: Pipe IO i Int -> Pipe IO i String
applicationPipe = takePipe 20 . mapPipe show

testGood :: IO ()
testGood = evalContT $ do
    let bufferSize = 1

    liftIO $ putStrLn "********* Starting the safe runner"
    stringSinkSafe <- newStringExplodingSink bufferSize sinkDelay
    intSourceSafe <- newIntExplodingSource bufferSize sourceDelay
    let
        safeRunner = throttler intSourceSafe stringSinkSafe applicationPipe
    liftIO safeRunner

testBroken :: IO ()
testBroken = evalContT $ do
    let bufferSize = 10 -- can hold 10 elements, but we will send 20
    liftIO $ putStrLn "******** Starting the broken runner"
    stringSinkBomb <- newStringExplodingSink bufferSize sinkDelay
    intSourceBomb <- newIntExplodingSource bufferSize sourceDelay
    let
        brokenRunner =
            runPipe (push stringSinkBomb) (pure NextFile)
                $ applicationPipe
                $ unfoldMSource_ (pull intSourceBomb)
    liftIO $ handle (print @SomeException) $ brokenRunner

test :: IO ()
test = do
    testBroken
    testGood

printInput :: (Show i) => Pipe IO i o -> Pipe IO i o
printInput = fix $ \go -> \case
    End -> End
    Effect m -> Effect $ go <$> m
    Element i s -> Element i $ go s
    Feedback f -> Feedback $ \i -> Effect $ do
        print i
        pure $ go $ f i

printOutput :: (Show o) => Pipe IO i o -> Pipe IO i o
printOutput = fix $ \go -> \case
    End -> End
    Effect m -> Effect $ go <$> m
    Element o s -> Effect $ do
        print o
        pure $ Element o $ go s
    Feedback f -> Feedback $ go . f

-- pipeline $ throttler 0.3 stringSink placeholderPipe
