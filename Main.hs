{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Main where

-- base
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Fix (MonadFix)
import Data.Foldable
import Data.Function ((&))
import Data.Functor
import Data.Maybe
import Text.Read (readMaybe)
import Debug.Trace

-- vector-space
import Data.VectorSpace

-- utf8-string
import Data.ByteString.UTF8
-- import Data.ByteString.Lazy.Internal (fromStrict)

-- essence-of-live-coding
import LiveCoding hiding (integrate)

-- essence-of-live-coding-gloss
-- import LiveCoding.Gloss

-- essence-of-live-coding-pulse
-- import LiveCoding.Pulse

-- essence-of-live-coding-warp
import LiveCoding.Warp
import Brillo (Picture)
import Brillo.Interface.IO.Game
import Data.ByteString (fromStrict)
import Control.Monad.Reader
    ( MonadReader(ask), MonadTrans(lift), ReaderT )
import Control.Monad.Writer ( MonadWriter(tell), WriterT )
import Control.Concurrent (ThreadId, newEmptyMVar, forkIO, putMVar, threadDelay)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef, modifyIORef)
import Control.Concurrent.MVar (MVar, tryTakeMVar)
import System.Exit (exitSuccess)

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader as X hiding (ask)
import Control.Monad.Trans.Writer.Strict
import Brillo.Interface.Pure.Display (display)
-- * Main program

main = display
  (InWindow "Nice Window" (200, 200) (10, 10))
  white
  (Circle 80)

-- main :: IO ()
-- main = runHandlingStateT $ foreground liveProgram

-- Uncomment the different *RunCells to start different media backends!
liveProgram :: LiveProgram (HandlingStateT IO)
liveProgram = liveCell $ proc _ -> do
  -- warpRunCell  -< ()
  -- traceM (show 1) -< ()
  glossRunCell -< ()
  -- pulseRunCell -< ()
  returnA      -< ()

-- * Warp subcomponent

-- | Starts a webserver on port 8080
warpRunCell :: Cell (HandlingStateT IO) () (Maybe RequestInfo)
warpRunCell = proc _ -> do
    s <- runWarpC 8080 warpCell -< ()
    traceM (show 4) -< ()
    returnA -< Nothing

-- | This handles the incoming request from the webserver
warpCell :: Cell IO ((), Request) (RequestInfo, Response)
warpCell = proc ((), request) -> do
  body <- arrM lazyRequestBody -< request
  returnA -< (getRequestInfo request, emptyResponse)

-- | The type of interesting data from the request
type RequestInfo = Query

-- | Extract data from the request to use in the rest of the program
getRequestInfo :: Request -> RequestInfo
getRequestInfo = queryString

-- Extend this for a more interesting website
emptyResponse :: Response
emptyResponse = responseLBS
  status200
  [("Content-Type", "text/plain")]
  (fromStrict $ fromString $ show program)

program = reverse $ Prelude.take 100 [1..]

-- * Gloss subcomponent

-- ** Backend setup

borderX :: Num a => a
borderX = 300

borderY :: Num a => a
borderY = 400

border :: Num a => (a, a)
border = (borderX, borderY)



-- | Run the gloss backend at 30 frames per second
glossRunCell :: Cell (HandlingStateT IO) () (Maybe ())
glossRunCell = glossWrapC glossSettings glossCell
  -- & (`withDebuggerC` statePlay) -- Uncomment to display the internal state

-- ** Main gloss cell




type PictureT m = ReaderT [Event] (Control.Monad.Trans.Writer.Strict.WriterT Picture m)

-- -- | 'PictureT' specialised to the 'IO' monad.
type PictureM = PictureT IO

-- -- | Run the effects of the gloss monad stack by explicitly passing events and pictures.
runPictureT ::
  (Monad m) =>
  Cell (PictureT m) a b ->
  Cell m ([Event], a) (Picture, b)
runPictureT = runWriterC . runReaderC'

addPicture :: (Monad m) => Cell (PictureT m) Picture ()
addPicture = arrM $ lift . Control.Monad.Trans.Writer.Strict.tell

-- -- | This cell is called for every frame of the graphics output
glossCell :: Cell PictureM () ()
glossCell = proc () -> do
  events <- constM ask -< ()
  ball <- ballSim -< events
  addPicture -< ballPic ball
  returnA    -< ()

-- -- ** Ball

ballRadius :: Num a => a
ballRadius = 20

-- | Draw the ball in gloss
ballPic :: Ball -> Picture
ballPic Ball { ballPos = (x, y) } = translate x y $ color white $ thickCircle (ballRadius / 2) ballRadius

-- -- | The type of internal state of the 'ballSim'
data Ball = Ball
  { ballPos :: (Float, Float)
  , ballVel :: (Float, Float)
  } deriving Data

ballPosX = fst . ballPos
ballPosY = snd . ballPos
ballVelX = fst . ballVel
ballVelY = snd . ballVel

-- | Simulate the position of the ball, given recent events such as mouse clicks
ballSim :: (Monad m, MonadFix m) => Cell m [Event] Ball
ballSim = proc events -> do
  rec
    let accMouse = sumV $ (^-^ ballPos ball) <$> clicks events
        accCollision = sumV $ catMaybes
          [ guard (ballPosX ball < - borderX + ballRadius && ballVelX ball < 0)
              $> (- (2 * ballVelX ball), 0)
          , guard (ballPosX ball >   borderX - ballRadius && ballVelX ball > 0)
              $> (- (2 * ballVelX ball), 0)
          , guard (ballPosY ball < - borderY + ballRadius && ballVelY ball < 0)
              $> (0,- (2 * ballVelY ball))
          , guard (ballPosY ball >   borderY - ballRadius && ballVelY ball > 0)
              $> (0,- (2 * ballVelY ball))
          ]
    frictionVel <- integrate -< (-0.3) *^ ballVel ball
    impulses <- sumS -< sumV [accMouse, 0.97 *^ accCollision]
    let newVel = frictionVel ^+^ impulses
    newPos <- integrate -< newVel
    let ball = Ball newPos newVel
  returnA -< ball

-- -- | Extract the positions of left mouse clicks
clicks :: [Event] -> [(Float, Float)]
clicks = mapMaybe click

click :: Event -> Maybe (Float, Float)
click (EventKey (MouseButton LeftButton) Down _ pos) = Just pos
click _ = Nothing

-- -- * Pulse subcomponent

-- -- | Run the PulseAudio backend at 48000 samples per second
-- pulseRunCell :: Cell (HandlingStateT IO) () [()]
-- pulseRunCell = pulseWrapC 1600 $ arr (const 440) >>> sawtooth >>> addSample



defaultSettings :: GlossSettings
defaultSettings =
  GlossSettings
    { displaySetting = InWindow "Essence of live coding" (600, 800) (20, 20)
    , backgroundColor = black
    , stepsPerSecond = 30
    , debugEvents = False
    }

glossSettings :: GlossSettings
glossSettings = defaultSettings
  { debugEvents = True
  , displaySetting = InWindow "Essence of Live Coding Tutorial" (border ^* 2) (0, 0)
  }

-- -- * Utilities

sumS
  :: (Monad m, Data v, VectorSpace v)
  => Cell m v v
sumS = foldC (^+^) zeroV

integrate
  :: (Monad m, Data v, VectorSpace v, Fractional (Scalar v))
  => Cell m v v
integrate = arr (^/ fromIntegral (stepsPerSecond glossSettings)) >>> sumS





{- | In a 'Handle', store a separate thread where the gloss main loop is executed,
   and several concurrent variables to communicate with it.
-}
data GlossHandle = GlossHandle
  { glossThread :: ThreadId
  , glossVars :: GlossVars
  }

-- | The concurrent variables needed to communicate with the gloss thread.
data GlossVars = GlossVars
  { glossEventsRef :: IORef [Event]
  -- ^ Stores all 'Event's that arrived since the last tick
  , glossPicRef :: IORef Picture
  -- ^ Stores the next 'Picture' to be painted
  , glossDTimeVar :: MVar Float
  -- ^ Stores the time passed since the last tick
  , glossExitRef :: IORef Bool
  -- ^ Write 'True' here to stop the gloss thread
  }

-- -- {- | Collect all settings that the @gloss@ backend requires.
-- --    Taken from @rhine-gloss@.
-- -- -}
data GlossSettings = GlossSettings
  { displaySetting :: Display
  -- ^ Display mode (e.g. 'InWindow' or 'FullScreen').
  , backgroundColor :: Color
  -- ^ Background color.
  , stepsPerSecond :: Int
  -- ^ Number of simulation steps per second of real time.
  , debugEvents :: Bool
  -- ^ Print all incoming events to the console.
  }



{- | Will create a handle for communication with the gloss thread,
   and start gloss.
-}
glossHandle :: GlossSettings -> Handle IO GlossHandle
glossHandle GlossSettings {..} =
  Handle
    { create = do
        glossEventsRef <- newIORef []
        glossDTimeVar <- newEmptyMVar
        glossPicRef <- newIORef blank
        glossExitRef <- newIORef False
        let glossVars = GlossVars {..}
        glossThread <-
          forkIO $
            playIO displaySetting backgroundColor stepsPerSecond glossVars getPicture (handleEvent debugEvents) stepGloss
        return GlossHandle {..}
    , destroy = \GlossHandle {glossVars = GlossVars {..}, ..} -> writeIORef glossExitRef True
    }

getPicture :: GlossVars -> IO Picture
getPicture GlossVars {..} = readIORef glossPicRef

handleEvent :: Bool -> Event -> GlossVars -> IO GlossVars
handleEvent debugEvents event vars@GlossVars {..} = do
  when debugEvents $ print event
  modifyIORef glossEventsRef (event :)
  return vars

stepGloss :: Float -> GlossVars -> IO GlossVars
stepGloss dTime vars@GlossVars {..} = do
  putMVar glossDTimeVar dTime
  exitNow <- readIORef glossExitRef
  when exitNow exitSuccess
  return vars

-- -- {- | Given a cell in the gloss monad 'PictureM',
-- -- start the gloss backend and connect the cell to it.

-- -- This introduces 'Handle's containing the gloss background thread,
-- -- which need to be taken care of by calling 'runHandlingState'
-- -- or a similar function.

-- -- The resulting cell never blocks,
-- -- but returns 'Nothing' if there currently is no gloss tick.
-- -- -}
glossWrapC ::
  GlossSettings ->
  Cell PictureM a b ->
  Cell (HandlingStateT IO) a (Maybe b)
glossWrapC glossSettings cell = proc a -> do
  GlossHandle {..} <- handling $ glossHandle glossSettings -< ()
  traceM (show 2) -< ()
  
  liftCell pump -< (glossVars, a)
  where
    pump = proc (GlossVars {..}, a) -> do
      timeMaybe <- arrM tryTakeMVar -< glossDTimeVar
      case timeMaybe of
        Just _ -> do
          events <- arrM $ flip atomicModifyIORef ([],) -< glossEventsRef
          (picture, b) <- runPictureT cell -< (events, a)
          arrM (uncurry writeIORef) -< (glossPicRef, picture)
          returnA -< Just b
        Nothing -> do
          arrM threadDelay -< 1000 -- Prevent too much CPU load
          returnA -< Nothing