module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (foldM, forM_, unless, when)
import           Data.Int (Int32)
import           Data.Vector.Unboxed.Mutable (IOVector)
import qualified Data.Vector.Unboxed.Mutable as Vector
import           Data.Word (Word8)
import           Foreign.C.Types (CInt)

import           SDL (Event, Point (P), Rectangle (Rectangle), V2 (V2), V4 (V4), ($=))
import qualified SDL

main :: IO ()
main = do
  SDL.initializeAll

  let windowSettings = SDL.defaultWindow { SDL.windowInitialSize = V2 1280 720 }
  window <- SDL.createWindow "Game Of Life" windowSettings
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.cursorVisible $= False

  mainLoop window renderer

mainLoop :: SDL.Window -> SDL.Renderer -> IO ()
mainLoop window renderer = do
  V2 w h <- SDL.get (SDL.windowSize window)
  gameState <- gameInit (fromIntegral w) (fromIntegral h)

  timeFirstFrame <- SDL.time
  continue timeFirstFrame gameState
  where
    targetTimePerFrame = 1.0 / 60.0
    continue timeLastFrame gameState = do
      events <- SDL.pollEvents
      gameState' <- gameUpdate renderer events gameState

      let targetTime = timeLastFrame + targetTimePerFrame
      sleepUntil targetTime

      SDL.present renderer
      timeEndFrame <- SDL.time

      let quit = any isQuitEvent events
      unless quit $ continue timeEndFrame gameState'

-- Put the thread to sleep until less than 0.5 milliseconds remain before the
-- target time, then spin the remaining time.
sleepUntil :: Double -> IO ()
sleepUntil targetTime = do
  t <- SDL.time
  let timeRemain = targetTime - t
      timeRemainUs = floor (timeRemain * 1000000.0) :: Int
  when (timeRemainUs > 500) $ threadDelay timeRemainUs
  spinUntil targetTime

-- Spin until the target time is reached.
spinUntil :: Double -> IO ()
spinUntil targetTime = go
  where
    go = do
      t <- SDL.time
      unless (t >= targetTime) go

isKeyDownEvent :: (SDL.Scancode -> Bool) -> SDL.Event -> Bool
isKeyDownEvent predicate event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent
      | motion <- SDL.keyboardEventKeyMotion keyboardEvent,
        keysym <- SDL.keyboardEventKeysym keyboardEvent,
        scancode <- SDL.keysymScancode keysym,
        motion == SDL.Pressed
          -> predicate scancode
    _ -> False

isQuitEvent :: SDL.Event -> Bool
isQuitEvent = isKeyDownEvent $ \scancode -> scancode == SDL.ScancodeEscape || scancode == SDL.ScancodeQ

isPauseEvent :: SDL.Event -> Bool
isPauseEvent = isKeyDownEvent (== SDL.ScancodeP)

isMouseDownEvent :: SDL.Event -> Bool
isMouseDownEvent event
  | SDL.MouseButtonEvent buttonEvent <- SDL.eventPayload event,
    SDL.Pressed <- SDL.mouseButtonEventMotion buttonEvent,
    SDL.ButtonLeft <- SDL.mouseButtonEventButton buttonEvent = True
isMouseDownEvent _ = False

data GameState = GameState {
  frame   :: Int32,
  pause   :: Bool,
  width   :: Int32,
  height  :: Int32,
  gen0    :: Cells,
  gen1    :: Cells
}

data Cells = Cells {
  width  :: Int32,
  height :: Int32,
  values :: IOVector Int32
}

readCell :: Cells -> Int32 -> Int32 -> IO Int32
readCell Cells { width, values } x y = Vector.read values (fromIntegral $ y * width + x)

writeCell :: Cells -> Int32 -> Int32 -> Int32 -> IO ()
writeCell Cells { width, values } x y = Vector.write values (fromIntegral $ y * width + x)

writeCells :: Cells -> Int32 -> Int32 -> [(Int32, Int32)] -> Int32 -> IO ()
writeCells cells offsetX offsetY points value =
  forM_ points $ \(x, y) -> writeCell cells (x + offsetX) (y + offsetY) value

pattern :: [(Int32, Int32)]
pattern = [(1, 0), (2, 0), (2, 1), (0, 2), (1, 2), (2, 2)]

gameInit :: Int32 -> Int32 -> IO GameState
gameInit windowWidth windowHeight = do
  let frame = 1
      pause = False
      width = windowWidth `div` fromIntegral cellSize
      height = windowHeight `div` fromIntegral cellSize
      numCells = fromIntegral (width * height)
  gen0Values <- Vector.new numCells
  gen1Values <- Vector.new numCells
  let gen0 = Cells width height gen0Values
  let gen1 = Cells width height gen1Values

  return GameState {..}

gameUpdate :: SDL.Renderer -> [SDL.Event] -> GameState -> IO GameState
gameUpdate renderer events state@GameState {..} = do
  P (V2 mouseX mouseY) <- SDL.getAbsoluteMouseLocation
  isPressed <- SDL.getMouseButtons
  let mouseCellX = fromIntegral (mouseX `div` cellSize)
      mouseCellY = fromIntegral (mouseY `div` cellSize)
      mouseIsPressed = isPressed SDL.ButtonRight
      mouseWasPressed = any isMouseDownEvent events
      pause' = pause /= any isPauseEvent events

  let state' = state {
        frame = frame + 1,
        pause = pause',
        gen0 = if pause then gen0 else gen1,
        gen1 = if pause then gen1 else gen0
      }

  when (mouseIsPressed || mouseWasPressed) $ writeCells gen1 mouseCellX mouseCellY pattern frame

  unless pause $ stepGeneration frame gen1 gen0

  drawGeneration renderer frame gen1
  drawCell renderer mouseCellX mouseCellY (V4 0xf7 0xca 0x88 0xff)

  return state'

stepGeneration :: Int32 -> Cells -> Cells -> IO ()
stepGeneration frame gen0 gen1@Cells { width, height } =
  forM_ [0 .. height - 1] $ \y ->
    forM_ [0 .. width - 1] $ \x -> do
      neighbours <- foldM checkNeighbour 0 (neighbouringCells x y)
      value <- readCell gen0 x y
      let newValue =
            if | value > 0, neighbours == 2 || neighbours == 3 -> value
               | value > 0, otherwise -> 0
               | value == 0, neighbours == 3 -> frame
               | otherwise -> 0
      writeCell gen1 x y newValue
  where
    checkNeighbour :: Int32 -> (Int32, Int32) -> IO Int32
    checkNeighbour !n (x, y)
      | x < 0 || x >= width || y < 0 || y >= height = return n
      | otherwise = do
          value <- readCell gen0 x y
          return $ n + if value > 0 then 1 else 0
    neighbouringCells x y =
      [
        (x-1,y-1),(x,y-1),(x+1,y-1),
        (x-1,y),(x+1,y),
        (x-1,y+1),(x,y+1),(x+1,y+1)
      ]

cellSize :: CInt
cellSize = 2 -- 2x2 pixles

drawCell :: SDL.Renderer -> Int32 -> Int32 -> V4 Word8 -> IO ()
drawCell renderer x y color = do
  let rect = Rectangle (P $ V2 (fromIntegral x * cellSize) (fromIntegral y * cellSize)) (V2 cellSize cellSize)
  SDL.rendererDrawColor renderer $= color
  SDL.fillRect renderer (Just rect)

drawGeneration :: SDL.Renderer -> Int32 -> Cells -> IO ()
drawGeneration renderer frame cells@Cells { width, height } = do
  SDL.rendererDrawColor renderer $= V4 0x18 0x18 0x18 0xff
  SDL.clear renderer
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  forM_ [0 .. height - 1] $ \y ->
    forM_ [0 .. width - 1] $ \x -> do
      value <- readCell cells x y
      when (value > 0) $ do
        let age = frame - value
            alpha = 0xff - fromIntegral (min 0xe0 age)
            color = V4 0xff 0xff 0xff alpha
        drawCell renderer x y color -- TODO: call fillRects with batches of cells
  SDL.rendererDrawBlendMode renderer $= SDL.BlendNone

