module Main (main) where

import           Control.Concurrent (threadDelay)
import           Control.Monad (foldM, forM_, unless, when)
import           Data.Bits ((.|.), shiftL)
import           Data.Int (Int32)
import           Control.Monad.Primitive (PrimState)
import           Data.Primitive.PrimArray (MutablePrimArray)
import qualified Data.Primitive.PrimArray as Array
import           Data.Word (Word8, Word32)
import           Foreign.Ptr (Ptr, castPtr, plusPtr)
import           Foreign.Storable (poke, sizeOf)
import           Text.Printf (printf)

import           SDL (Event, Point (P), V2 (V2), ($=))
import qualified SDL

main :: IO ()
main = do
  SDL.initializeAll

  let windowSettings =
        SDL.defaultWindow {
          SDL.windowInitialSize = V2 1280 720,
          SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
        }
      rendererSettings =
        SDL.defaultRenderer {
          SDL.rendererType = SDL.AcceleratedRenderer
        }
  window <- SDL.createWindow "Game Of Life" windowSettings
  renderer <- SDL.createRenderer window (-1) rendererSettings
  rendererInfo <- SDL.getRendererInfo renderer
  putStrLn $ "Renderer: " ++ show (SDL.rendererInfoName rendererInfo)
  windowSize@(V2 windowWidth windowHeight) <- SDL.get (SDL.windowSize window)
  texture <- SDL.createTexture renderer SDL.RGB888 SDL.TextureAccessStreaming windowSize

  SDL.cursorVisible $= False

  mainLoop renderer texture (fromIntegral windowWidth, fromIntegral windowHeight)

mainLoop :: SDL.Renderer -> SDL.Texture -> (Int, Int) -> IO ()
mainLoop renderer texture (windowWidth, windowHeight) = do
  gameState <- gameInit windowWidth windowHeight

  timeFirstFrame <- SDL.time
  continue 1 timeFirstFrame gameState
  where
    targetTimePerFrame = 1.0 / 60.0
    continue !frame timeLastFrame gameState = do
      events <- SDL.pollEvents

      (castPtr -> pixels, fromIntegral -> pitch) <- SDL.lockTexture texture Nothing
      gameState' <- gameUpdate frame (Buffer pixels pitch windowWidth windowHeight) events gameState
      SDL.unlockTexture texture
      SDL.copy renderer texture Nothing Nothing
      timeEndWork <- SDL.time

      let targetTime = timeLastFrame + targetTimePerFrame
      sleepUntil targetTime
      timeEndSleep <- SDL.time

      SDL.present renderer
      let timeEndFrame = timeEndSleep

      when (frame `mod` 60 == 0) $ do
        let durationWork = timeEndWork - timeLastFrame
            durationSleep = timeEndSleep - timeEndWork
            durationFrame = timeEndFrame - timeLastFrame
        printf "wk=%.1f sl=%.1f fr=%.3f fps=%.1f\n"
          (durationWork * 1000.0)
          (durationSleep * 1000.0)
          (durationFrame * 1000.0)
          (1.0 / durationFrame)

      let quit = any isQuitEvent events
      unless quit $ continue (frame + 1) timeEndFrame gameState'

-- Put the thread to sleep until less than one millisecond remains before the
-- target time, then spin the remaining time.
sleepUntil :: Double -> IO ()
sleepUntil targetTime = do
  now <- SDL.time
  let thresholdDuration = 0.001 -- one millisecond
  when (now + thresholdDuration < targetTime) $ do
    let sleepTime = targetTime - now - thresholdDuration
    threadDelay (floor $ sleepTime * 1000.0 * 1000.0)
  spinUntil targetTime

-- Spin until the target time is reached.
spinUntil :: Double -> IO ()
spinUntil targetTime = go
  where
    go = do
      now <- SDL.time
      unless (now >= targetTime) go

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

isMouseDownEvent :: SDL.Event -> Bool
isMouseDownEvent event
  | SDL.MouseButtonEvent buttonEvent <- SDL.eventPayload event,
    SDL.Pressed <- SDL.mouseButtonEventMotion buttonEvent,
    SDL.ButtonLeft <- SDL.mouseButtonEventButton buttonEvent = True
isMouseDownEvent _ = False

data GameState = GameState {
  pause   :: Bool,
  step    :: Bool,
  width   :: Int,
  height  :: Int,
  gen0    :: Cells,
  gen1    :: Cells,
  selectedPatternNumber :: Int,
  selectedRotationNumber :: Int
}

data Cells = Cells {
  width  :: Int,
  height :: Int,
  values :: MutablePrimArray (PrimState IO) Int32
}

readCell :: Cells -> Int -> Int -> IO Int32
readCell Cells { width, values } x y =
  Array.readPrimArray values (y * width + x)

writeCell :: Cells -> Int -> Int -> Int32 -> IO ()
writeCell Cells { width, values } x y =
  Array.writePrimArray values (y * width + x)

writeCells :: Cells -> Int -> Int -> Rotation -> Pattern -> Int32 -> IO ()
writeCells cells@Cells { width, height } offsetX offsetY (rx, ry) (Pattern ox oy points) value =
  forM_ points $ \(x, y) -> do
    let pointX = rx * (x - ox) + offsetX
        pointY = ry * (y - oy) + offsetY
    when (pointX >= 0 && pointY >= 0 && pointX < width && pointY < height ) $
      writeCell cells pointX pointY value

type Rotation = (Int, Int)

rotations :: [Rotation]
rotations = [(1, 1), (1, -1), (-1, -1), (-1, 1)]

crosshairsPattern :: Pattern
crosshairsPattern = 
    Pattern 0 0 [
      (-20, 0), (-21, 0), (-22, 0), (-23, 0),
      (0, -20), (0, -21), (0, -22), (0, -23),
      (20, 0), (21, 0), (22, 0), (23, 0),
      (0, 20), (0, 21), (0, 22), (0, 23)
    ]

data Pattern = Pattern
  { _originX :: Int,
    _originY :: Int,
    _points  :: [(Int, Int)]
  }

patterns :: [Pattern]
patterns = [glider, gliderMistake, gliderGun]
  where
    glider = Pattern 0 0 [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
    gliderMistake = Pattern 0 0 [(1, 0), (2, 0), (2, 1), (0, 2), (1, 2), (2, 2)]
    gliderGun =
      Pattern 18 5 [
        (0,  5), (0,  6),
        (1,  5), (1,  6),
        (10, 5), (10, 6), (10, 7),
        (11, 4), (11, 8),
        (12, 3), (12, 9),
        (13, 3), (13, 9),
        (14, 6),
        (15, 4), (15, 8),
        (16, 5), (16, 6), (16, 7),
        (17, 6),
        (20, 3), (20, 4), (20, 5),
        (21, 3), (21, 4), (21, 5),
        (22, 2), (22, 6),
        (24, 1), (24, 2), (24, 6), (24, 7),
        (34, 3), (34, 4),
        (35, 3), (35, 4)
      ]

gameInit :: Int -> Int -> IO GameState
gameInit windowWidth windowHeight = do
  let pause = False
      step = False
      width = windowWidth `div` cellSize
      height = windowHeight `div` cellSize
      numCells = fromIntegral (width * height)
      selectedPatternNumber = 0
      selectedRotationNumber = 0
  gen0Values <- Array.newPrimArray numCells
  gen1Values <- Array.newPrimArray numCells
  let gen0 = Cells width height gen0Values
  let gen1 = Cells width height gen1Values
  return GameState {..}

gameUpdate :: Int32 -> Buffer -> [SDL.Event] -> GameState -> IO GameState
gameUpdate frame buffer events state = do
  state'@GameState {..} <- foldM (flip processInput) state events

  P (V2 mouseX mouseY) <- SDL.getAbsoluteMouseLocation
  isPressed <- SDL.getMouseButtons
  let mouseCellX = fromIntegral mouseX `div` cellSize
      mouseCellY = fromIntegral mouseY `div` cellSize
      mouseIsPressed = isPressed SDL.ButtonRight -- held down for dragging
      mouseWasPressed = any isMouseDownEvent events
      selectedPattern = patterns !! selectedPatternNumber
      selectedRotation = rotations !! selectedRotationNumber
      advance = step || not pause

  when (mouseIsPressed || mouseWasPressed) $
    writeCells gen1 mouseCellX mouseCellY selectedRotation selectedPattern frame

  when advance $
    stepGeneration frame gen1 gen0

  drawGeneration buffer frame gen1
  drawPattern buffer mouseCellX mouseCellY selectedRotation selectedPattern 0xf7ca88
  drawPattern buffer mouseCellX mouseCellY selectedRotation crosshairsPattern 0x383838

  return state' {
      step = False,
      gen0 = if advance then gen1 else gen0,
      gen1 = if advance then gen0 else gen1
    }

processInput :: SDL.Event -> GameState -> IO GameState
processInput event
  | SDL.KeyboardEvent keyboardEvent <- SDL.eventPayload event,
    SDL.Pressed <- SDL.keyboardEventKeyMotion keyboardEvent,
    keysym <- SDL.keyboardEventKeysym keyboardEvent,
    scancode <- SDL.keysymScancode keysym
    = keymap scancode
processInput _ = return

keymap :: SDL.Scancode -> GameState -> IO GameState
keymap scancode state@GameState {..} =
  case scancode of
    SDL.Scancode1 -> return state { selectedPatternNumber = 0 }
    SDL.Scancode2 -> return state { selectedPatternNumber = 1 }
    SDL.Scancode3 -> return state { selectedPatternNumber = 2 }
    SDL.ScancodeP -> return state { pause = not pause }
    SDL.ScancodeR -> return state { selectedRotationNumber = succ selectedRotationNumber `mod` 4 }
    SDL.ScancodeBackspace -> clearGeneration gen1 >> return state
    SDL.ScancodePeriod -> return state { step = True }
    _ -> return state

clearGeneration :: Cells -> IO ()
clearGeneration gen@Cells { width, height } =
  forM_ [0 .. height - 1] $ \y ->
    forM_ [0 .. width - 1] $ \x ->
      writeCell gen x y 0

stepGeneration :: Int32 -> Cells -> Cells -> IO ()
stepGeneration frame gen0 gen1@Cells { width, height } =
  forM_ [1 .. height - 2] $ \y ->
    forM_ [1 .. width - 2] $ \x -> do
      neighbours <- countNeighbours gen0 x y
      value <- readCell gen0 x y
      let newValue =
            if | value > 0, neighbours == 2 || neighbours == 3 -> value
               | value > 0, otherwise -> 0
               | value == 0, neighbours == 3 -> frame
               | otherwise -> 0
      writeCell gen1 x y newValue

countNeighbours :: Cells -> Int -> Int -> IO Int32
countNeighbours gen@Cells { .. } x y = do
    a <- readCell gen (x - 1) (y - 1)
    b <- readCell gen  x      (y - 1)
    c <- readCell gen (x + 1) (y - 1)
    d <- readCell gen (x - 1)  y
    e <- readCell gen (x + 1)  y
    f <- readCell gen (x - 1) (y + 1)
    g <- readCell gen  x      (y + 1)
    h <- readCell gen (x + 1) (y + 1)
    let count = n a + n b + n c + n d + n e + n f + n g + n h
    return count
  where
    n value = if value > 0 then 1 else 0

data Buffer = Buffer { pixels :: Ptr Word8, pitch :: Int, width :: Int, height :: Int }

backgroundColor :: Word32
backgroundColor = 0x181818

cellSize :: Int
cellSize = 2 -- 2x2 pixles

drawCell :: Buffer -> Int -> Int -> Word32 -> IO ()
drawCell Buffer {..} cellX cellY color = do
  drawPoint (cellX * cellSize) (cellY * cellSize)
  drawPoint (cellX * cellSize + 1) (cellY * cellSize)
  drawPoint (cellX * cellSize + 1) (cellY * cellSize + 1)
  drawPoint (cellX * cellSize) (cellY * cellSize + 1)
  where
    drawPoint x y
      | x < 0 || x >= width || y < 0 || y >= height = return ()
      | otherwise =
          let rowPtr  = pixels `plusPtr` (y * pitch)
              cellPtr = castPtr rowPtr `plusPtr` (x * sizeOf color)
          in  poke cellPtr color -- UNSAFE: Ensure cellPtr, x, y are within bounds.

drawPattern :: Buffer -> Int -> Int -> Rotation -> Pattern -> Word32 -> IO ()
drawPattern buffer offsetX offsetY (rx, ry) (Pattern ox oy points) color =
  forM_ points $ \(x, y) ->
    drawCell buffer (rx * (x - ox) + offsetX) (ry * (y - oy) + offsetY) color

drawGeneration :: Buffer -> Int32 -> Cells -> IO ()
drawGeneration buffer frame cells@Cells { width, height } = do
  -- TODO: Draw in scan lines for cache efficiency
  forM_ [0 .. height - 1] $ \y ->
    forM_ [0 .. width - 1] $ \x -> do
      value <- readCell cells x y
      let age = frame - value
          shade = 0xff - fromIntegral (min 0xd0 age)
          color = if value > 0 then rgb shade shade shade else backgroundColor
      drawCell buffer x y color

rgb :: Word8 -> Word8 -> Word8 -> Word32
rgb (fromIntegral -> r) (fromIntegral -> g) (fromIntegral -> b) =
  shiftL r 16 .|. shiftL g 8 .|. b

