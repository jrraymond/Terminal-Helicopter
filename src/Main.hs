import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List
import UI.NCurses
import Data.Foldable (foldMap)
import Data.Monoid
import System.Clock
import System.Environment
import System.Random
import System.IO.Unsafe

{- A wall is the bottom, top, and horizontal position -}
data Wall = Wall Integer Integer Integer

data Game =
  Game { gBird :: Double
       , gDist :: Integer
       , gWd :: Integer
       , gHt :: Integer
       , gWalls :: [Wall] }

getScore :: GameState -> Integer
getScore (GameState _ _ (Game _ d _ _ _) _ _) = d


data GameState =
  GameState { gsPaused :: Bool
            , gsFlapping :: Bool
            , gsState :: Game
            , gsIsAlive :: Bool
            , gsColsPerSec :: Integer }

initGame :: Integer -> Integer -> Game
initGame wd ht = Game (fromIntegral (ht `div` 2)) 0 wd ht []

wall2Str :: Integer -> Wall -> String
wall2Str ht (Wall y0 y1 wx) =
  replicate top '.' ++ replicate y' '#' ++ replicate bot '.'
  where
    bot = fromInteger y0
    y' = fromInteger (y1 - y0)
    top = fromInteger (ht - y1)

fillCols :: Integer -> Integer -> [Wall] -> [String]
fillCols _ 0 _ = []
fillCols h w [] = replicate (fromInteger h) ' ' : fillCols h (w-1) []
fillCols h w (s@(Wall y0 y1 wx):ws) 
  | wx == w = wall2Str h s : fillCols h (w-1) ws
  | otherwise = replicate (fromInteger h) ' ' : fillCols h (w-1) (s:ws)

collides :: Integer -> Integer -> [Wall] -> Bool
collides y wd =
  any (\(Wall y0 y1 wx) -> y >= y0 && y <= y1 && wd `div` 2 == wx)

shiftWalls :: Integer -> [Wall] -> [Wall]
shiftWalls _ [] = []
shiftWalls wd (Wall y0 y1 x:xs)
  | x <= wd = Wall y0 y1 (x-1):shiftWalls wd xs
  | otherwise = shiftWalls wd xs

getWall :: Double -> Integer -> Integer -> Integer -> Wall
getWall rd wd ht s = Wall y0 y1 wd where
  y0 = floor (rd * fromInteger ht)
  y1 = min (ht - 1) (y0 + ht `div` 8)

stepGame :: Double -> GameState -> GameState
stepGame rd gs@(GameState True _ _ _ _) = gs
stepGame rd gs@(GameState _ flapping b@(Game y d wd ht ws) _ _)
 | yi' <= 0 || yi' > ht || collides yi' wd ws' = gs { gsIsAlive = False }
 | otherwise = gs { gsState = Game y' d' wd ht ws'}
 where
    ws' = if d' `mod` cps == 0
            then getWall rd wd ht (ht `div` 2):shiftWalls wd ws
            else shiftWalls wd ws
    y' | flapping = y - 0.5 | otherwise = y + 0.5
    yi' = round y'
    d' = d + 1
    cps = max 5 (10 - (d `div` 100))

insertInto :: Integer -> Integer -> a -> [[a]] -> [[a]]
insertInto j i v xs = take j' xs ++ (take i' (xs!!j') ++ v:drop (i'+1) (xs!!j')):drop j' xs 
  where i' = fromIntegral i
        j' = fromIntegral j

data Input = Flap | Quit | NoOp | Pause

getInput :: Integer -> Window -> Curses Input
getInput d w = do
  ev <- getEvent w (Just 0)
  case ev of
    Just (EventCharacter c) | c == ' ' -> return Flap
    Just (EventCharacter c) | c == 'q' -> return Quit
    Just (EventCharacter c) | c == 'p' -> return Pause
    _ -> return NoOp

mainLoop :: Integer -> Integer -> Window -> [Integer] -> Curses [Integer]
mainLoop wd ht w scores = do
  drawMenu wd ht w scores
  inp <- getInput 0 w
  case inp of
    Pause -> do
      t0 <- liftIO $ getTime Monotonic
      score <- gameLoop t0 w (GameState False False (initGame wd ht) True 20)
      mainLoop wd ht w (score:scores)
    Quit -> return scores
    _ -> mainLoop wd ht w scores

updatePhysics :: Double -> Input -> GameState -> GameState
updatePhysics r inp gs =
  case inp of
    Pause | gsPaused gs -> gs { gsPaused = False }
    Pause               -> gs { gsPaused = True }
    NoOp | gsPaused gs  -> gs
    Quit                -> gs { gsIsAlive = False}
    NoOp                -> stepGame r (gs { gsFlapping = False })
    Flap                -> stepGame r (gs { gsFlapping = True })


gameLoop :: TimeSpec -> Window -> GameState -> Curses Integer
gameLoop tp w gs@(GameState paused flapping game alive cps) =
  if not alive
    then return (getScore gs)
    else do
      t <- liftIO $ getTime Monotonic
      let elapsed = t - tp
      inp <- getInput (getScore gs) w
      r <- liftIO $ getStdRandom (randomR (0,1))
      let gs' = updatePhysics r inp gs { gsColsPerSec = gsColsPerSec gs + 1}
          delay = 33333.3 --(fromIntegral (nsec (tp - t)) / 10^3) --30fps
      drawGame w gs' (show (delay / 10^3))
      liftIO $ threadDelay (floor delay)
      gameLoop t w gs'

drawMenu :: Integer -> Integer -> Window -> [Integer] -> Curses ()
drawMenu wd ht w gs = do
  updateWindow w $ do
    let title = "HASKY BIRD" 
    moveCursor (ht `div` 2 - 1) (wd `div` 2 - toInteger (length title) `div` 2)
    drawString title
    let scores = "Scores: " ++ show (take 10 (sortBy (flip compare) gs))
    moveCursor (ht `div` 2) (wd `div` 2 - toInteger (length scores) `div` 2)
    drawString scores
    let info = "Hold down space to flap, q to quit, p to play/pause."
    moveCursor (ht `div` 2 + 1) (wd `div` 2 - toInteger (length info) `div` 2)
    drawString info
    drawBox Nothing Nothing
  render

drawChar :: Integer -> Integer -> Integer -> Integer -> Char -> Update ()
drawChar ht wd row col c
  | row < 0 || row >= ht || col < 0 || col >= wd - 1 = return ()
  | otherwise = moveCursor row col >> drawString [c]

drawWall :: Integer -> Integer -> Wall -> Update ()
drawWall ht wd (Wall y0 y1 wx) =
  mapM_ (\y -> drawChar ht wd y wx '#') [y0..y1]

drawGame :: Window -> GameState -> String -> Curses ()
drawGame w gs str = do
  let Game y d wd ht ws = gsState gs
  updateWindow w $ do
    clear
    drawChar ht wd (round y) (wd `div` 2) '@'
    mapM_ (drawWall ht wd) ws
    drawBox Nothing Nothing
    moveCursor (ht - 1) 1
    when (gsPaused gs) $ drawString " PAUSED "
    drawString $ "Score " ++ show d ++ " height: " ++ show y ++ " " ++ str
  render

main :: IO ()
main = runCurses $ do
  args <- liftIO getArgs
  let wd = read (head args) :: Integer
      ht = read (args !! 1) :: Integer
  setEcho False
  setCursorMode CursorInvisible
  w <- newWindow ht wd 0 0
  scores <- mainLoop wd ht w []
  closeWindow w
  liftIO $ print (sort scores)
