import Control.Concurrent (threadDelay)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List
import UI.NCurses
import Data.Foldable (foldMap)
import Data.Monoid
import System.Environment
import System.Random
import System.IO.Unsafe

{- A wall is the bottom, top, and horizontal position -}
data Wall = Wall Integer Integer Integer

data Game =
  Game { gBird :: Integer
       , gDist :: Integer
       , gWd :: Integer
       , gHt :: Integer
       , gWalls :: [Wall] }

getScore :: GameState -> Integer
getScore (GameState _ (Game _ d _ _ _) _) = d


data GameState =
  GameState { gsPaused :: Bool
            , gsState :: Game
            , gsIsAlive :: Bool }

initGame :: Integer -> Integer -> Game
initGame wd ht = Game (ht `div` 2) 0 wd ht []

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

stepGame :: Double -> GameState -> (Integer -> Integer) -> GameState
stepGame rd gs@(GameState True _ _) _ = gs
stepGame rd gs@(GameState _ b@(Game y d wd ht ws) _) f
 | y' <= 0 || y' > ht = GameState False b False
 | collides y' wd ws' = GameState False b False
 | otherwise = GameState False (Game y' d' wd ht ws') True
 where
    ws' = if d' `mod` 10 == 0
            then getWall rd wd ht (ht `div` 2):shiftWalls wd ws
            else shiftWalls wd ws
    y' = f y
    d' = d + 1

insertInto :: Integer -> Integer -> a -> [[a]] -> [[a]]
insertInto j i v xs = take j' xs ++ (take i' (xs!!j') ++ v:drop (i'+1) (xs!!j')):drop j' xs 
  where i' = fromIntegral i
        j' = fromIntegral j

data Input = Flap | Quit | NoOp | Pause

getInput :: Integer -> Window -> Curses Input
getInput d w = do
  ev <- getEvent w (Just (200 ))
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
      score <- gameLoop w (GameState False (initGame wd ht) True)
      mainLoop wd ht w (score:scores)
    Quit -> return scores
    _ -> mainLoop wd ht w scores

gameLoop :: Window -> GameState -> Curses Integer
gameLoop w gs@(GameState paused game alive) =
  if not alive
    then return (getScore gs)
    else do
      drawGame w gs
      r <- liftIO $ getStdRandom (randomR (0,1))
      inp <- getInput (getScore gs) w
      case inp of
        Pause -> gameLoop w (GameState (not paused) game True)
        NoOp -> gameLoop w (stepGame r gs (+1))
        Flap -> gameLoop w (stepGame r gs (\x -> x - 1))
        Quit -> return (getScore gs)

drawMenu :: Integer -> Integer -> Window -> [Integer] -> Curses ()
drawMenu wd ht w gs = do
  updateWindow w $ do
    let title = "HASKY BIRD" 
    moveCursor (ht `div` 2 - 1) (wd `div` 2 - toInteger (length title) `div` 2)
    drawString title
    let scores = "Scores: " ++ show (sort gs)
    moveCursor (ht `div` 2) (wd `div` 2 - toInteger (length scores) `div` 2)
    drawString  scores
    drawBox Nothing Nothing
  render

drawChar :: Integer -> Integer -> Integer -> Integer -> Char -> Update ()
drawChar ht wd row col c
  | row < 0 || row >= ht || col < 0 || col >= wd - 1 = return ()
  | otherwise = moveCursor row col >> drawString [c]

drawWall :: Integer -> Integer -> Wall -> Update ()
drawWall ht wd (Wall y0 y1 wx) =
  mapM_ (\y -> drawChar ht wd y wx '#') [y0..y1]

drawGame :: Window -> GameState -> Curses ()
drawGame w gs = do
  let Game y d wd ht ws = gsState gs
  updateWindow w $ do
    clear
    drawChar ht wd y (wd `div` 2) '@'
    mapM_ (drawWall ht wd) ws
    drawBox Nothing Nothing
    moveCursor (ht - 1) 1
    when (gsPaused gs) $ drawString " PAUSED "
    drawString $ "Score " ++ show d ++ " height: " ++ show y
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
