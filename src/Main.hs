import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.List
import UI.NCurses
import Data.Foldable (foldMap)
import Data.Monoid
import System.Environment
import System.Random
import System.IO.Unsafe

data Wall = Wall Int Int Int Int

instance Show Wall where 
  show (Wall y0 y1 y2 x) = replicate y0 ' ' ++ replicate y1 '#' ++ replicate y2 ' '

data Game =
  Game { gBird :: Int
       , gDist :: Int
       , gWd :: Int
       , gHt :: Int
       , gWalls :: [Wall] }

instance Show Game where
  show (Game y d wd ht ws) = intersperse ' ' (unlines m)
    where
      m = insertInto y (wd `div` 2) '@' $ transpose $ fillCols ht wd ws

getScore :: GameState -> Int
getScore (GameState _ (Game _ d _ _ _) _) = d


data GameState =
  GameState { gsPaused :: Bool
            , gsState :: Game
            , gsIsAlive :: Bool }

initGame :: Int -> Int -> Game
initGame wd ht = Game (ht `div` 2) 0 wd ht []

fillCols :: Int -> Int -> [Wall] -> [String]
fillCols _ 0 _ = []
fillCols h w [] = replicate h ' ':fillCols h (w-1) []
fillCols h w (s@(Wall y0 y1 y2 x):ws) 
  | x == w = show s:fillCols h (w-1) ws
  | otherwise = replicate h ' ':fillCols h (w-1) (s:ws)

collides :: Game -> Bool
collides (Game y d wd ht ws) = getAny $ foldMap (Any . (\(Wall y0 y1 y2 wx) -> y >= y0 && y <= y0 + y1 && wd `div` 2 == wx)) ws

shiftWalls :: [Wall] -> [Wall]
shiftWalls [] = []
shiftWalls ((s@(Wall y0 y1 y2 x)):xs)
  | True = Wall y0 y1 y2 (x-1):shiftWalls xs
  | otherwise = shiftWalls xs

getWall :: Int -> Int -> Int -> Wall
getWall wd ht s = Wall y0 y1 ht wd where
  y0 = 2
  y1 = y0+1

stepGame :: GameState -> (Int -> Int) -> GameState
stepGame gs@(GameState True _ _) _ = gs
stepGame gs@(GameState _ b@(Game y d wd ht ws) _) f
 | y' <= 0 || y' > ht = GameState False b False
 | collides (Game y' d' wd ht ws') = GameState False b False
 | otherwise = GameState False (Game y' d' wd ht ws') True
 where
    ws' = if d' `mod` 10 == 0
            then getWall wd ht (ht `div` 2):shiftWalls ws
            else shiftWalls ws
    y' = f y
    d' = d + 1

insertInto :: Int -> Int -> a -> [[a]] -> [[a]]
insertInto j i v xs = take j xs ++ (take i (xs!!j) ++ v:drop (i+1) (xs!!j)):drop j xs 

data Input = Flap | Quit | NoOp | Pause

getInput :: Window -> Curses Input
getInput w = do
  ev <- getEvent w (Just 0)
  case ev of
    Just (EventCharacter c) | c == ' ' -> return Flap
    Just (EventCharacter c) | c == 'q' -> return Quit
    Just (EventCharacter c) | c == 'p' -> return Pause
    _ -> return NoOp

mainLoop :: Int -> Int -> Window -> [Int] -> Curses [Int]
mainLoop wd ht w scores = do
  drawMenu wd ht w scores
  inp <- getInput w
  case inp of
    Pause -> do
      score <- gameLoop w (GameState False (initGame wd ht) True)
      mainLoop wd ht w (score:scores)
    Quit -> return scores
    _ -> mainLoop wd ht w scores

gameLoop :: Window -> GameState -> Curses Int
gameLoop w gs@(GameState paused game alive) =
  if not alive
    then return (getScore gs)
    else do
      drawGame w gs
      inp <- getInput w
      case inp of
        Pause -> gameLoop w (GameState (not paused) game True)
        NoOp -> gameLoop w (stepGame gs id)
        Flap -> gameLoop w (stepGame gs (+1))
        Quit -> return (getScore gs)

drawMenu :: Int -> Int -> Window -> [Int] -> Curses ()
drawMenu wd ht w gs = do
  updateWindow w $ do
    moveCursor (toInteger (ht `div` 2)) (toInteger (wd `div` 2))
    drawString $ "Scores: " ++ show (sort gs)
  render

drawGame :: Window -> GameState -> Curses ()
drawGame w gs = do
  let Game y d wd ht _ = gsState gs
  updateWindow w $ do
    moveCursor 0 0
    drawString $ show (gsState gs)
    drawBorder (Just $ Glyph '|' []) (Just $ Glyph '|' []) 
               (Just $ Glyph '-' []) (Just $ Glyph '-' [])
               (Just $ Glyph '+' []) (Just $ Glyph '+' [])
               (Just $ Glyph '+' []) (Just $ Glyph '+' [])
    moveCursor (toInteger ht) 1
    when (gsPaused gs) $ drawString " PAUSED "
    drawString $ "Score " ++ show d ++ " height: " ++ show y
  render

main :: IO ()
main = runCurses $ do
  args <- liftIO getArgs
  let wd = read (head args) :: Int
      ht = read (args !! 1) :: Int
  setEcho False
  setCursorMode CursorInvisible
  w <- defaultWindow
  scores <- mainLoop wd ht w []
  liftIO $ print (sort scores)
