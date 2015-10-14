import Data.List
import UI.NCurses
import Data.Foldable (foldMap)
import Data.Monoid
import System.Random
import System.IO.Unsafe

data Wall = Wall Int Int Int Int

instance Show Wall where 
    show (Wall y0 y1 y2 x) = replicate y0 ' ' ++ replicate y1 '#' ++ replicate y2 ' '

data Bird = Bird Int Int Int
instance Show Bird where
    show _ = "@"

data Game = Game { bird :: Bird , walls :: [Wall], ht :: Int , wd :: Int }

instance Show Game where
    show (Game (Bird y x d) ws ht wd) = (intersperse ' ' (unlines m)) where
         m = insertInto y x '@' $ transpose $ fillCols ht wd ws

fillCols :: Int -> Int -> [Wall] -> [String]
fillCols _ 0 _ = []
fillCols h w [] = (replicate h ' '):fillCols h (w-1) []
fillCols h w (s@(Wall y0 y1 y2 x):ws) 
  | x == w = (show s):fillCols h (w-1) ws
  | otherwise = (replicate h ' '):fillCols h (w-1) (s:ws)

collides :: Game -> Bool
collides (Game (Bird y x d) ws ht wd) = getAny $ foldMap (Any . (\(Wall y0 y1 y2 wx) -> y >= y0 && y <= y0 + y1 && x == wx)) ws

shiftWalls :: [Wall] -> [Wall]
shiftWalls [] = []
shiftWalls ((s@(Wall y0 y1 y2 x)):xs)
  | True = (Wall y0 y1 y2 (x-1)):(shiftWalls xs)
  | otherwise = shiftWalls xs

getWall :: Int -> Int -> Int -> Wall
getWall wd ht s = Wall y0 y1 ht wd where
  y0 = unsafePerformIO (getStdRandom (randomR (0, 5)))
  y1 = y0+5

stepGame :: Game -> Int -> Maybe Game
stepGame b@(Game (Bird y x d) ws ht wd ) i
 | y' <= 0 || y' > ht = Nothing
 | collides (Game (Bird y' x d') ws' ht wd) = Nothing
 | otherwise = Just (Game (Bird y' x d') ws' ht wd) where
    ws' = if d' `mod` 10 == 0 then (getWall wd ht (ht `div` 2)):shiftWalls ws
                            else shiftWalls ws
    y' = y + i
    d' = d + 1

insertInto :: Int -> Int -> a -> [[a]] -> [[a]]
insertInto j i v xs = take j xs ++ (take i (xs!!j) ++ v:(drop (i+1) (xs!!j))):(drop j xs) 

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor w p = loop (Just g) where
  g = Game (Bird 9 21 0) [] 20 40
  loop Nothing = do drawGame w g 
                    ev <- getEvent w (Just 100000000)
                    case ev of 
                         Nothing -> loop Nothing
                         Just ev' -> return ()
  loop (Just g@(Game b ws ht wd)) = do
    drawGame w g
    ev <- getEvent w (Just 250)
    case ev of 
         Nothing -> loop $ stepGame g 1
         Just ev' -> if p ev' then loop $ stepGame g (-1)
                              else return ()

drawGame :: Window -> Game -> Curses ()
drawGame w g@(Game (Bird y _ d) _ ht _) = do
    updateWindow w $ do moveCursor 0 0
                        drawString $ show g
                        drawBorder (Just $ Glyph '|' []) (Just $ Glyph '|' []) 
                                   (Just $ Glyph '-' []) (Just $ Glyph '-' [])
                                   (Just $ Glyph '+' []) (Just $ Glyph '+' [])
                                   (Just $ Glyph '+' []) (Just $ Glyph '+' [])
                        moveCursor (toInteger ht+1) 1
                        drawString $ "Score " ++ show d ++ " height: " ++ show y
    render

main :: IO ()
main = runCurses $ do
  setEcho False
  setCursorMode CursorInvisible
  w <- defaultWindow
  waitFor w (\ev -> ev == EventCharacter ' ' )