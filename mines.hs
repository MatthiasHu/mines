{-# LANGUAGE TemplateHaskell #-}


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Array
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Control.Monad.Random
import Control.Monad.State
import Data.Monoid ((<>), Any(..), getAny)



type Ind = (Int, Int)

type Board = Array Ind CellState

data CellState = CellState
  { _open :: Bool
  , _mine :: Bool
  , _neighbourMines :: Int
  }

makeLenses ''CellState


cellScale :: (Num a) => a
cellScale = 50


main :: IO ()
main = do
  board <- startBoard dimens 50
  play
    (InWindow "hi" (over both (*cellScale) dimens) (0, 0))
    black
    30
    board
    boardPic
    handleEvent
    (const id)
  where dimens = (20, 10)

handleEvent :: Event -> Board -> Board
handleEvent (EventKey (MouseButton LeftButton) Down _ (x, y)) =
  handleClick (x, y)
handleEvent _ = id

handleClick :: (Float, Float) -> Board -> Board
handleClick (x0, y0) = execState $ do
  bounds <- use (to bounds)
  let (ctx, cty) = centeringTranslation bounds
      (x, y) = over both ((+1) . floor . (/cellScale)) (x0-ctx, y0-cty)
  when (inRange bounds (x, y)) $
    ix (x, y) . open .= True


startBoard :: MonadRandom m => Ind -> Int -> m Board
startBoard size mines = do
  b <- mines `timesM` addMine $ blankBoard size
  return $ validateNeighbourMines b

blankBoard :: Ind -> Board
blankBoard size = listArray ((1, 1), size) $ repeat $ CellState False False 0

-- add a mine, leaving neighbourMines invalid
addMine :: MonadRandom m => Board -> m Board
addMine board = do
  i <- randomIndex $ bounds board
  case board ^? ix i . mine of
    Just True  -> addMine board
    Just False -> return $ ix i . mine .~ True $ board

timesM :: Monad m => Int -> (a -> m a) -> a -> m a
n `timesM` f | n <= 0    = return
             | otherwise = f >=> (n-1) `timesM` f

randomIndex :: MonadRandom m => (Ind, Ind) -> m Ind
randomIndex ((x0, y0), (x1, y1)) = do
  x <- getRandomR (x0, x1)
  y <- getRandomR (y0, y1)
  return (x, y)

validateNeighbourMines :: Board -> Board
validateNeighbourMines board =
  array (bounds board)
    [ (i, cs & neighbourMines .~ actualNeighbourMines board i)
    | (i, cs) <- assocs board ]

actualNeighbourMines :: Board -> Ind -> Int
actualNeighbourMines board =
  length . filter (\i -> getAny $ board ^. ix i . mine . to Any)
  . neighbours

neighbours :: Ind -> [Ind]
neighbours (x, y) = do
  dx <- [-1, 0, 1]
  dy <- [-1, 0, 1]
  guard $ dx /= 0 || dy /= 0
  return (x+dx, y+dy)


-- output

boardPic :: Board -> Picture
boardPic board = uncurry translate (centeringTranslation $ bounds board) $
  pictures
    [ translate (cellScale * fromIntegral (x-1))
                (cellScale * fromIntegral (y-1))
      . scale cellScale cellScale $
      cellPic s <> borders
    | ((x, y), s) <- assocs board ]
  where
    (w, h) = over both fromIntegral $ snd $ bounds board
    borders = color black $ line [(0, 0), (1, 0)] <> line [(0, 0), (0, 1)]

centeringTranslation :: (Ind, Ind) -> (Float, Float)
centeringTranslation =
  over both ((*(-0.5*cellScale)) . fromIntegral) . snd

cellPic :: CellState -> Picture
cellPic s | s ^. open . to not  = color (greyN 0.5) $ polygon cellPath
          | s ^. mine  = color red $ polygon minePath
          | otherwise  = color white . numberPic $ s ^. neighbourMines

cellPath :: Path
cellPath = [(0, 0), (0, 1), (1, 1), (1, 0)]

minePath :: Path
minePath = [(0.5, 0.2), (0.2, 0.5), (0.5, 0.8), (0.8, 0.5)]

numberPic :: Int -> Picture
numberPic n = translate 0.3 0.2 . scale s s . text $ show n
  where s = 0.005
