{-# LANGUAGE TemplateHaskell #-}


import Graphics.Gloss
import Data.Array
import Control.Lens
import Control.Lens.TH
import Control.Monad
import Control.Monad.Random
import Data.Monoid ((<>))



type Ind = (Int, Int)

type Board = Array Ind CellState

data CellState = CellState
  { _open :: Bool
  , _mine :: Bool
  }

makeLenses ''CellState


blankBoard :: Ind -> Board
blankBoard size = listArray ((1, 1), size) $ repeat $ CellState False False


main :: IO ()
main = startBoard dimens 50 >>=
  display (InWindow "hi" (over both (*cellScale) dimens) (0, 0)) black
  . boardPic
  where dimens = (20, 10)

cellScale :: (Num a) => a
cellScale = 50


startBoard :: MonadRandom m => Ind -> Int -> m Board
startBoard size mines = mines `timesM` addMine $ blankBoard size

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


boardPic :: Board -> Picture
boardPic board = translate (-w*0.5*cellScale) (-h*0.5*cellScale) $
  pictures
    [ translate (cellScale * fromIntegral (x-1))
                (cellScale * fromIntegral (y-1))
      . scale cellScale cellScale $
      cellPic s <> borders
    | ((x, y), s) <- assocs board ]
  where
    (w, h) = over both fromIntegral $ snd $ bounds board
    borders = color black $ line [(0, 0), (1, 0)] <> line [(0, 0), (0, 1)]

cellPic :: CellState -> Picture
cellPic s | s ^. open . to not  = color (greyN 0.5) $ polygon cellPath
          | s ^. mine  = color red $ polygon minePath
          | otherwise  = blank

cellPath :: Path
cellPath = [(0, 0), (0, 1), (1, 1), (1, 0)]

minePath :: Path
minePath = [(0.5, 0.2), (0.2, 0.5), (0.5, 0.8), (0.8, 0.5)]
