{-# LANGUAGE TemplateHaskell #-}


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Array
import qualified Data.Set as Set
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

data GameState = GameState
  { _board :: Board
  , _alive :: Bool
  }

makeLenses ''GameState


cellScale :: (Num a) => a
cellScale = 50


main :: IO ()
main = do
  board0 <- startBoard dimens 40
  play
    (InWindow "hi" (over both (*cellScale) dimens) (0, 0))
    black
    30
    (GameState board0 True)
    boardPic
    (guardAlive . handleEvent)
    (const id)
  where dimens = (20, 10)

handleEvent :: Event -> GameState -> GameState
handleEvent (EventKey (MouseButton button) Down _ (x, y)) =
  handleClick button (x, y)
handleEvent _ = id

guardAlive :: (GameState -> GameState) -> GameState -> GameState
guardAlive f gs | gs ^. alive  = f gs
                | otherwise    = gs

handleClick :: MouseButton -> (Float, Float) -> GameState -> GameState
handleClick button (x0, y0) = execState $ do
  bounds <- use (board . to bounds)
  let (ctx, cty) = centeringTranslation bounds
      (x, y) = over both ((+1) . floor . (/cellScale)) (x0-ctx, y0-cty)
      (validButton, mineExpected) = case button of
        LeftButton  -> (True, False)
        RightButton -> (True, True)
        _           -> (False, False)
  when (validButton && inRange bounds (x, y)) $ do
    openCell mineExpected (x, y)
    modify $ guardAlive (over board $ openAll easyNoMines)

openCell :: Bool -> Ind -> State GameState ()
openCell mineExpected i = do
  closedCell <- use $ board . ix i . open . to not . to Any
  when (getAny closedCell) $ do
    board . ix i . open .= True
    mine <- use $ board . ix i . mine . to Any
    when (getAny mine /= mineExpected) $
      alive .= False


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
    [ (i, cs & neighbourMines .~ neighbourCells (^. mine) board i)
    | (i, cs) <- assocs board ]

neighbourCells :: (CellState -> Bool) -> Board -> Ind -> Int
neighbourCells p board =
  length . filter (\i -> getAny $ board ^. ix i . (to p) . to Any)
  . neighbours

neighbours :: Ind -> [Ind]
neighbours (x, y) = do
  dx <- [-1, 0, 1]
  dy <- [-1, 0, 1]
  guard $ dx /= 0 || dy /= 0
  return (x+dx, y+dy)


-- automated easy moves

easyNoMines :: Board -> Set.Set Ind
easyNoMines b = Set.fromList $ do
  i0 <- range $ bounds b
  guard $ getAny $ b ^. ix i0 . open . to Any
  guard $ not . getAny $ b ^. ix i0 . mine . to Any
  let openMine cs = cs ^. open && cs ^. mine in
    guard $ b ! i0 ^. neighbourMines == neighbourCells openMine b i0
  i1 <- neighbours i0
  guard $ getAny $ b ^. ix i1 . open . to not . to Any
  return i1

openAll :: (Board -> Set.Set Ind) -> Board -> Board
openAll finder b
  | Set.null found = b
  | otherwise      = openAll finder $ Set.fold setOpen b found
  where
    found = finder b
    setOpen i b' = b' & ix i . open .~ True


-- output

boardPic :: GameState -> Picture
boardPic gs = uncurry translate (centeringTranslation $ bounds b) $
  pictures
    [ translate (cellScale * fromIntegral (x-1))
                (cellScale * fromIntegral (y-1))
      . scale cellScale cellScale $
      cellPic s <> borders
    | ((x, y), s) <- assocs b]
  where
    b = gs ^. board
    (w, h) = over both fromIntegral $ snd $ bounds b
    borders = color bordersColor $
      line [(0, 0), (1, 0)] <> line [(0, 0), (0, 1)]
    bordersColor = case gs ^. alive of
      True  -> greyN 0.2
      False -> red

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
