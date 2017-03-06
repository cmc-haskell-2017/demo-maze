{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
module Maze where

import Data.Monoid ((<>))
import Graphics.Gloss.Interface.Pure.Game
import System.Random

demo :: IO ()
demo = play display bgColor fps initGame drawGame handleGame updateGame
  where
    display = InWindow "Лабиринты" (500, 500) (200, 200)
    bgColor = black   -- цвет фона
    fps     = 60      -- кол-во кадров в секунду

type Game = Maze

initGame :: Game
initGame = genMaze gameArea (mkStdGen 1)

gameArea :: Area
gameArea = Area (-23, -23) (23, 23)

drawGame :: Game -> Picture
drawGame = scale 10 10 . foldMap drawBrick . mappend (borderMaze gameArea)

borderMaze :: Area -> Maze
borderMaze area = Space
  { spaceObjects = concat
      [ map (l-1,) [b-1..t+1]
      , map (r+1,) [b-1..t+1]
      , map (,b-1) [l-1..r+1]
      , map (,t+1) [l-1..r+1]
      ]
  , spaceArea = Just area
  }
  where
    Area (l, b) (r, t) = area

handleGame :: Event -> Game -> Game
handleGame _ = id

updateGame :: Float -> Game -> Game
updateGame _ = id

drawBrick :: Brick -> Picture
drawBrick (i, j) = translate (fromIntegral i) (fromIntegral j)
  (color orange (scale 0.45 0.45 (polygon [ (-1, -1), (1, -1), (1, 1), (-1, 1) ])))


type Coords = (Int, Int)

data Area = Area Coords Coords
  deriving (Show)

maxArea :: Area -> Area -> Area
maxArea (Area (l1, b1) (r1, t1)) (Area (l2, b2) (r2, t2))
  = Area (min l1 l2, min b1 b2) (max r1 r2, max t1 t2)

-- | Проверить, что точка находится в заданной области.
inArea :: Area -> Coords -> Bool
inArea (Area (l, b) (r, t)) (i, j) = and
  [ l <= i && i <= r
  , b <= j && j <= t
  ]

data Space a = Space
  { spaceObjects :: [a]
  , spaceArea    :: Maybe Area
  } deriving (Show, Functor, Foldable)

instance Monoid (Space a) where
  mempty = Space [] Nothing
  Space xs ax `mappend` Space ys ay = Space (xs ++ ys) (maxArea' ax ay)
    where
      maxArea' Nothing a2 = a2
      maxArea' a1 Nothing = a1
      maxArea' (Just a1) (Just a2) = Just (maxArea a1 a2)

-- | Оставить в пространстве только указанную область.
crop :: Area -> Maze -> Maze
crop area space = Space
  { spaceObjects = filter (inArea area) (spaceObjects space)
  , spaceArea    = Just area
  }

type Brick = Coords

type Maze = Space Brick

mkWall :: Area -> Maze
mkWall area = Space
  { spaceObjects = [ (i, j) | i <- [l..r], j <- [b..t] ]
  , spaceArea = Just area
  }
  where
    Area (l, b) (r, t) = area

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

transposeArea :: Area -> Area
transposeArea (Area lb rt) = Area (swap lb) (swap rt)

transposeMaze :: Maze -> Maze
transposeMaze (Space objects area) = Space
  (fmap swap objects)
  (fmap transposeArea area)

splitH :: Int -> Area -> Split Int
splitH x (Area (l, b) (r, t)) = Split left x right
  where
    left  = Area (l, b) (x - 1, t)
    right = Area (x + 1, b) (r, t)

splitV :: Int -> Area -> Split Int
splitV y area = Split (transposeArea bottom) y (transposeArea top)
  where
    Split bottom _ top = splitH y (transposeArea area)

data Split a = Split Area a Area
  deriving (Functor)

randomSplitH :: RandomGen g => g -> Area -> (Split Int, g)
randomSplitH g area = (splitH (2 * x) area, g')
  where
    (x, g') = randomR ((l + 1) `div` 2, (r - 1) `div` 2) g
    Area (l, _) (r, _) = area

randomSplitWall :: RandomGen g => g -> Area -> (Split Int, g)
randomSplitWall g area = (splitV (2 * y + 1) area, g')
  where
    (y, g') = randomR (b `div` 2, t `div` 2) g
    Area (_, b) (_, t) = area

randomWallsH :: RandomGen g => g -> Area -> (Split Maze, g)
randomWallsH g area = (Split left (mkWall top <> mkWall bottom) right, g'')
  where
    Area (_, b) (_, t) = area
    wallsArea = Area (x, b) (x, t)
    (Split left x right,  g')  = randomSplitH g area
    (Split top  _ bottom, g'') = randomSplitWall g' wallsArea

thinArea :: Area -> Bool
thinArea (Area (l, b) (r, t)) = r <= l + 1 || t <= b + 1

genMaze :: RandomGen g => Area -> g -> Maze
genMaze area g
  | thinArea area = mempty
  | otherwise     = leftMaze <> walls <> rightMaze
  where
    (Split left walls right, g') = randomWallsH g area
    (gl, gr) = split g'
    leftMaze  = transposeMaze (genMaze (transposeArea left)  gl)
    rightMaze = transposeMaze (genMaze (transposeArea right) gr)

