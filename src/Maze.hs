module Maze where

import Data.Monoid
import Graphics.Gloss.Interface.Pure.Game
import System.Random

import Maze.Space

-- | Запустить демонстрацию с лабиринтом.
demo :: Maze -> IO ()
demo maze = play display bgColor fps (initGame maze) drawGame handleGame updateGame
  where
    display = InWindow "Лабиринты" (1000, 1000) (200, 200)
    bgColor = voidColor -- цвет фона
    fps     = 60        -- кол-во кадров в секунду

-- | Состояние игры.
data Game = Game
  { gameMaze   :: Maze    -- ^ Весь лабиринт.
  , gamePlayer :: Coords  -- ^ Положение игрока.
  }

-- | Начальное состояние игры.
initGame :: Maze -> Game
initGame maze = Game
  { gameMaze   = withBorder maze
  , gamePlayer = (0, 0)
  }

-- | Отрисовка игры.
drawGame :: Game -> Picture
drawGame game = scale 20 20 (pictures
  [ foldMap drawBrick (crop (playerArea (gamePlayer game)) (gameMaze game))
  , fadeAt (gamePlayer game)
  , drawPlayer (gamePlayer game)
  ])

-- | Минимальная прямоугольная область, вмещающая поле зрения игрока.
playerArea :: Coords -> Area
playerArea (i, j) = Area (i - fadeRadius, j - fadeRadius) (i + fadeRadius, j + fadeRadius)

-- | Радиус видимости, определяющий поле зрения игрока (в ячейках).
fadeRadius :: Num a => a
fadeRadius = 8

-- | Отобразить плавное затемнение вокруг игрока.
fadeAt :: Coords -> Picture
fadeAt (i, j) = translate (fromIntegral i) (fromIntegral j) (scale fadeRadius fadeRadius fade)

-- | Плавное затемнение с радиусом 1.
fade :: Picture
fade = foldMap fadeRing [0..n] <> outerVoid
  where
    n = 100
    fadeRing r = color (withAlpha ((r / n)^5) voidColor)
      (thickCircle (r / n) (10 / n))
    outerVoid = color voidColor (thickCircle 1.5 1)

-- | Цвет пустоты (невидимой для игрока зоны).
voidColor :: Color
voidColor = black

-- | Отобразить игрока.
drawPlayer :: Coords -> Picture
drawPlayer (i, j) = translate (fromIntegral i) (fromIntegral j) (color magenta (thickCircle 0.2 0.4))

-- | Отобразить ячейку стены лабиринта.
drawBrick :: Brick -> Picture
drawBrick (i, j) = translate (fromIntegral i) (fromIntegral j)
  (color orange (scale 0.45 0.45 (polygon [ (-1, -1), (1, -1), (1, 1), (-1, 1) ])))

-- | Внешняя граница лабиринта.
borderMaze :: Area -> Maze
borderMaze area = fromCoordsList ( concat
  [ map (\y -> (l - 1, y)) [b-1..t+1]
  , map (\y -> (r + 1, y)) [b-1..t+1]
  , map (\x -> (x, b - 1)) [l-1..r+1]
  , map (\x -> (x, t + 1)) [l-1..r+1]
  ] )
  where
    Area (l, b) (r, t) = area

-- | Добавить лабиринту внешнюю границу.
withBorder :: Maze -> Maze
withBorder maze = maze <> foldMap borderMaze (spaceArea maze)

-- | Обработка событий игры.
handleGame :: Event -> Game -> Game
handleGame (EventKey (SpecialKey KeyLeft)  Down _ _) = movePlayer (-1,  0)
handleGame (EventKey (SpecialKey KeyRight) Down _ _) = movePlayer ( 1,  0)
handleGame (EventKey (SpecialKey KeyUp)    Down _ _) = movePlayer ( 0,  1)
handleGame (EventKey (SpecialKey KeyDown)  Down _ _) = movePlayer ( 0, -1)
handleGame _ = id

-- | Переместить игрока на заданный вектор.
movePlayer :: (Int, Int) -> Game -> Game
movePlayer (dx, dy) game
  | canMove from to (gameMaze game) = game { gamePlayer = to }
  | otherwise = game
  where
    from@(i, j) = gamePlayer game
    to   = (i + dx, j + dy)

-- | Может ли игрок переместиться из одной точки в другую в лабиринте?
canMove :: Coords -> Coords -> Maze -> Bool
canMove from to maze = True

-- | Обновление игры.
-- Поскольку все обновления происходят по событиям, эта функция ничего не делает.
updateGame :: Float -> Game -> Game
updateGame _ = id

-- | Ячейка стены лабиринта.
type Brick = Coords

-- | Лабиринт — это пространство с ячейками стен.
type Maze = Space Brick

-- | Стена — это тоже пространство с ячейками стен.
type Wall = Space Brick

-- | Транспонировать лабиринт.
transposeMaze :: Maze -> Maze
transposeMaze = fmap swap . transposeSpace

-- | Сгенерировать случайные координаты для вертикальной стены.
-- Координата x указывает положение стены, а координата y — положение прохода в стене.
-- Чтобы стены не склеивались и лабиринт оставался связным,
-- стены всегда создаются на нечётных позициях, а проходы — на чётных.
randomWallCoords :: RandomGen g => Area -> g -> (Coords, g)
randomWallCoords (Area (l, b) (r, t)) g = ((2 * i + 1, 2 * j), g'')
  where
    (i, g')  = randomR (l `div` 2, (r - 1) `div` 2) g
    (j, g'') = randomR ((b + 1) `div` 2, t `div` 2) g'

-- | Создать вертикальную стену с заданными координатами.
-- Координата x указывает положение стены, а координата y — положение прохода в стене.
mkVerticalWall :: Coords -> Area -> Maze
mkVerticalWall (i, j) (Area (_, b) (_, t)) = fromCoordsList
  [ (i, y) | y <- [b..t], y /= j ]

-- | Сгенерировать случайный лабиринт в заданной прямоугольной области.
-- Генерация происходит при помощи рекурсивного разделения области
-- стенами с одним проходом.
genMaze :: RandomGen g => Area -> g -> Maze
genMaze area g
  | thinArea area = mempty { spaceArea = Just area }
  | otherwise     = wall <> leftMaze <> rightMaze
  where
    ((i, j), g')  = randomWallCoords area g
    wall          = mkVerticalWall (i, j) area
    (left, right) = splitH i area

    -- для левой и правой подобластей мы вызываем genMaze рекурсивно,
    -- транспонируя обе подобласти, а затем транспонируя обратно
    -- полученные подлабиринты
    (gl, gr) = split g'
    leftMaze  = transposeMaze (genMaze (transposeArea left)  gl)
    rightMaze = transposeMaze (genMaze (transposeArea right) gr)
