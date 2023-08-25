module Snake
    ( Direction(..)
    , GameState(..)
    , initialGameState
    , changeDirection
    , move
    , checkGameOver
    , generateNewFood
    ) where

import Data.Map as Map
import System.Random

-- Define a direção do movimento da cobra
data Direction = UP | DOWN | LEFT | RIGHT
    deriving (Eq, Ord)

-- Define o tipo para representar a posição da comida
type Food = (Int, Int)

-- Define cada parte do corpo da cobra, com suas cordenadas 
type SnakeBody = (Int, Int)
-- Define o tipo para representar a cobra
type Snake = [SnakeBody]

-- Definições de constantes para as dimensões do jogo
cols, rows :: Int
cols = 32
rows = 24

-- Mapeia direções para vetores de deslocamento
directionVectorMap :: Map Direction (Int, Int)
directionVectorMap = Map.fromList
    [ (UP, (0, -1))
    , (DOWN, (0, 1))
    , (LEFT, (-1, 0))
    , (RIGHT, (1, 0))
    ]

-- Move a cobra de acordo com a direção e a posição da comida
move :: Direction -> Food -> Snake -> (Bool, Snake)
move direction food snake =
    if wasFoodEaten
        then (True, newHead : snake)
        else (False, newHead : init snake)
  where
    wasFoodEaten = newHead == food
    newHead = directionVectorMap ! direction +: head snake
    (a, b) +: (c, d) = (a + c, b + d)

-- Verifica se o jogo terminou devido a colisão ou saída dos limites
checkGameOver :: Snake -> Bool
checkGameOver [] = True
checkGameOver snake =
    headX == 0 || headX == cols ||
    headY == 0 || headY == rows ||
    head' `elem` tail'
  where
    (headX, headY) = head snake
    (head':tail') = snake

-- Gera uma nova posição para a comida que não esteja na cobra
generateNewFood :: StdGen -> Snake -> (Food, StdGen)
generateNewFood stdGen snake =
    if newFood `elem` snake
        then generateNewFood stdGen3 snake
        else ((foodX, foodY), stdGen3)
  where
    (foodX, stdGen2) = randomR (1, cols - 1) stdGen
    (foodY, stdGen3) = randomR (1, rows - 1) stdGen2
    newFood = (foodX, foodY)



-- Define a mudança de direção no estado do jogo
changeDirection :: GameState -> Direction -> GameState
changeDirection gameState newDir =
    gameState { getDirection = newDir }

-- Define os a estrutura de estados do jogo
data GameState = GameState
    { getSnake :: Snake
    , getFood :: Food
    , getDirection :: Direction
    , isGameOver :: Bool
    , getRandomStdGen :: StdGen
    , getScore :: Int
    }

-- Define o estado inicial do jogo
initialGameState :: Bool -> GameState
initialGameState gameOver =
    GameState
        { getSnake = initialSnake
        , getFood = (3, 3)
        , getDirection = DOWN
        , isGameOver = gameOver
        , getRandomStdGen = mkStdGen 100
        , getScore = 0
        }
  where
    snakeX = cols `div` 2
    snakeY = rows `div` 2
    initialSnake =
        [ (snakeX, snakeY)
        , (snakeX, snakeY - 1)
        , (snakeX, snakeY - 2)
        , (snakeX - 1, snakeY - 2)
        , (snakeX - 2, snakeY - 2)
        ]
