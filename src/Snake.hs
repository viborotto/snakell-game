module Snake
    ( Direction(..)
    , GameState(..)
    , initialGameState
    , changeDirection
    , move
    , checkGameOver
    , getNewFood
    , newGameGameState
    ) where

import Data.Map as Map
import System.Random

{-- ADT e TYPES --}

-- Define a direção do movimento da cobra
data Direction = UP | DOWN | LEFT | RIGHT
    deriving (Eq, Ord)

-- Define o tipo para representar a posição da comida
type Food = (Int, Int)

-- Define cada parte do corpo da cobra, com suas cordenadas 
type SnakeBody = (Int, Int)

-- Define o tipo para representar a cobra
type Snake = [SnakeBody]

-- Define os a estrutura de estados do jogo
data GameState = GameState
    { getSnake :: Snake
    , getFood :: Food
    , getDirection :: Direction
    , isGameOver :: Bool
    , getRandomStdGen :: StdGen
    , getScore :: Int
    , isNewGame :: Bool
    }

{--CONSTANTES--}
cols, rows :: Int
cols = 32
rows = 24

{--FUNCOES--}
-- Mapeia direções para vetores de deslocamento, o Map aqui funciona em um sistema de chave e valor
directionVectorMap :: Map Direction (Int, Int)
directionVectorMap = Map.fromList
    [ (UP, (0, -1))
    , (DOWN, (0, 1))
    , (LEFT, (-1, 0))
    , (RIGHT, (1, 0))
    ]

-- Define a mudança de direção no estado do jogo
changeDirection :: GameState -> Direction -> GameState
changeDirection (GameState snake food dir gameOver randomGen score isNewGame) newDir =
    GameState snake food newDir gameOver randomGen score isNewGame

-- move: responsável por atualizar a posição da cobra no jogo, com base na 
-- direção de movimento fornecida, e também verificar se a cobra comeu a comida presente na sua nova posição.
move :: Direction -> Food -> Snake -> (Bool, Snake)
move direction food snake
    | comeu        = (True, newSnake)
    | otherwise    = (False, newSnake)
  where
    (a, b) += (c, d) = (a + c, b + d)
    comeu = newHead == food
    newHead = (directionVectorMap ! direction) += head snake
    newSnake
        | comeu     = newHead : snake
        | otherwise = newHead : init snake

-- Verifica se o jogo terminou devido a colisão ou saída dos limites
checkGameOver :: Snake -> Bool
checkGameOver [] = True
checkGameOver (cabeca:cauda) =
    (cabecaX == 0 || cabecaX == cols) || (cabecaY == 0 || cabecaY == rows) || cabeca `elem` cauda
  where
    (cabecaX, cabecaY) = cabeca

-- Gera uma nova posição para a comida que não esteja na cobra
-- Se a posição da comida coincidir com alguma do corpo da cobra, a função é chamada novamente
getNewFood :: StdGen -> Snake -> (Food, StdGen)
getNewFood stdGen snake
    | (posX, posY) `elem` snake = getNewFood newGen2 snake
    | otherwise                 = ((posX, posY), newGen2)
  where
    (posX, newGen) = randomR (1, cols - 1) stdGen
    (posY, newGen2) = randomR (1, rows - 1) newGen


-- Define o estado inicial do jogo
initialGameState gameOver =
    GameState
        { getSnake = initialSnake
        , getFood = (3, 3)
        , getDirection = DOWN
        , isGameOver = gameOver
        , getRandomStdGen = mkStdGen 100
        , getScore = 0
        , isNewGame = True
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

newGameGameState =
    GameState
        { getSnake = initialSnake
        , getFood = (3, 3)
        , getDirection = DOWN
        , isGameOver = False  -- Inicialmente, o jogo não está encerrado
        , getRandomStdGen = mkStdGen 100
        , getScore = 0
        , isNewGame = True
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