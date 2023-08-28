import Test.QuickCheck
import Test.QuickCheck.Monadic
import Snake

import Test.Hspec
import Test.QuickCheck
import Snake -- Import your Snake module
import System.Random

instance Show Direction where
  show UP = "UP"
  show DOWN = "DOWN"
  show LEFT = "LEFT"
  show RIGHT = "RIGHT"
  
main :: IO ()
main = hspec $ do
        describe "changeDirection" $ do
            it "changes the direction in the game state" $ do
                let gameState = initialGameState False
                    newDir = UP
                    updatedGameState = changeDirection gameState newDir
                getDirection updatedGameState `shouldBe` newDir

        describe "checkGameOver" $ do
            it "returns True when the snake hits the wall" $ do
                let snake = [(0, 0)]
                checkGameOver snake `shouldBe` True

            it "returns False when the snake is not hitting the wall" $ do
                let snake = [(1, 1)]
                checkGameOver snake `shouldBe` False

        describe "getNewFood" $ do
            it "generates a new food position" $ do
                let stdGen = mkStdGen 42
                    snake = [(2, 2), (2, 3), (2, 4)] -- Snake positions to avoid
                    (food, _) = getNewFood stdGen snake
                food `shouldNotBe` (2, 2) -- Should not be in snake positions
        
        describe "move" $ do
            it "increases the snake's length by 1 when it eats food" $ do
                let snake = [(2, 2)]
                    food = (3, 2)
                    (ateFood, newSnake) = move RIGHT food snake
                ateFood `shouldBe` True
                length newSnake `shouldBe` 2
        
        describe "newGameGameState" $ do
            it "returns a valid initial game state" $ do
                let gameState = newGameGameState
                    initialSnake =
                        [ (snakeX, snakeY)
                        , (snakeX - 1, snakeY)
                        , (snakeX - 2, snakeY)
                        , (snakeX - 3, snakeY)
                        , (snakeX - 4, snakeY)
                        ]
                    snakeX = cols `div` 2
                    snakeY = rows `div` 2
                getSnake gameState `shouldBe` initialSnake
                getFood gameState `shouldBe` (25, 10)
                getDirection gameState `shouldBe` DOWN
                isGameOver gameState `shouldBe` False
                getScore gameState `shouldBe` 0
                isNewGame gameState `shouldBe` True
