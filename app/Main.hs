module Main (main) where

import Snake
import Graphics.Gloss
    ( black,
      red,
      yellow,
      blank,
      circleSolid,
      color,
      pictures,
      rectangleSolid,
      scale,
      text,
      translate,
      play,
      makeColorI,
      Display(InWindow),
      Color,
      Picture )
import Graphics.Gloss.Interface.Pure.Game

-- import Call -- Import the call library
-- import Codec.Audio.OggVorbis -- Import the module for handling Ogg Vorbis audio files

data GameMessage = GameOverMessage Picture | TryAgainMessage Picture | WelcomeMessage Picture

instance Semigroup GameMessage where
    (WelcomeMessage pic1) <> _ = WelcomeMessage pic1
    _ <> (WelcomeMessage pic2) = WelcomeMessage pic2
    (TryAgainMessage pic1) <> (TryAgainMessage pic2) = TryAgainMessage (pic1 <> pic2)
    (GameOverMessage pic1) <> (GameOverMessage pic2) = GameOverMessage (pic1 <> pic2)
    (TryAgainMessage pic1) <> (GameOverMessage pic2) = TryAgainMessage (pic1 <> pic2)
    (GameOverMessage pic1) <> (TryAgainMessage pic2) = TryAgainMessage (pic1 <> pic2)

instance Monoid GameMessage where
    mempty = WelcomeMessage blank

window :: Display
window = InWindow "Snakell Game" (840, 680) (200,200)

darkGreenColor :: Color
darkGreenColor = makeColorI 0 128 0 255


drawCircle :: Color -> (Float, Float) -> Float -> Picture
drawCircle cor (tx, ty) radius = color cor $
                                     translate (tx * 20 - 320) (ty * 20 - 240) $
                                     circleSolid radius

convertToPicture :: Color -> (Int, Int) -> Picture
convertToPicture cor (x, y) = drawCircle cor (toFloat (x, y)) 10
    where
        toFloat (coordX, coordY) = (fromIntegral coordX, fromIntegral coordY)

render :: GameState -> Picture
render gameState = pictures $ [drawHaskellLogo, cloud1, cloud2, cloud3, cloud4, cloud5] ++
                              shapesContornoJogo ++ (convertToPicture violet <$> snake) ++
                              [foodPicture] ++
                              [foldr mappend mempty gameMessage]

  where snake = getSnake gameState
        cloud1, cloud2, cloud3, cloud4, cloud5, drawHaskellLogo, foodPicture :: Picture
        cloud1 = translate (-100) 150 $ scale 0.4 0.4 $ color white $ pictures [ translate 0 20 $ circleSolid 20, translate 20 20 $ circleSolid 30, translate (-20) 20 $ circleSolid 30, translate 0 0 $ circleSolid 40, translate 40 0 $ circleSolid 30, translate (-40) 0 $ circleSolid 30]
        cloud2 = translate (-240) 50 $ scale 0.4 0.4 $ color white $ pictures [ translate 0 20 $ circleSolid 20, translate 20 20 $ circleSolid 30, translate (-20) 20 $ circleSolid 30, translate 0 0 $ circleSolid 40, translate 40 0 $ circleSolid 30, translate (-40) 0 $ circleSolid 30]
        cloud3 = translate 150 (200) $ scale 0.4 0.4 $ color white $ pictures [ translate 0 20 $ circleSolid 20, translate 20 20 $ circleSolid 30, translate (-20) 20 $ circleSolid 30, translate 0 0 $ circleSolid 40, translate 40 0 $ circleSolid 30, translate (-40) 0 $ circleSolid 30]
        cloud4 = translate (-200) (-200) $ scale 0.4 0.4 $ color white $ pictures [ translate 0 20 $ circleSolid 20, translate 20 20 $ circleSolid 30, translate (-20) 20 $ circleSolid 30, translate 0 0 $ circleSolid 40, translate 40 0 $ circleSolid 30, translate (-40) 0 $ circleSolid 30]
        cloud5 = translate 150 (-150) $ scale 0.4 0.4 $ color white $ pictures [ translate 0 20 $ circleSolid 20, translate 20 20 $ circleSolid 30, translate (-20) 20 $ circleSolid 30, translate 0 0 $ circleSolid 40, translate 40 0 $ circleSolid 30, translate (-40) 0 $ circleSolid 30]
        drawHaskellLogo = color violet $ translate (-25) 270 $ scale 0.2 0.2 $ text ">>="
        foodPicture = pictures [convertToPicture red food, translateFoodText]
            where
                (foodX, foodY) = getFood gameState
                foodText = "<*>"  -- texto para dentro do circulo da comida
                (circleTx, circleTy) = (fromIntegral foodX * 20 - 320, fromIntegral foodY * 20 - 240)
                textWidth = 17
                textHeight = 8
                (textTx, textTy) = (circleTx - textWidth / 2, circleTy - textHeight / 2)
                translateFoodText = translate textTx textTy $ scale 0.07 0.07 $ color white $ text foodText

        food = getFood gameState
        shapesContornoJogo = [ fillRectangle darkGreenColor (16, 0) (640, 20)
                 , fillRectangle darkGreenColor (16, 24) (640, 20)
                 , fillRectangle darkGreenColor (0, 12) (20, 480)
                 , fillRectangle darkGreenColor (32, 12) (20, 480) ]
        fillRectangle cor (tx, ty) (w, h) = color cor $
                                                scale 1 (-1) $
                                                translate (tx * 20 - 320) (ty * 20 - 240) $
                                                rectangleSolid w h
        gameMessage
          | isGameOver gameState = gameOverMessage
          | isNewGame gameState = welcomeMessage
          | otherwise = []

        gameOverMessage = if isGameOver gameState
                          then [color black $
                                translate (-200) 0 $
                                scale 0.5 0.5 $
                                text "GAME OVER",
                                color black $
                                translate (-175) (-50) $
                                scale 0.2 0.2 $
                                text "PRESS SPACE TO TRY AGAIN.",
                                color (makeColorI 0 128 0 255) $
                                translate (-175) (-100) $
                                scale 0.3 0.3 $
                                text ("Score: " ++ show (getScore gameState)),
                                color (makeColorI 0 128 0 255) $
                                translate (-175) (-150) $
                                scale 0.2 0.2 $
                                text ("Best Score: " ++ show (getBestScore gameState))]
                        else []

        welcomeMessage = [color darkGreenColor $
                        translate (-150) 300 $
                        scale 0.16 0.16 $
                        text "David Pianura | Vittoria Borotto",
                        color black $
                        translate (-200) 10 $
                        scale 0.3 0.3 $
                        text "WELCOME TO SNAKELL",
                        color black $
                        translate (-175) (-50) $
                        scale 0.2 0.2 $
                        text "PRESS SPACE TO PLAY"]


update :: Float -> GameState -> GameState
update _ gameState
    | isNewGame gameState = gameState
    | gameOver = gameState
    | otherwise = gameState
        { getSnake = newSnake
        , getFood = newFood'
        , isGameOver = newGameOver
        , getRandomStdGen = newStdGen
        , getScore = newScore
        , getBestScore = newBestScore
        }
    where
        snake = getSnake gameState
        food = getFood gameState
        direction = getDirection gameState
        gameOver = isGameOver gameState
        stdGen = getRandomStdGen gameState
        (wasFoodEaten, newSnake) = move direction food snake
        (newFood, newStdGen) = getNewFood stdGen newSnake
        newFood' = if wasFoodEaten
                   then newFood
                   else food
        newGameOver = checkGameOver newSnake
        newScore = if wasFoodEaten
                   then getScore gameState + 1
                   else getScore gameState
        newBestScore = if newScore > getBestScore gameState
                       then newScore
                       else getBestScore gameState
        newGameState = GameState newSnake newFood' direction newGameOver newStdGen newScore


-- https://hackage.haskell.org/package/gloss-1.1.1.0/docs/Graphics-Gloss-Game.html
-- EventKey Key KeyState Modifiers (Float, Float)
handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (SpecialKey KeyLeft) Down _ _) gameState
    | getDirection gameState /= RIGHT = changeDirection gameState LEFT
    | otherwise                       = gameState

handleKeys (EventKey (SpecialKey KeyRight) Down _ _) gameState
    | getDirection gameState /= LEFT = changeDirection gameState RIGHT
    | otherwise                      = gameState

handleKeys (EventKey (SpecialKey KeyUp) Down _ _) gameState
    | getDirection gameState /= UP = changeDirection gameState DOWN
    | otherwise                    =  gameState

handleKeys (EventKey (SpecialKey KeyDown) Down _ _) gameState
    | getDirection gameState /= DOWN = changeDirection gameState UP
    | otherwise                      =  gameState

handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gameState
    | isNewGame gameState = gameState { isNewGame = False }
    -- funcao de alta ordem max entre os scores
    | isGameOver gameState = initialGameState False $ max (getBestScore gameState) (getScore gameState)
    | otherwise = gameState

handleKeys _ gameState = gameState

background :: Color
background = makeColorI 135 206 235 255

main :: IO ()
main = do
    -- Load the audio file
    -- audioFile <- oggFile "toystory.ogg" -- Replace with the actual path to your audio file

    -- Initialize the call library
    -- initializeCall

    -- Play the audio file in a loop
    -- playLoop audioFile
    let initialBestScore = 0  -- Set your initial best score here
    play window background 10 (newGameGameState initialBestScore) render handleKeys update

