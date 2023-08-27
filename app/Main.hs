module Main (main) where

import Snake
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

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
window = InWindow "Snakell Game" (640, 480) (100, 100)

pipemarioColor :: Color
pipemarioColor = makeColorI 0 128 0 255

loadSnakeImage :: IO Picture
loadSnakeImage = loadBMP "star.bmp"

drawCircle :: Color -> (Float, Float) -> Float -> Picture
drawCircle cor (tx, ty) radius = color cor $
                                     translate (tx * 20 - 320) (ty * 20 - 240) $
                                     circleSolid radius

convertToPicture :: Color -> (Int, Int) -> Picture
convertToPicture cor (x, y) = drawCircle cor (toFloat (x, y)) 10
    where
        toFloat (coordX, coordY) = (fromIntegral coordX, fromIntegral coordY)

-- render :: GameState -> Picture
-- render gameState = pictures $ shapesContornoJogo ++ (convertToPicture yellow <$> snake) ++
--                               fmap (convertToPicture red) [food] ++
--                               [foldr mappend mempty gameOverMessage]
--   where snake = getSnake gameState
--         food = getFood gameState
--         shapesContornoJogo = [ fillRectangle pipemarioColor (16, 0) (640, 20)
--                  , fillRectangle pipemarioColor (16, 24) (640, 20)
--                  , fillRectangle pipemarioColor (0, 12) (20, 480)
--                  , fillRectangle pipemarioColor (32, 12) (20, 480) ]
--         fillRectangle cor (tx, ty) (w, h) = color cor $
--                                                 scale 1 (-1) $
--                                                 translate (tx * 20 - 320) (ty * 20 - 240) $
--                                                 rectangleSolid w h
        -- gameOverMessage = if isGameOver gameState
        --                   then [color black $
        --                         translate (-200) 0 $
        --                         scale 0.5 0.5 $
        --                         text "GAME OVER",
        --                         color black $
        --                         translate (-175) (-50) $
        --                         scale 0.2 0.2 $
        --                         text "PRESS SPACE TO TRY AGAIN.",
        --                         color (makeColorI 0 128 0 255) $
        --                         translate (-175) (-100) $
        --                         scale 0.3 0.3 $
        --                         text ("Score: " ++ show (getScore gameState))]
        --                   else []


render :: GameState -> Picture
render gameState = pictures $ shapesContornoJogo ++ (convertToPicture yellow <$> snake) ++
                              fmap (convertToPicture red) [food] ++
                              [foldr mappend mempty gameMessage]

  where snake = getSnake gameState
        food = getFood gameState
        shapesContornoJogo = [ fillRectangle pipemarioColor (16, 0) (640, 20)
                 , fillRectangle pipemarioColor (16, 24) (640, 20)
                 , fillRectangle pipemarioColor (0, 12) (20, 480)
                 , fillRectangle pipemarioColor (32, 12) (20, 480) ]
        fillRectangle cor (tx, ty) (w, h) = color cor $
                                                scale 1 (-1) $
                                                translate (tx * 20 - 320) (ty * 20 - 240) $
                                                rectangleSolid w h
        
        gameMessage = if isGameOver gameState
                  then gameOverMessage
                  else if isNewGame gameState
                       then welcomeMessage
                       else []

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
                                text ("Score: " ++ show (getScore gameState))]
                        else []

        welcomeMessage = [color black $
                        translate (-200) 0 $
                        scale 0.2 0.2 $
                        text "WELCOME TO SNAKELL",
                        color black $
                        translate (-175) (-50) $
                        scale 0.2 0.2 $
                        text "PRESS SPACE TO PLAY"]

        isNewGame = not . isGameOver

update :: Float -> GameState -> GameState
update _ gameState
    | isNewGame gameState = gameState
    | gameOver = gameState
    | otherwise = newGameState (isNewGame gameState) -- Pass the isNewGame field as an argument
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

-- handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gameState
--     | isGameOver gameState = initialGameState False
--     | otherwise            = gameState

handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gameState
    | isNewGame gameState = gameState { isNewGame = False }
    | isGameOver gameState = initialGameState False
    | otherwise = gameState

handleKeys _ gameState = gameState

loadBackgroundImage :: IO Picture
loadBackgroundImage = loadBMP "sky.bmp"

background :: Color
background = makeColorI 135 206 235 255

main :: IO ()
main = do
    play window background 10 newGameGameState render handleKeys update
    
    