module Main (main) where

import Snake
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data GameMessage = GameOverMessage Picture | TryAgainMessage Picture

instance Semigroup GameMessage where
    (TryAgainMessage pic1) <> (TryAgainMessage pic2) = TryAgainMessage (pic1 <> pic2)
    (GameOverMessage pic1) <> (GameOverMessage pic2) = GameOverMessage (pic1 <> pic2)
    (TryAgainMessage pic1) <> (GameOverMessage pic2) = TryAgainMessage (pic1 <> pic2)
    (GameOverMessage pic1) <> (TryAgainMessage pic2) = TryAgainMessage (pic1 <> pic2)

instance Monoid GameMessage where
    mempty = GameOverMessage blank

window :: Display
window = InWindow "Snakell Game" (640, 480) (100, 100)
        -- FullScreen

--Peguei Bola, desenhaBola, criaBola do VIDEO DE GLOSS PROFESSOR EMILIO
          --((x,y), (vx, vy), p)

pipemarioColor :: Color
pipemarioColor = makeColorI 0 128 0 255

loadSnakeImage :: IO Picture
loadSnakeImage = loadBMP "star.bmp"

drawCircle :: Color -> (Float, Float) -> Float -> Picture
drawCircle cor (tx, ty) radius = color cor $
                                     translate (tx * 20 - 320) (ty * 20 - 240) $
                                     circleSolid radius

-- tentativa de desenhar um circulo com imagem
drawCircleImage :: Picture -> (Float, Float) -> Float -> Picture
drawCircleImage img (tx, ty) radius = translate (tx * 20 - 320) (ty * 20 - 240) $ 
    pictures
        [ img
        , circle (radius * 20) -- Multiplica o raio pelo fator de escala
        ]

loadMushroomImage :: IO Picture
loadMushroomImage = loadBMP "mushroom.bmp"

-- TODO: FAZER UMA FUNCAO QUE DESENHE O CIRCULO COM A IMAGEM DA COGUMELO COMO COMIDA
-- PROBLEMA: loadMushRoom devolve IO Picture e nao Picture
-- convertToPictureMushroom :: Picture -> (Int, Int) -> Picture
-- convertToPictureMushroom img (x, y) = drawCircleImage loadMushroomImage (toFloat (x, y)) 0.5
--     where
--         toFloat (x, y) = (fromIntegral x, fromIntegral y)

convertToPicture :: Color -> (Int, Int) -> Picture
convertToPicture cor (x, y) = drawCircle cor (toFloat (x, y)) 10
    where
        toFloat (coordX, coordY) = (fromIntegral coordX, fromIntegral coordY)

render :: GameState -> Picture
render gameState = pictures $ shapesContornoJogo ++ (convertToPicture yellow <$> snake) ++
                              fmap (convertToPicture red) [food] ++
                              [foldr mappend mempty gameOverMessage]
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

update :: Float -> GameState -> GameState
update _ gameState
    | gameOver = gameState
    | otherwise = newGameState
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

handleKeys (EventKey (SpecialKey KeySpace) Down _ _) gameState
    | isGameOver gameState = initialGameState False
    | otherwise            = gameState
        
handleKeys _ gameState = gameState

loadBackgroundImage :: IO Picture
loadBackgroundImage = loadBMP "sky.bmp"

background :: Color
background = makeColorI 135 206 235 255

main :: IO ()
main = do
    play window background 10 (initialGameState True) render handleKeys update
    
    