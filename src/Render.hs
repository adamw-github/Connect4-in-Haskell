module Render(module Render) where
import Game
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.Gloss.Interface.IO.Interact
import Data.Char

{- Viewport dimensions -}
windowWidth :: Float
windowWidth = 1024.0

windowHeight :: Float
windowHeight = 768.0

-- Used to add padding around the edge of the window
buffer :: Float
buffer = 200.0

-- Q6 (Rendering the board)
render :: Board -> Picture
render b = drawGrid b <> drawCounters b

-- Renders the grid lines only
drawGrid :: Board -> Picture
drawGrid b = grid
    where
        numColsFloat = fromIntegral (numCols b)
        numRowsFloat = fromIntegral (numRows b)
        minDim = min ((windowWidth-buffer)/numColsFloat) ((windowHeight - buffer)/numRowsFloat)
        verticalLines = [line [ decentralise( minDim*n , 0 ) , decentralise( minDim*n, minDim *  numRowsFloat) ] | n <- [0..numColsFloat]]
        horizontalLines = [line [ decentralise( 0 , minDim*n ) , decentralise( minDim * numColsFloat, minDim*n) ] | n <- [0..numRowsFloat]]
        grid = color blue (pictures (verticalLines ++ horizontalLines))

-- Renders the counters only
drawCounters :: Board -> Picture
drawCounters b = counters
    where
        numColsInt = numCols b
        numColsFloat = fromIntegral numColsInt
        numRowsInt = numRows b
        numRowsFloat = fromIntegral numRowsInt

        minDim = min ((windowWidth-buffer)/numColsFloat) ((windowHeight - buffer)/numRowsFloat)

        counterTranslate (x,y) = translate x y

        reds    = [ counterTranslate (decentralise ((fromIntegral c *minDim + minDim/2),(fromIntegral r *minDim + minDim/2))) (color red    (circleSolid (minDim/2))) | r <- [0..numRowsInt-1], c <- [0..numColsInt-1], getCounter b r c == Just Red]
        yellows = [ counterTranslate (decentralise ((fromIntegral c *minDim + minDim/2),(fromIntegral r *minDim + minDim/2))) (color yellow (circleSolid (minDim/2))) | r <- [0..numRowsInt-1], c <- [0..numColsInt-1], getCounter b r c == Just Yellow]
        counters = pictures (reds ++ yellows)

-- Used to move the board away from the origin/centre of the window
decentralise :: Point -> Point
decentralise (x, y) = (x - windowWidth/2 + buffer/2 , y - windowHeight/2 + buffer/2)


-- Q7 (Game loop)
-- Example of how to use displayIO (similar to interactIO)
testPicture :: IO ()
testPicture =
    displayIO
        (InWindow "Connect 4" (floor windowWidth, floor windowHeight) (0, 0))
        white
        (return (render (newBoardUnsafe (newBoardUnsafe (newBoardUnsafe (emptyBoard 6 7) 0 Red) 0 Yellow) 5 Red)))
        (const $ return ())
        

-- New type for world which contains the board, current player, current input, and the winner (if there is one)
data World = World
    { worldBoard :: Board
    , worldPlayer :: Player
    , worldInput :: Int
    , worldWinner :: Maybe Player
    }

-- Renders the world. During the game, it also displays the current player and the current input. After the game, it displays the winner and a reset message.
renderWorld :: World -> IO Picture
renderWorld world = picture
    where
        p = render (worldBoard world)
        currentPlayer = translate (-windowWidth/2 + buffer/4) (windowHeight/2 - buffer/4) (scale 0.2 0.2 (color black (text ("Current Player:" ++ (show (worldPlayer world))) ) ))
        currentInput = translate (-windowWidth/2 + buffer/4) (windowHeight/2 - buffer/2.4) (scale 0.2 0.2 (color black (text ("Your column choice: " ++ (show (worldInput world)) ++ ", Press ENTER to drop a counter") ) ))
        resetText = translate (-windowWidth/2 + buffer/4) (windowHeight/2 - buffer/4) (scale 0.2 0.2 (color black (text "Press R to reset" ) ))
        currentWinner = case worldWinner world of
            Just winner -> translate 0 (windowHeight/2 - 50) (scale 0.4 0.4 (color black (text ((if winner == Red then "Red" else "Yellow") ++ " wins!"))))
            Nothing -> blank
        
        picture = case worldWinner world of
            Just _ -> return(p <>  resetText <> currentWinner)
            Nothing -> return(p <> currentPlayer <> currentInput)

--Event handler
handleInput :: Event -> World -> IO World
handleInput (EventKey (Char input) Up _ _) world = do
    if isDigit input then do
        let inputInt = fromEnum input -48
        if worldInput world == 0 then
            return World { worldBoard = worldBoard world, worldPlayer = worldPlayer world, worldInput = inputInt, worldWinner = worldWinner world}
        else
            return World { worldBoard = worldBoard world, worldPlayer = worldPlayer world, worldInput = (worldInput world)*10 + inputInt, worldWinner = worldWinner world}
    else if input == 'r' then
        return World { worldBoard = emptyBoard (numRows (worldBoard world)) (numCols (worldBoard world)), worldPlayer = Red, worldInput = 0, worldWinner = Nothing}
    else
        return world

handleInput (EventKey (SpecialKey KeyEnter) Up _ _ ) world = do
    let b = worldBoard world
    let player = worldPlayer world
    let input = worldInput world
    case dropCounter b input player of
        Just b' ->
            case checkWin b' of
                Just winner ->
                    return World { worldBoard = b', worldPlayer = togglePlayer player, worldInput = 0, worldWinner = Just winner}
                Nothing ->
                    return World { worldBoard = b', worldPlayer = togglePlayer player, worldInput = 0, worldWinner = worldWinner world}
        Nothing ->
            return World { worldBoard = b, worldPlayer = player, worldInput = 0, worldWinner = worldWinner world}

handleInput _ world = return world


gameLoop :: Int -> Int -> IO ()
gameLoop rows cols = do
    let world = World { worldBoard= emptyBoard rows cols, worldPlayer = Red, worldInput = 0, worldWinner = Nothing}
    interactIO
        (InWindow "Connect 4" (floor windowWidth, floor windowHeight) (0, 0))
        white
        world
        renderWorld
        handleInput
        (const $ return ())
