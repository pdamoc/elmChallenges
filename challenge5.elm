import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug
import Random 
import Text exposing (fromString, Text, monospace, bold)

-- MODEL

type Direction = Left | Right | Up | Down 


type Status = Start | Active | Paused | End

snakeSpeed = 5 

type alias Keys = { x:Int, y:Int }

type alias Model =
  { body : List (Int, Int)
  , newDir : Direction
  , dir : Direction
  , status : Status
  , cherry : Maybe (Int, Int)
  , delta : Time
  }

pitWidth = 40
pitHeight = 20
pitBlock = 20

startSnake : Model
startSnake = 
  { body = [ (pitWidth//2, pitHeight//2)
            , (pitWidth//2-1, pitHeight//2)
            , (pitWidth//2-2, pitHeight//2)
            ]
  , newDir = Down
  , dir = Down
  , status = Start
  , cherry = Nothing
  , delta = 0
  }


-- UPDATE

toggleGame : Status -> Status
toggleGame status = 
  case status of 
    End -> Active
    Paused -> Active
    Start -> Active
    Active -> Paused

updateDelta : Time -> Model -> Model
updateDelta t snake =
  let 
    newDelta = snake.delta + t
  in 
    if newDelta > 20/snakeSpeed 
    then {snake | delta <- 0}
    else {snake | delta <- newDelta}

updateDirection : Keys -> Model -> Model
updateDirection keys snake = 
  let 
    newDir = if  
      | keys.x < 0 && (snake.dir == Up || snake.dir == Down) -> Left
      | keys.x > 0 && (snake.dir == Up || snake.dir == Down) -> Right
      | keys.y > 0 && (snake.dir == Left || snake.dir == Right) -> Up
      | keys.y < 0 && (snake.dir == Left || snake.dir == Right) -> Down
      | otherwise  -> snake.newDir
  in 
    {snake | newDir <- newDir}

endIfCollision : Model -> Model
endIfCollision snake = 
  let
    delta = snake.delta
    body = snake.body
    dir = snake.newDir
    (newHead, newBody) = newPos dir body
  in 
    if 
      | delta == 0 && (outside newHead || List.member newHead body) ->
        { snake | 
          status <- End
        }
      | otherwise -> snake

moveIfActive : Model -> Model
moveIfActive snake = 
  let 
    delta = snake.delta
    body = snake.body
    dir = snake.newDir
    (newHead, newBody) = newPos dir body   
  in 
    if 
      | snake.status == Active &&  delta == 0 ->  
        if snake.cherry == Just newHead 
        then 
          { snake |
            dir <- dir
          , body <- newHead::snake.body
          , cherry <- Nothing}
        else
          { snake |
            dir <- dir
          , body <- newHead::newBody}

      | otherwise -> snake


update : Input -> Model -> Model
update input snake =
  case input of 
    Space space -> if 
      | space && snake.status == End -> 
        {startSnake | status <- toggleGame snake.status}
      | space -> 
        {snake | status <- toggleGame snake.status}   
      | otherwise -> 
        snake

    DeltaKeys t keys -> 
      updateDelta t snake
      |> updateDirection keys
      |> endIfCollision
      |> moveIfActive
      |> Debug.watch "snake"

    PopCherry (x, y) -> if 
      | snake.status /= Active -> snake
      | snake.cherry /= Nothing -> snake
      | List.member (x, y) snake.body -> snake
      | x == -1 -> snake
      | otherwise -> {snake | cherry <- Just (x, y)}


outside : (Int, Int) -> Bool
outside (x, y) = not <| (x >= 0 && x < pitWidth) && (y >= 0 && y < pitHeight)


newPos : Direction -> List (Int, Int) -> ((Int, Int), List (Int, Int))
newPos dir snake = 
  let 
    body = List.take ((List.length snake)-1) snake
    head' = List.head body
    head = case head' of 
              Just (x, y) -> (x, y)
              Nothing -> (0,0) 
    newHead = (moveHead dir head)
  in
    (newHead, body)


moveHead : Direction -> (Int, Int) -> (Int, Int)
moveHead dir (x, y) = 
  case dir of 
    Up -> (x, y-1)
    Down -> (x, y+1)
    Left -> (x-1, y)
    Right -> (x+1, y)


-- VIEW

greyText : String -> Form
greyText s = 
  fromString s |> monospace |> Text.color darkGrey |> Text.height 40
  |> text |> moveXY ((pitWidth//2), 3*(pitHeight//4)) 

view : (Int, Int) -> Model -> Element
view (w', h') snake = 
  let
    (w, h) = (toFloat w', toFloat h')
    pitWidth' = toFloat pitWidth*pitBlock
    pitHeight' = toFloat pitHeight*pitBlock
    cherry = case snake.cherry of
      Just (x, y) -> [circle (pitBlock/2) |> filled red |> moveXY (x, y)]
      Nothing -> [] 
    msg = case snake.status of
      Start -> "Press SPACE to Start!"
      Active -> ""
      Paused -> "Press SPACE to Unpause"
      End -> "Press SPACE to Restart"
    info = 
      if snake.status /= Active 
      then [(greyText msg) ] 
      else []
  in
    collage w' h' <|
        [ rect w h
            |> filled (rgb 174 238 238)
        , rect (pitWidth'+2*pitBlock) (pitHeight'+2*pitBlock)
            |> filled (if snake.status == End then red else darkBrown)
        , rect pitWidth' pitHeight'
            |> filled black
        ] ++ (renderSnake snake) ++ cherry ++ info


type SnakePart = Head | Body


moveXY : (Int, Int) -> Graphics.Collage.Form -> Graphics.Collage.Form
moveXY (x, y) = 
  move ( toFloat (-pitWidth//2+x)*pitBlock+pitBlock/2
       , toFloat (pitHeight//2-y)*pitBlock-pitBlock/2)


renderSnakePart : SnakePart -> (Int, Int) -> Graphics.Collage.Form
renderSnakePart part (x, y) = 
  let 
    partColor = case part of 
    Head -> darkYellow
    Body -> yellow
  in 
    rect pitBlock pitBlock 
    |> filled partColor
    |> moveXY (x, y)


renderSnake : Model -> List Graphics.Collage.Form
renderSnake snake = 
  case snake.body of 
      hd::tl ->  renderSnakePart Head hd :: List.map (renderSnakePart Body) tl
      [] -> []


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update startSnake input)


type Input
  = DeltaKeys Time {x: Int, y:Int}
  | Space Bool
  | PopCherry (Int, Int)


nextCherry t ((x,y), seed) =
  Random.generate cherryGen seed

  
cherryGen = Random.pair (Random.int 0 pitWidth) (Random.int 0 pitHeight)


cherrySig = Signal.map PopCherry
  <| Signal.map (\a -> fst a)
  <| Signal.foldp nextCherry ((-1,0), Random.initialSeed 42)
  <| Time.every <| 5*Time.second


input : Signal Input
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)  
  in
     (Signal.mergeMany 
        [ Signal.sampleOn delta <| Signal.map2 DeltaKeys delta Keyboard.arrows
        , Signal.map Space Keyboard.space
        , cherrySig
        ])