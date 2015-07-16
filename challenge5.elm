import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug


-- MODEL

type Direction = Left | Right | Up | Down 

type Status = Start | Active | Paused | End

type alias Keys = { x:Int, y:Int }

type alias Model =
  { snake : List (Int, Int)
  , dir : Direction
  , status : Status
  }

pitWidth = 30
pitHeight = 20
pitBlock = 20

startSnake : Model
startSnake = 
  { snake = [(pitWidth//2, pitHeight//2), (pitWidth//2-1, pitHeight//2), (pitWidth//2-2, pitHeight//2)]
  , dir = Down
  , status = Start
  }


-- UPDATE

update : Input -> Model -> Model
update input snake =
  let 
    s = Debug.watch "input" input
  in
  case input of 
    Space space -> if 
      | space && snake.status == End -> {startSnake | status <- Active}
      | space && (snake.status == Paused || snake.status == Start) -> {snake | status <- Active}
      | space && snake.status == Active -> {snake | status <- Paused}     
      | snake.status == Paused || snake.status == Start -> snake      
      | otherwise -> snake
    DeltaKeys t keys -> 
      let 
        dir = if  | keys.x < 0 && (snake.dir == Up || snake.dir == Down) -> Left
                  | keys.x > 0 && (snake.dir == Up || snake.dir == Down) -> Right
                  | keys.y > 0 && (snake.dir == Left || snake.dir == Right) -> Up
                  | keys.y < 0 && (snake.dir == Left || snake.dir == Right) -> Down
                  | otherwise  -> snake.dir
        (newHead, newBody) = newPos snake.dir snake.snake
        nx = fst newHead
        ny = snd newHead
      in
      if
      | snake.status == Active && (nx >= 0 && nx < pitWidth) && (ny >=   0 && ny < pitHeight) -> 
          { snake |
            dir <- dir
          , snake <- newHead::newBody}
      | not ((nx >= 0 && nx < pitWidth) && (ny >=   0 && ny < pitHeight)) -> 
          { snake | 
            status <- End
          }
      | otherwise -> snake

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

moveHead dir (x, y) = 
  case dir of 
    Up -> (x, y-1)
    Down -> (x, y+1)
    Left -> (x-1, y)
    Right -> (x+1, y)


-- VIEW


view : (Int, Int) -> Model -> Element
view (w', h') snake = 
  let
    (w, h) = (toFloat w', toFloat h')
    pitWidth' = toFloat pitWidth*pitBlock
    pitHeight' = toFloat pitHeight*pitBlock
  in
    collage w' h' <|
        [ rect w h
            |> filled (rgb 174 238 238)
        , rect (pitWidth'+2*pitBlock) (pitHeight'+2*pitBlock)
            |> filled (if snake.status == End then red else darkBrown)
        , rect pitWidth' pitHeight'
            |> filled black
        ] ++ (renderSnake snake)


type SnakePart = Head | Body

renderSnakePart : SnakePart -> (Int, Int) -> Graphics.Collage.Form
renderSnakePart part (x, y) = 
  let partColor = case part of 
    Head -> darkYellow
    Body -> yellow
  in 
    rect pitBlock pitBlock 
    |> filled partColor
    |> move (toFloat  (-pitWidth//2+x)*pitBlock+pitBlock/2, toFloat  (pitHeight//2-y)*pitBlock-pitBlock/2)

renderSnake : Model -> List Graphics.Collage.Form
renderSnake snake = 
  case snake.snake of 
      hd::tl ->  renderSnakePart Head hd :: List.map (renderSnakePart Body) tl
      [] -> []


-- SIGNALS

main : Signal Element
main =
  Signal.map2 view Window.dimensions (Signal.foldp update startSnake input)

type Input
  = DeltaKeys Time {x: Int, y:Int}
  | Space Bool

input : Signal Input
input =
  let
    delta = Signal.map (\t -> t/20) (fps 15)  
  in
     (Signal.mergeMany [Signal.sampleOn delta <| Signal.map2 DeltaKeys delta Keyboard.arrows, Signal.map Space Keyboard.space])