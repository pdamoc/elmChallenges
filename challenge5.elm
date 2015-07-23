module Challenge5 where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window
import Debug
import Random 
import Text exposing (fromString, Text, monospace, bold)
import String exposing (trim)
import Html exposing (fromElement, div, Attribute)
import Html.Attributes exposing (style, type', placeholder, value)
import Html.Events exposing (on, keyCode, targetValue)
import Json.Decode as Json
import Signal exposing (Address)

-- CONFIG 

pitWidth = 40
pitHeight = 20
pitBlock = 22

-- MODEL

type Direction = Left | Right | Up | Down 


type Status = Start | Active | Paused | End | HighScore


type alias Keys = { x:Int, y:Int }

type alias Model =
  { body : List (Int, Int)
  , dir : Direction
  , status : Status
  , fruit : Maybe (Int, Int)
  , delta : Time
  , speed : Float
  , highScores : List (String, Int)
  , name : String
  }

fakeHighScores = 
  [ ("John Snow", 9001)
  , ("Night's King", 8500)
  , ("The Mountain", 8000)
  , ("Drogon", 70)
  , ("Hodor", 10) 
  ]

initHS = Maybe.withDefault fakeHighScores getStorage

startSnake : Model
startSnake = 
  { body = [ (pitWidth//2, pitHeight//2)
            , (pitWidth//2-1, pitHeight//2)
            , (pitWidth//2-2, pitHeight//2)
            ]
  , dir = Down
  , status = Start
  , fruit = Nothing
  , delta = 0
  , speed = 3
  , highScores = initHS
  , name = ""
  }

-- UPDATE

toggleGame : Status -> Status
toggleGame status = 
  case status of 
    End -> Active
    Paused -> Active
    Start -> Active
    Active -> Paused
    HighScore -> HighScore

updateDelta : Time -> Model -> Model
updateDelta t snake =
  let 
    newDelta = snake.delta + t
  in 
    if newDelta > 20/snake.speed 
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
      | otherwise  -> snake.dir
  in 
    if snake.dir == newDir 
    then snake
    else {snake | dir <- newDir, delta <-0}

highScoreFromSpeed : Float -> Int
highScoreFromSpeed s = truncate <| ((s-3)/2)*100


newHighScore : Float -> List (String, Int) -> Bool
newHighScore speed highScores = 
  let 
    scores = snd <| List.unzip highScores
    min = case (List.minimum scores) of
      Just x -> x
      Nothing -> 0
  in 
    (highScoreFromSpeed speed) > min


endIfCollision : Model -> Model
endIfCollision snake = 
  let
    delta = snake.delta
    body = snake.body
    (newHead, newBody) = newPos snake.dir body
  in 
    if 
      | snake.status == Active && delta == 0 && (outside newHead || List.member newHead body) ->
        { snake | 
          status <- if newHighScore snake.speed snake.highScores 
            then HighScore 
            else End
        }
      | otherwise -> snake

moveIfActive : Model -> Model
moveIfActive snake = 
  let 
    delta = snake.delta
    body = snake.body
    (newHead, newBody) = newPos snake.dir body   
  in 
    if 
      | snake.status == Active &&  delta == 0 ->  
        if snake.fruit == Just newHead 
        then 
          { snake | body <- newHead::snake.body
          , fruit <- Nothing
          , speed <- snake.speed + 0.5}
        else
          { snake | body <- newHead::newBody}

      | otherwise -> snake

updateHighScores name speed highScores = 
  let 
    highScore = highScoreFromSpeed speed
    highScores' = (name, highScore)::highScores 
    newHighScores = List.reverse <| List.sortBy (\(n, s) -> s) highScores'
  in
    List.take 5 newHighScores

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
      |> updateDirection (Debug.watch "keys" keys)
      |> endIfCollision
      |> moveIfActive
      |> Debug.watch "snake"

    PopFruit (x, y) -> if 
      | snake.status /= Active -> snake
      | snake.fruit /= Nothing -> snake
      | List.member (x, y) snake.body -> snake
      | x == -1 -> snake
      | otherwise -> {snake | fruit <- Just (x, y)}

    HighScoreEntered l -> 
      {snake | 
          highScores <- l
        , name <- ""
        , status <- End}

    UpdateName name -> 
      {snake | name <- name}
    --Nothing -> snake


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

--fruitImg = toForm <| image pitBlock pitBlock "apple.png"
fruitImg = toForm <| image pitBlock pitBlock "https://raw.githubusercontent.com/pdamoc/elmChallenges/master/apple.png"
  

coloredText : String -> Color.Color -> Form
coloredText s c = 
  fromString s |> monospace |> Text.color c |> Text.height 40
  |> text |> moveXY ((pitWidth//2), 4*(pitHeight//5)) 


highScoresLine : (Int, (String, Int)) -> Element
highScoresLine (i, (name, score)) = 
  let 
    pos = toString (i+1)
    dotsNeeded = 30 - (String.length name) - (String.length (toString score))
    dots = String.repeat dotsNeeded "."
    score' = toString score
    txt = fromString (" " ++ pos ++ "." ++ name ++ dots ++ 
      if score > 0 then score' else ".")
  in 
    txt |> monospace |> Text.color white |> Text.height 40
    |> leftAligned |> width (pitWidth*pitBlock - 2*pitBlock)
    -- |> moveXY ((pitWidth//2),   pitHeight + i)   

highScoresList : List (String, Int) -> Form
highScoresList highScores = 
    let 
      hsList = List.indexedMap (,) highScores
      header = fromString "High Scores" |> monospace |> Text.color white 
        |> Text.height 40 |> centered |> width (pitWidth*pitBlock - 2*pitBlock)
    in
      toForm <| flow down <| [header]++ (List.map highScoresLine hsList)


toPair : List (String) -> (String, String) 
toPair l = 
  case l of
    a::b::[] -> (trim a, trim b)
    _ -> ("", "")
    
toStyle : String ->  Attribute
toStyle s = 
  let 
    attrs = String.split ";" <| trim s
    attrs' = List.map (\s' -> String.split ":" <| trim s') attrs
  in 
    style <| List.map toPair attrs' 

onEnter : Address a -> a -> Attribute
onEnter address value =
    on "keydown"
      (Json.customDecoder keyCode is13)
      (\_ -> Signal.message address value)


is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"

highScoreInput : Address Input -> Model -> Html.Html
highScoreInput address snake =
  div [toStyle "display:flex; position:absolute; top:0; left:0;justify-content:center;align-items:center;width:100%;height:100%;"] 
      [ Html.input [ toStyle "font-size:40px;"
        , type' "input"
        , value snake.name
        , placeholder "Enter your name"
        , on "input" targetValue (Signal.message address << UpdateName)
        , onEnter address (HighScoreEntered (updateHighScores snake.name snake.speed snake.highScores))
        ] []
      ] 


view : Signal.Address Input -> (Int, Int) -> Model -> Html.Html
view address (w', h') snake = 
  let
    (w, h) = (toFloat w', toFloat h')
    pitWidth' = toFloat pitWidth*pitBlock
    pitHeight' = toFloat pitHeight*pitBlock
    fruit = case snake.fruit of
      Just (x, y) -> [fruitImg |> moveXY (x, y)]
      Nothing -> [] 
    msg = case snake.status of
      Start -> "Press SPACE to Start!"
      Active -> ""
      Paused -> "Press SPACE to Unpause"
      End -> "Press SPACE to Restart"
      HighScore -> "New High Score: "++ (toString <| highScoreFromSpeed snake.speed)

    info = 
      case snake.status of 
        Active -> []
        End -> 
          [rect (pitWidth'-2*pitBlock) (pitHeight'-2*pitBlock)
            |> filled (rgba 100 100 100 0.8)
          , highScoresList snake.highScores |> move (0.0, (toFloat 2*pitBlock))
          , (coloredText msg white) 
          ]
        HighScore -> 
          [rect (pitWidth'-2*pitBlock) (pitHeight'-2*pitBlock)
            |> filled (rgba 100 100 100 0.8)
            , (coloredText msg white) 
          ]
        _ -> [(coloredText msg white) ] 
    highScoreForm = case snake.status of 
      HighScore -> [ highScoreInput address snake ]
      _ -> []
      
  in
    div [] ([
    fromElement <| collage w' h' <|
        [ rect w h
            |> filled (rgb 174 238 238)
        , rect (pitWidth'+2*pitBlock) (pitHeight'+2*pitBlock)
            |> filled (if snake.status == End then red else darkBrown)
        , rect pitWidth' pitHeight'
            |> filled black
        ] ++ (renderSnake snake) ++ fruit ++ info
    ] ++ highScoreForm)


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
    group [ rect pitBlock pitBlock |> filled partColor 
    , rect pitBlock pitBlock |> outlined (solid darkBrown)
    , if part == Head 
      then 
        ngon 5 (pitBlock/2.2) |> outlined (solid red)
      else 
        rect (pitBlock/1.2) (pitBlock/1.2) 
          |> outlined (solid darkBrown) 

    ] |> moveXY (x, y)


renderSnake : Model -> List Graphics.Collage.Form
renderSnake snake = 
  case snake.body of 
      hd::tl ->  renderSnakePart Head hd :: List.map (renderSnakePart Body) tl
      [] -> []


-- SIGNALS

main : Signal Html.Html
main =
  Signal.map2 (view actions.address) Window.dimensions model

model = Signal.foldp update startSnake input

type Input
  = DeltaKeys Time {x: Int, y:Int}
  | Space Bool
  | PopFruit (Int, Int)
  | HighScoreEntered (List (String, Int))
  | UpdateName String
  --| Nothing


nextFruit t ((x,y), seed) =
  Random.generate fruitGen seed

  
fruitGen = Random.pair (Random.int 0 <| pitWidth-1) (Random.int 0 <| pitHeight-1)


fruitSig = Signal.map PopFruit
  <| Signal.map (\a -> fst a)
  <| Signal.foldp nextFruit ((-1,0), Random.initialSeed 42)
  <| Time.every <| 2*Time.second

actions : Signal.Mailbox Input
actions = Signal.mailbox (Space False)

input : Signal Input
input =
  let
    delta = Signal.map (\t -> t/20) (fps 30)  
  in
     (Signal.mergeMany 
        [ Signal.sampleOn delta <| Signal.map2 DeltaKeys delta Keyboard.arrows
        , Signal.map Space Keyboard.space
        , fruitSig
        , actions.signal
        ])


updateHS input hs =
  case input of
    HighScoreEntered l -> l
    _ -> hs

highScoreSig = Signal.foldp updateHS initHS input

-- interactions with localStorage to save the high scores
port getStorage : Maybe (List (String, Int))

port setStorage : Signal (List (String, Int))
port setStorage = highScoreSig