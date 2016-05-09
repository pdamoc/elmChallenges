import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard exposing (KeyCode)
import Time exposing (..)
import Window exposing (Size)
import Random exposing (Seed)
import Text exposing (fromString, Text, monospace, bold)
import String exposing (trim)
import Html exposing (Html, Attribute, div)
import Html.App as App
import Html.Attributes exposing (style, type', placeholder, value)
import Html.Events exposing (on, keyCode, targetValue)
import Json.Decode as Json
import AnimationFrame 
import Task 


-- CONFIG 

pitWidth : Int
pitWidth = 40


pitHeight : Int
pitHeight = 30


pitBlock : number
pitBlock = 22

fruitInterval = 2*Time.second

-- MODEL

type Direction = Left | Right | Up | Down 


type Status = Start | Active | Paused | End | HighScore


type alias Model =
  { body : List (Int, Int)
  , dir : Direction
  , status : Status
  , fruit : Maybe (Int, Int)
  , ripeness : Time
  , delta : Time
  , speed : Float
  , highScores : List (String, Int)
  , name : String
  , size : Size 
  }

--initHS = Maybe.withDefault (List.repeat 5 ("", 0)) getStorage
initHS = (List.repeat 5 ("", 0)) -- for non-storage version

startSnake : Model
startSnake = 
  { body = [ (pitWidth//2, pitHeight//2)
            , (pitWidth//2-1, pitHeight//2)
            , (pitWidth//2-2, pitHeight//2)
            ]
  , dir = Down
  , status = Start
  , fruit = Nothing
  , ripeness = 0
  , delta = 0
  , speed = 3
  , highScores = initHS
  , name = ""
  , size = Size 0 0 
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
    (newDelta, newRipeness) = 
      if snake.status == Active then 
        (snake.delta + t, snake.ripeness + t) 
      else (0, 0) 
  in 
    if newDelta > 120/snake.speed then 
      {snake | delta = 0, ripeness = newRipeness}
    else 
      {snake | delta = newDelta, ripeness = newRipeness}

updateDirection : Direction -> Model -> Model
updateDirection arrow snake = 
  let 
    newDir = 
      case (arrow, snake.dir == Up || snake.dir == Down) of 
        (Left, True) -> Left
        (Right, True) -> Right
        (Up, False) -> Up
        (Down, False) -> Down
        _ -> snake.dir

  in 
    if snake.dir == newDir then 
      snake
    else 
      {snake | dir = newDir, delta =0}

scoreFromSpeed : Float -> Int
scoreFromSpeed s = truncate <| ((s-3)/2)*100


newHighScore : Float -> List (String, Int) -> Bool
newHighScore speed highScores = 
  let 
    scores = snd <| List.unzip highScores
    min = case (List.minimum scores) of
      Just x -> x
      Nothing -> 0
  in 
    (scoreFromSpeed speed) > min


endIfCollision : Model -> Model
endIfCollision snake = 
  let
    delta = snake.delta
    body = snake.body
    (newHead, newBody) = newPos snake.dir body
  in 
    if snake.status == Active && delta == 0 && (outside newHead || List.member newHead body)
    then
      { snake | 
        status = 
          if newHighScore snake.speed snake.highScores then 
            HighScore 
          else 
            End
      }
    else snake

moveIfActive : Model -> Model
moveIfActive snake = 
  let 
    delta = snake.delta
    body = snake.body
    (newHead, newBody) = newPos snake.dir body   
  in 
    if snake.status == Active &&  delta == 0 
    then  
      if snake.fruit == Just newHead 
      then 
        { snake | body = newHead::snake.body
        , fruit = Nothing
        , speed = snake.speed + 0.5}
      else
        { snake | body = newHead::newBody}
    else snake

updateHighScores name speed highScores = 
  let 
    highScore = scoreFromSpeed speed
    highScores' = (name, highScore)::highScores 
    newHighScores = List.reverse <| List.sortBy (\(n, s) -> s) highScores'
  in
    List.take 5 newHighScores

fruitGen = Random.pair (Random.int 0 <| pitWidth-1) (Random.int 0 <| pitHeight-1)

popFruitIfRipe snake =
  if snake.ripeness > fruitInterval then 
    ({ snake | ripeness = 0 }, Random.generate PopFruit fruitGen)
  else
    (snake, Cmd.none) 

noCmd model = (model, Cmd.none)


type Msg
  = Tick Time 
  | Keys KeyPress
  | PopFruit (Int, Int)
  | HighScoreEntered (List (String, Int))
  | UpdateName String
  | Resize Size 
  | DoNothing

type KeyPress = Space | Key Direction | OtherKeys

toKeyPress keyCode =
  case keyCode of 
    32 -> Space
    38 -> Key Up
    40 -> Key Down
    37 -> Key Left
    39 -> Key Right
    _ -> OtherKeys

update : Msg -> Model -> (Model, Cmd Msg)
update msg snake =
  let
    log = Debug.log "msg " msg 
  in 
  case msg of 
    Keys keyPress ->
      case keyPress of
        Space -> 
          if snake.status == End then 
            noCmd {startSnake | status = toggleGame snake.status
                  , highScores = snake.highScores}
          else  
            noCmd { snake | status = toggleGame snake.status}   
        Key arrow -> 
          noCmd (updateDirection arrow snake)  
        OtherKeys -> noCmd snake

    Tick t -> 
      updateDelta t snake
      |> endIfCollision
      |> moveIfActive
      |> popFruitIfRipe


    PopFruit (x, y) -> 
      if (snake.status /= Active) || (snake.fruit /= Nothing) ||
        ( List.member (x, y) snake.body) || (x == -1) 
      then noCmd snake
      else noCmd {snake | fruit = Just (x, y)}

    HighScoreEntered l -> 
      noCmd { snake | 
          highScores = l
        , name = ""
        , status = End}

    UpdateName name -> 
      noCmd { snake | name = name}

    Resize size -> 
      noCmd { snake | size = size}

    DoNothing -> noCmd snake


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


toPair : List (String) -> Maybe (String, String) 
toPair l = 
  case l of
    a::b::[] -> Just (trim a, trim b)
    _ -> Nothing
    
toStyle : String ->  Attribute msg
toStyle s = 
  let 
    attrs = String.split ";" <| trim s
    attrs' = List.map (\s' -> String.split ":" <| trim s') attrs
  in 
    style <| List.filterMap toPair attrs' 

onEnter : msg -> msg -> Attribute msg
onEnter fail success =
  let 
    tagger code = 
      if code == 13 then success
      else fail
  in 
    on "keyup" (Json.map tagger keyCode)
     

highScoreInput : Model -> Html Msg
highScoreInput snake =
  div [ toStyle "display:flex; position:absolute; top:0; left:0;justify-content:center;align-items:center;width:100%;height:100%;"] 
      [ Html.input [ toStyle "font-size:40px;"
        , type' "input"
        , value snake.name
        , placeholder "Enter your name"
        , on "input" (Json.map UpdateName targetValue)
        , onEnter DoNothing (HighScoreEntered (updateHighScores snake.name snake.speed snake.highScores))
        ] []
      ] 


view : Model -> Html Msg
view snake = 
  let
    {width, height} = snake.size
    (w, h) = (toFloat width, toFloat height)
    pitWidth' = toFloat pitWidth*pitBlock
    pitHeight' = toFloat pitHeight*pitBlock
    fruit = case snake.fruit of
      Just (x, y) -> [fruitImg |> moveXY (x, y)]
      Nothing -> [] 
    score = scoreFromSpeed snake.speed

    msg = case snake.status of
      Start -> "Press SPACE to Start!"
      Active -> ""
      Paused -> "Press SPACE to Unpause"
      End -> "Press SPACE to Restart"
      HighScore -> "New High Score: "++ (toString <| score)

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
      HighScore -> [ highScoreInput snake ]
      _ -> []

    scoreText = if snake.status == Active then (toString score) else ""
    scoreForm = fromString scoreText |> monospace |> Text.color darkGrey |> Text.height 40
      |> text |> moveXY ((pitWidth//2), 0) 

    elements = 
      collage width height 
        ([ rect w h
            |> filled (rgb 29 41 81)
        , rect (pitWidth'+2*pitBlock) (pitHeight'+2*pitBlock)
            |> filled (if snake.status == End then red else darkBrown)
        , rect pitWidth' pitHeight'
            |> filled black
        , scoreForm 
        ] ++ (renderSnake snake) ++ fruit ++ info)
  in
    div [] ([
    toHtml elements
    ] ++ highScoreForm)


type SnakePart = Head | Body


moveXY : (Int, Int) -> Form -> Form
moveXY (x, y) = 
  move ( toFloat (-pitWidth//2+x)*pitBlock+pitBlock/2
       , toFloat (pitHeight//2-y)*pitBlock-pitBlock/2)


renderSnakePart : SnakePart -> (Int, Int) -> Form
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


renderSnake : Model -> List Form
renderSnake snake = 
  case snake.body of 
      hd::tl ->  renderSnakePart Head hd :: List.map (renderSnakePart Body) tl
      [] -> []


-- WIRING 

main : Program Never
main =
  App.program
    { init = (startSnake, Task.perform (\_ -> DoNothing) Resize Window.size)
    , update = update
    , view = view
    , subscriptions = 
        (\_ -> Sub.batch 
          [ Window.resizes Resize
          , AnimationFrame.diffs Tick
          , Keyboard.downs (Keys << toKeyPress)]) 
    }


--main : Signal Html Msg
--main =
--  Signal.map2 (view actions.address) Window.dimensions model

--model = Signal.foldp update startSnake input

--nextFruit : Time -> ((Int, Int), Seed) -> ((Int, Int), Seed)
--nextFruit t ((x,y), seed) =
--  Random.generate fruitGen seed




--fruitSig = Signal.map PopFruit
--  <| Signal.map (\a -> fst a)
--  <| Signal.foldp nextFruit ((-1,0), Random.initialSeed 42)
--  <| Time.every <| 2*Time.second


--actions : Signal.Mailbox Input
--actions = Signal.mailbox (Space False)


--input : Signal Input
--input =
--  let
--    delta = Signal.map (\t -> t/20) (fps 30)  
--  in
--     (Signal.mergeMany 
--        [ Signal.sampleOn delta <| Signal.map2 DeltaKeys delta Keyboard.arrows
--        , Signal.map Space Keyboard.space
--        , fruitSig
--        , actions.signal
--        ])


--inputToHighScore input =
--  case input of
--    HighScoreEntered l -> Just l
--    _ -> Nothing


--highScoreSig = Signal.filterMap inputToHighScore initHS input

-- interactions with localStorage to save the high scores 

--port getStorage : Maybe (List (String, Int))

--port setStorage : Signal (List (String, Int))
--port setStorage = highScoreSig