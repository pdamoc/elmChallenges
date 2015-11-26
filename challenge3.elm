import Color exposing (lightBlue)
import Graphics.Collage exposing (collage, circle, filled, move)
import Window exposing (dimensions)
import Random exposing (Seed, initialSeed, generate, pair, int)
import Time exposing (every, Time)
import Keyboard exposing (presses)
import Char exposing (fromCode, KeyCode)
import Signal exposing (map, sampleOn, Address)
import Html exposing (Html, div, button, text, toElement, fromElement)
import Html.Events exposing (onClick)
import StartApp 
import Effects exposing (Effects)

interval : Signal Time
interval = every 500

type alias Model =
  { locs : List (Int,Int)
  , seed : Seed
  , dimensions: (Int, Int)
  , key: Char
  , active: Bool
  }


init : Model
init = 
  { locs = []
  , seed = initialSeed 42
  , dimensions = (0,0) 
  , key = 'a'
  , active = True
  }

type Action = Tick (Int, Int) | KeyPress Char


getNextCircle : (Int, Int) -> Seed -> ((Int, Int), Seed)
getNextCircle (w, h) seed =
  generate (pair (int 10 (w-10)) (int 10 (h-10))) seed


update: Action -> Model -> (Model, Effects Action) 
update action model= 
  case action of
    Tick (w, h) -> 
      let 
        (pos, seed') = getNextCircle (w, h) model.seed
        newModel =  
          if model.active 
          then { model | seed = seed', dimensions = (w, h), locs = pos :: model.locs}
          else model 
      in 
        (newModel, Effects.none)

    KeyPress char -> 
      let
        newModel = case char of 
          'p' -> {model | active = not model.active}
          'P' -> {model | active = not model.active}
          'r' -> {model | locs = []}
          'R' -> {model | locs = []}
          _   -> model
      in
        (newModel, Effects.none)

view : Address Action -> Model -> Html
view address model  =
  let 
    (w, h) = model.dimensions
    drawCircle (x,y) =
          circle 10
            |> filled lightBlue
            |> move (toFloat x - toFloat w / 2 , toFloat h / 2 - toFloat y)
  in
      div []
        [button [onClick address (KeyPress 'p') ] 
                [text (if model.active then "Pause" else "Play")]
        ,button [onClick address (KeyPress 'r') ] [text "Reset"]
        , fromElement (collage w h (List.map drawCircle model.locs)) 
        ]


app = StartApp.start 
  { init = (init, Effects.none)
  , update = update
  , view = view 
  , inputs =     
    [ map Tick (sampleOn interval dimensions)
    , map KeyPress (map fromCode presses)
    ]}

main = app.html