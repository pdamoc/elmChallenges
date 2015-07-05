import Color exposing (lightBlue)
import Graphics.Collage exposing (collage, circle, filled, move)
import Graphics.Element exposing (Element, layers, container, bottomRight, show)
import Window exposing (dimensions)
import Random exposing (Seed, initialSeed, generate, pair, int)
import Time exposing (every, Time)
import Keyboard exposing (presses, KeyCode)
import Char exposing (fromCode)
import Signal exposing (map, foldp, mergeMany, sampleOn, mailbox, Mailbox)
import Html exposing (Html, div, button, text, toElement, fromElement)
import Html.Events exposing (onClick)


main : Signal Html
main = map view model 


interval : Signal Time
interval = every 500


type alias Model =
  { locs : List (Int,Int)
  , seed : Seed
  , dimensions: (Int, Int)
  , key: Char
  , active: Bool
  }


initialModel : Model
initialModel = 
  { locs = []
  , seed = initialSeed 42
  , dimensions = (0,0) 
  , key = 'a'
  , active = True
  }


actionMailbox : Mailbox Char 
actionMailbox = mailbox 'a' 

    
type Action = Tick (Int, Int) | KeyPress Char


updates : Signal Action
updates =
  mergeMany
    [map Tick (sampleOn interval dimensions)
    ,map KeyPress (map fromCode presses)
    ,map KeyPress actionMailbox.signal
    ]


model : Signal Model
model = foldp update initialModel updates



getNextCircle : (Int, Int) -> Seed -> ((Int, Int), Seed)
getNextCircle (w, h) seed =
  generate (pair (int 10 (w-10)) (int 10 (h-10))) seed


update: Action -> Model -> Model
update action model= 
  case action of
    Tick (w, h) -> 
      let 
        (pos, seed') = getNextCircle (w, h) model.seed
      in 
        if model.active then { model | seed <- seed'
          , dimensions <- (w, h)
          , locs <- pos :: model.locs}
        else model 

    KeyPress char -> 
      case char of 
        'p' -> {model | active <-not model.active}
        'P' -> {model | active <-not model.active}
        'r' -> {model | locs<-[]}
        'R' -> {model | locs<-[]}
        _   -> model


view : Model -> Html
view model  =
  let 
    (w, h) = model.dimensions
    drawCircle (x,y) =
          circle 10
            |> filled lightBlue
            |> move (toFloat x - toFloat w / 2 , toFloat h / 2 - toFloat y)
  in
      div []
        [button [onClick actionMailbox.address 'p' ] 
                [text (if model.active then "Pause" else "Play")]
        ,button [onClick actionMailbox.address 'r' ] [text "Reset"]
        , fromElement (collage w h (List.map drawCircle model.locs)) 
        ]
