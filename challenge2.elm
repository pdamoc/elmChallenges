import Color exposing (lightBlue)
import Graphics.Collage exposing (collage, circle, filled, move)
import Graphics.Element exposing (Element, layers)
import Window
import Random exposing (Seed, initialSeed, generate, pair, int)
import Time exposing (every)
import StartApp 
import Effects exposing (Effects)
import Signal exposing (Address)
import Html exposing (Html, fromElement)

interval = every 500

type alias Model =
    { locs : List (Int,Int)
    , seed : Seed
    , dimensions: (Int, Int)
    }

init = 
  ({ locs = []
  , seed = initialSeed 42
  , dimensions = (0,0) 
  }, Effects.none)

type Action = Update (Int, Int)

dimensionsTick : Signal Action 
dimensionsTick =
  Signal.map Update (Signal.sampleOn interval Window.dimensions)


update: Action -> Model -> (Model, Effects Action)
update action model=
  case action of 
    Update (w, h) -> 
      let 
        (pos, seed') = generate (pair (int 10 (w-10)) (int 10 (h-10))) model.seed
      in 
        ({ model | seed = seed'
        , dimensions = (w, h)
        , locs = pos :: model.locs
        }, Effects.none) 

view : Address Action -> Model -> Html
view address model  =
  let 
    (w, h) = model.dimensions
    drawCircle (x,y) =
          circle 10
            |> filled lightBlue
            |> move (toFloat x - toFloat w / 2 , toFloat h / 2 - toFloat y)
  in
      fromElement <| layers
        [ collage w h (List.map drawCircle model.locs) 
        --, show (w,h)
        ]

app = StartApp.start 
  { init = init
  , update = update
  , view = view
  , inputs = [ dimensionsTick ]
  }

main = app.html