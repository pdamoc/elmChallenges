import Color exposing (lightBlue)
import Graphics.Collage exposing (collage, circle, filled, move)
import Graphics.Element exposing (Element, layers)
import Window
import Random exposing (Seed, initialSeed, generate, pair, int)
import Time exposing (every)

interval = every 500

type alias Model =
    { locs : List (Int,Int)
    , seed : Seed
    , dimensions: (Int, Int)
    }
initialModel = 
  { locs = []
  , seed = initialSeed 42
  , dimensions = (0,0) 
  }

model : Signal Model
model =
  Signal.foldp update initialModel (Signal.sampleOn interval Window.dimensions)


main : Signal Element
main = Signal.map view model 

update: (Int, Int) -> Model -> Model
update (w, h) model= 
  let 
    (pos, seed') = generate (pair (int 10 (w-10)) (int 10 (h-10))) model.seed
  in 
    { model | seed <- seed'
    , dimensions <- (w, h)
    , locs <- pos :: model.locs
    } 

view : Model -> Element
view model  =
  let 
    (w, h) = model.dimensions
    drawCircle (x,y) =
          circle 10
            |> filled lightBlue
            |> move (toFloat x - toFloat w / 2 , toFloat h / 2 - toFloat y)
  in
      layers
        [ collage w h (List.map drawCircle model.locs) 
        --, show (w,h)
        ]
