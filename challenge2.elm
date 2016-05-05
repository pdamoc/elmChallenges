import Svg exposing (..)
import Svg.Attributes exposing (width, height, viewBox, xmlSpace, cx, cy, r, fill)
import Html.App as HA
import Window exposing (Size)
import Random exposing (generate, pair, int)
import Time exposing (every, Time)
import Task

interval : Time
interval = 500

type alias Model =
    { locs : List (Int,Int)
    , size: Size
    }

init : (Model, Cmd Msg)
init = 
  ({ locs = []
  , size = Size 0 0 
  }, Task.perform (\_ -> Resize (Size 0 0)) Resize Window.size) 

type Msg = 
  Resize Size
  | Tick Time 
  | NewDot (Int, Int)


update: Msg -> Model -> (Model, Cmd Msg)
update msg model=
  case msg of 
    Resize size -> 
      ({model| size = size}, Cmd.none)
    Tick _ -> 
      let 
        generator =
          pair (int 10 (model.size.width-10)) (int 10 (model.size.height-10))
      in 
        ( model, generate NewDot generator)

    NewDot pos -> 
      ({model| locs = pos::model.locs}, Cmd.none)

view : Model -> Svg msg
view model =
  let 
    w = toString model.size.width
    h = toString (model.size.height-5) 
    viewBoxA = viewBox ("0 0 "++w++" "++h)
    sLocs = List.map (\(x, y)-> (toString x, toString y)) model.locs
    toCircle (x, y) = 
      circle [cx x, cy y, r "10", fill "lightBlue"][]
  in 
    svg 
    [ width w, height h, viewBoxA, xmlSpace "http://www.w3.org/2000/svg" ]
    (List.map toCircle sLocs)


main : Program Never
main =
  HA.program
    { init = init
    , update = update
    , view = view
    , subscriptions = 
        (\_ -> Sub.batch [ Window.resizes Resize, Time.every interval Tick]) 
    }