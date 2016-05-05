import Svg exposing (..)
import Html exposing (Html, body, div, button)
import Html.Attributes as HAttr
import Html.Events exposing (onClick, on, keyCode)
import Svg.Attributes exposing (width, height, viewBox, xmlSpace, cx, cy, r, fill)
import Html.App as HA
import Window exposing (Size)
import Random exposing (generate, pair, int)
import Time exposing (every, Time)
import Task
import Json.Decode as Json
import Char


interval : Time
interval = 500


type alias Model =
    { locs : List (Int,Int)
    , size: Size
    , isRunning : Bool
    }


init : (Model, Cmd Msg)
init = 
  ({ locs = []
  , size = Size 0 0 
  , isRunning = True
  }, Task.perform (\_ -> DoNothing) Resize Window.size) 


type Msg = 
  Resize Size
  | Tick Time 
  | NewDot (Int, Int)
  | ToggleRunning
  | Reset
  | DoNothing 


update: Msg -> Model -> (Model, Cmd Msg)
update msg model=
  case msg of 
    Resize size -> 
      ({model| size = size}, Cmd.none)
    Tick _ -> 
      let 
        generator =
          pair (int 10 (model.size.width-10)) (int 10 (model.size.height-10))
        cmd = 
          if model.isRunning then 
            generate NewDot generator 
            else Cmd.none
      in 
        ( model, cmd)

    NewDot pos -> 
      ({model| locs = pos::model.locs}, Cmd.none)

    ToggleRunning -> 
      ({model| isRunning = not model.isRunning}, Cmd.none)
 
    Reset -> 
      ({model| locs = []}, Cmd.none)
 
    DoNothing -> 
      (model, Cmd.none)
    

onKeyUp : Attribute Msg
onKeyUp =
  let 
    tagger code = 
      case (Char.toUpper <| Char.fromCode code) of 
        'P' -> ToggleRunning
        'R' ->  Reset 
        _ ->  DoNothing 
  in 
    on "keyup" (Json.map tagger keyCode)



view : Model -> Html Msg
view model =
  let 
    w = toString model.size.width
    h = toString (model.size.height-5) -- weird bug
    viewBoxA = viewBox ("0 0 "++w++" "++h)
    sLocs = List.map (\(x, y)-> (toString x, toString y)) model.locs
    toCircle (x, y) = 
      circle [cx x, cy y, r "10", fill "lightBlue"][]
    playPauseLabel = if model.isRunning then "Pause" else "Play"
    buttons = 
      div [HAttr.style [("position", "absolute")]]
      [ button [onClick ToggleRunning] [text playPauseLabel]
      , button [onClick Reset] [text "Reset"]]
  in 
    div [onKeyUp]
    [ buttons 
    , svg 
      [ width w, height h, viewBoxA, xmlSpace "http://www.w3.org/2000/svg" ]
      (List.map toCircle sLocs)
    ]

main : Program Never
main =
  HA.program
    { init = init
    , update = update
    , view = view
    , subscriptions = 
        (\_ -> Sub.batch [ Window.resizes Resize, Time.every interval Tick]) 
    }