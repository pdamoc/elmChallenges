import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as HA
import Window exposing (Size)
import Mouse exposing (Position)
import Task 


type alias Model = 
  { size : Size
  , position : Position
  }


init : (Model, Cmd Msg)
init = 
  ({ size = Size 0 0 
  , position = Position 0 0 
  }, Task.perform (\_ -> DoNothing) SizeChange Window.size)


type Msg = 
  SizeChange Size
  | MouseMove Position 
  | DoNothing 


update : Msg -> Model -> Model
update msg model = 
  case msg of 
    SizeChange size -> {model | size = size}
    MouseMove pos -> {model | position = pos}
    DoNothing -> model


view: Model -> Html msg
view model = 
  let 
    (bkg_color, text_color, label) = 
        if model.position.x < model.size.width //2
        then ("purple", "white", "Left") 
        else ("lightBlue", "black", "Right") 
  in 
    div 
      [ fullscreen bkg_color]
      [ span 
        [ largeText text_color] 
        [ text label] 
      ]


largeText : String -> Attribute msg
largeText text_color = 
  style 
    [ ("color", text_color)
    , ("font-family", "sans-serif")
    , ("font-size", "32px")]


fullscreen : String -> Attribute msg
fullscreen bkg_color = 
  style 
    [ ("display", "flex")
    , ("width", "100vw")
    , ("minHeight", "100vh")
    , ("align-items", "center")
    , ("justify-content", "center")
    , ("background", bkg_color) ]


main : Program Never
main =
  HA.program
    { init = init
    , update = \msg model -> (update msg model, Cmd.none)
    , view = view
    , subscriptions = 
        (\_ -> Sub.batch [ Window.resizes SizeChange, Mouse.moves MouseMove]) 
    }

