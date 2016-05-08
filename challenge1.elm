import Color exposing (Color, lightBlue, purple, white, black)
import Element exposing (Element, color, container, middle, show, centered, toHtml)
import Text exposing (Text)
import Html.App as App
import Html exposing (Html)
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

coloredText : String -> Color -> Text
coloredText text color = (Text.color color (Text.fromString text))

view: Model -> Html msg
view model = 
  let 
    (w, h) = (model.size.width, model.size.height)
    (bkg_color, text_color, text) = 
        if   model.position.x < w //2
        then (purple, white, "Left") 
        else (lightBlue, black, "Right")
  in 
    toHtml
    <| color bkg_color
    <| container w h middle 
    <| centered
    <| coloredText text text_color


main : Program Never
main =
  App.program
    { init = init
    , update = \msg model -> (update msg model, Cmd.none)
    , view = view
    , subscriptions = 
        (\_ -> Sub.batch [ Window.resizes SizeChange, Mouse.moves MouseMove]) 
    }

