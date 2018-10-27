module Challenge1 exposing (main)

import Browser
import Browser.Events exposing (onMouseMove, onResize)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Json.Decode as Decode


type alias Model =
    { windowWidth : Int
    , mouseX : Int
    }


init : Model -> ( Model, Cmd Msg )
init initialModel =
    ( initialModel, Cmd.none )


type Msg
    = SetMouseX Int
    | SetWindowWidth Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMouseX mouseX ->
            ( { model | mouseX = mouseX }, Cmd.none )

        SetWindowWidth windowWidth ->
            ( { model | windowWidth = windowWidth }, Cmd.none )


view : Model -> Html Msg
view model =
    let
        ( message, color, backgroundColor ) =
            if model.mouseX < (model.windowWidth // 2) then
                ( "LEFT", "white", "black" )

            else
                ( "RIGHT", "black", "white" )
    in
    div [ class "container", style "color" color, style "background-color" backgroundColor ]
        [ text message ]


mouseXDecoder : Decode.Decoder Int
mouseXDecoder =
    Decode.field "clientX" Decode.int


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\width height -> SetWindowWidth width)
        , onMouseMove (Decode.map SetMouseX mouseXDecoder)
        ]


main : Program Model Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
