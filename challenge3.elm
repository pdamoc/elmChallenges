module Challenge3 exposing (main)

import Browser
import Browser.Events exposing (onKeyPress, onResize)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random
import Time


type alias WindowSize =
    { width : Int, height : Int }


type alias Dot =
    { x : Float, y : Float }


type alias Model =
    { dots : List Dot
    , windowSize : WindowSize
    , isRunning : Bool
    }


init : WindowSize -> ( Model, Cmd Msg )
init windowSize =
    ( { dots = [], windowSize = windowSize, isRunning = True }, Cmd.none )


type Msg
    = SetWindowSize WindowSize
    | Tick
    | NewDot Dot
    | ToggleRunning
    | Restart
    | DoNothing


generateNewDot : Cmd Msg
generateNewDot =
    Random.generate NewDot
        (Random.pair (Random.float 0 1) (Random.float 0 1)
            |> Random.map (\( x, y ) -> { x = x, y = y })
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWindowSize windowSize ->
            ( { model | windowSize = windowSize }, Cmd.none )

        Tick ->
            ( model, generateNewDot )

        NewDot dot ->
            ( { model | dots = dot :: model.dots }, Cmd.none )

        ToggleRunning ->
            ( { model | isRunning = not model.isRunning }, Cmd.none )

        Restart ->
            ( { model | dots = [], isRunning = True }, Cmd.none )

        DoNothing ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div []
            (List.map
                (\dot ->
                    let
                        left =
                            String.fromFloat (toFloat model.windowSize.width * dot.x) ++ "px"

                        top =
                            String.fromFloat (toFloat model.windowSize.height * dot.y) ++ "px"
                    in
                    div [ class "dot", style "left" left, style "top" top ] []
                )
                model.dots
            )
        , button [ onClick Restart ] [ text "Restart" ]
        , button [ onClick ToggleRunning ]
            [ text <|
                if model.isRunning then
                    "Pause"

                else
                    "Resume"
            ]
        ]


keyPressDecoder : Decode.Decoder Msg
keyPressDecoder =
    Decode.map
        (\key ->
            case String.toUpper key of
                "P" ->
                    ToggleRunning

                "R" ->
                    Restart

                _ ->
                    DoNothing
        )
        (Decode.field "key" Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\width height -> SetWindowSize { width = width, height = height })
        , Time.every 500
            (\_ ->
                if model.isRunning then
                    Tick

                else
                    DoNothing
            )
        , onKeyPress keyPressDecoder
        ]


main : Program WindowSize Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
