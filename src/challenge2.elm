module Challenge2 exposing (main)

import Browser
import Browser.Events exposing (onResize)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class, style)
import Random
import Time


type alias WindowSize =
    { width : Int, height : Int }


type alias Dot =
    { x : Float, y : Float }


type alias Model =
    { dots : List Dot
    , windowSize : WindowSize
    }


init : WindowSize -> ( Model, Cmd Msg )
init windowSize =
    ( { dots = [], windowSize = windowSize }, Cmd.none )


type Msg
    = SetWindowSize WindowSize
    | Tick
    | NewDot Dot


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


view : Model -> Html Msg
view model =
    div []
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onResize (\width height -> SetWindowSize { width = width, height = height })
        , Time.every 500 (\_ -> Tick)
        ]


main : Program WindowSize Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
