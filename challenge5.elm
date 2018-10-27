port module Challenge5 exposing (main)

import Browser
import Browser.Events exposing (onKeyPress)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import List.Extra as List
import Random
import Time


type alias Box =
    { x : Int
    , y : Int
    }


type Direction
    = Up
    | Down
    | Left
    | Right


type alias Model =
    { direction : Direction
    , head : Box
    , tail : List Box
    , isGameover : Bool
    , food : Box
    , score : Int
    , highScore : Int
    }


initialModel : Model
initialModel =
    { direction = Right
    , head = { x = 0, y = 9 }
    , tail = []
    , isGameover = False
    , food = { x = 9, y = 9 }
    , score = 0
    , highScore = 0
    }


init : Decode.Value -> ( Model, Cmd Msg )
init value =
    let
        highScore =
            Decode.decodeValue Decode.string value
                |> Result.withDefault "0"
                |> String.toInt
                |> Maybe.withDefault 0
    in
    ( { initialModel | highScore = highScore }, Cmd.none )


type Msg
    = NoOp
    | Tick
    | SetDirection Direction
    | SetFood Box
    | Restart


areOppositeDirections : Direction -> Direction -> Bool
areOppositeDirections direction direction_ =
    case direction of
        Up ->
            direction_ == Down

        Down ->
            direction_ == Up

        Left ->
            direction_ == Right

        Right ->
            direction_ == Left


isOutOfBounds : Box -> Bool
isOutOfBounds { x, y } =
    x < 0 || x >= 20 || y < 0 || y >= 20


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ head, tail, direction, isGameover, food, score, highScore } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick ->
            if isGameover then
                ( model, Cmd.none )

            else
                let
                    newHead =
                        case direction of
                            Up ->
                                { head | y = head.y - 1 }

                            Down ->
                                { head | y = head.y + 1 }

                            Left ->
                                { head | x = head.x - 1 }

                            Right ->
                                { head | x = head.x + 1 }

                    isEatingFood =
                        newHead.x == food.x && newHead.y == food.y

                    newTail =
                        if isEatingFood then
                            head :: tail

                        else
                            case List.init tail of
                                Nothing ->
                                    []

                                Just end ->
                                    head :: end

                    newScore =
                        if isEatingFood then
                            score + 1

                        else
                            score

                    newHighScore =
                        max highScore newScore
                in
                if List.any ((==) newHead) newTail || isOutOfBounds newHead then
                    ( { model | isGameover = True }, Cmd.none )

                else
                    ( { model
                        | head = newHead
                        , tail = newTail
                        , score = newScore
                        , highScore = newHighScore
                      }
                    , if isEatingFood then
                        Cmd.batch [ generateFood, saveHighScore newHighScore ]

                      else
                        Cmd.none
                    )

        SetDirection newDirection ->
            if areOppositeDirections direction newDirection then
                ( model, Cmd.none )

            else
                ( { model | direction = newDirection }, Cmd.none )

        SetFood newFood ->
            ( { model | food = newFood }, Cmd.none )

        Restart ->
            ( { initialModel | highScore = highScore }, Cmd.none )


view : Model -> Html Msg
view ({ head, tail, isGameover, food, score, highScore } as model) =
    div []
        [ div [ class "screen" ]
            [ div
                [ class "box"
                , style "background-color" "green"
                , style "left" (String.fromInt (food.x * 20) ++ "px")
                , style "top" (String.fromInt (food.y * 20) ++ "px")
                ]
                []
            , div []
                (List.map
                    (\box ->
                        div
                            [ class "box"
                            , style "left" (String.fromInt (box.x * 20) ++ "px")
                            , style "top" (String.fromInt (box.y * 20) ++ "px")
                            ]
                            []
                    )
                    (head :: tail)
                )
            ]
        , div [ class "message" ] [ text ("High Score: " ++ String.fromInt highScore) ]
        , div [ class "message" ] [ text ("Score: " ++ String.fromInt score) ]
        , if isGameover then
            div []
                [ div [ class "message" ] [ text "Game Over" ]
                , button [ class "restart-button", onClick Restart ]
                    [ text "Restart" ]
                ]

          else
            text ""
        ]


generateFood : Cmd Msg
generateFood =
    Random.generate SetFood
        (Random.map2
            (\x y -> { x = x, y = y })
            (Random.int 0 19)
            (Random.int 0 19)
        )


keyPressDecoder : Decode.Decoder Msg
keyPressDecoder =
    Decode.field "key" Decode.string
        |> Decode.map
            (\key ->
                case String.toUpper key of
                    "W" ->
                        SetDirection Up

                    "S" ->
                        SetDirection Down

                    "A" ->
                        SetDirection Left

                    "D" ->
                        SetDirection Right

                    _ ->
                        NoOp
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every 200 (\_ -> Tick)
        , onKeyPress keyPressDecoder
        ]


port saveHighScore : Int -> Cmd msg


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
