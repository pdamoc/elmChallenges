module Challenge4 exposing (main)

import Browser
import Html exposing (Html, div, img, input, text)
import Html.Attributes exposing (placeholder, src, value)
import Html.Events exposing (onInput)
import Http
import Json.Decode as Decode
import List.Extra as List
import Task exposing (Task)
import Time exposing (Posix)


type alias Model =
    { username : String
    , user : Maybe User
    , lastKeyPress : Maybe Posix
    , lastUsername : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { username = "evancz"
      , user = Nothing
      , lastKeyPress = Nothing
      , lastUsername = ""
      }
    , Cmd.none
    )


type Msg
    = SetUsername String
    | GetUser
    | SetUser (Maybe User)
    | Tick Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUsername username ->
            ( { model | username = username }, Cmd.none )

        GetUser ->
            ( model, getUser model.username )

        SetUser user ->
            ( { model | user = user }, Cmd.none )

        Tick now ->
            case model.lastKeyPress of
                Nothing ->
                    ( { model | lastKeyPress = Just now }, Cmd.none )

                Just lastKeyPress ->
                    if Time.posixToMillis now - Time.posixToMillis lastKeyPress >= 1000 then
                        ( { model | lastKeyPress = Just now, lastUsername = model.username }
                        , getUser model.username
                        )

                    else
                        ( model, Cmd.none )


knownLanguages : List String -> String
knownLanguages languages =
    "Knows the following languages: " ++ String.join ", " languages


view : Model -> Html Msg
view model =
    div []
        [ input
            [ placeholder "Github username"
            , onInput SetUsername
            , value model.username
            ]
            []
        , case model.user of
            Nothing ->
                text ""

            Just user ->
                div []
                    [ div [] [ text user.name ]
                    , img [ src user.picture ] []
                    , div [] [ text (knownLanguages user.languages) ]
                    ]
        ]


type alias User =
    { name : String
    , picture : String
    , reposUrl : String
    , languages : List String
    }


getUrl : String -> Decode.Decoder a -> Task Http.Error a
getUrl url decoder =
    Http.get url decoder
        |> Http.toTask


userDecoder : Decode.Decoder User
userDecoder =
    Decode.map4 User
        (Decode.field "name" Decode.string)
        (Decode.field "avatar_url" Decode.string)
        (Decode.field "repos_url" Decode.string)
        (Decode.succeed [])


languagesDecoder : Decode.Decoder (List String)
languagesDecoder =
    Decode.map
        (\languages -> List.unique (List.filter (\s -> s /= "") languages))
        (Decode.list
            (Decode.oneOf
                [ Decode.at [ "language" ] Decode.string
                , Decode.succeed ""
                ]
            )
        )


errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl message ->
            "Bad Url, " ++ message

        Http.Timeout ->
            "Request timeout error"

        Http.NetworkError ->
            "Network error, make sure you're connected to the internet"

        Http.BadStatus response ->
            "Bad status, " ++ response.status.message

        Http.BadPayload message response ->
            "Bad payload, " ++ message


getUser : String -> Cmd Msg
getUser username =
    getUrl ("https://api.github.com/users/" ++ username) userDecoder
        |> Task.andThen
            (\user ->
                (getUrl user.reposUrl languagesDecoder
                    |> Task.onError (\msg -> Task.succeed [ errorToString msg ])
                )
                    |> Task.andThen
                        (\languages ->
                            Task.succeed { user | languages = languages }
                        )
            )
        |> Task.attempt (Result.toMaybe >> SetUser)


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
