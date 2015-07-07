import Char
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)
import Set
import String exposing (join )
import Time exposing (since, second, Time)
import Signal exposing (sampleOn, dropRepeats)
import Debug

-- VIEW

view : String -> Result String (User) -> Html
view string result =
  let field =
        input
          [ placeholder "Please enter the GitHub username"
          , value string
          , on "input" targetValue (Signal.message query.address)
          , myStyle
          ]
          []

      messages =
        case result of
          Err msg ->
              [ div [ myStyle ] [ text msg ] ]

          Ok user ->
              [ div [ myStyle ] [ text user.name ]
              , img  [ src user.avatar_url, imgStyle] []
              , div [ myStyle ] [ text <| knownLanguages user.languages]
              ]
  in
      div [] ((div [ myStyle ] [ text "GitHub Username" ]) :: field :: messages)


knownLanguages : List String -> String 
knownLanguages langs = 
  "Knows the following programming languages: " ++ (join ", " langs)

imgStyle : Attribute
imgStyle =
  style
    [ ("display", "block")
    , ("margin-left", "auto")
    , ("margin-right", "auto")
    ]


myStyle : Attribute
myStyle =
  style
    [ ("width", "100%")
    , ("height", "40px")
    , ("padding", "10px 0")
    , ("font-size", "2em")
    , ("text-align", "center")
    ]


-- WIRING

main =
  Signal.map2 view query.signal results.signal


query : Signal.Mailbox String
query =
  Signal.mailbox "evancz"


results : Signal.Mailbox (Result String (User)) 
results =
  Signal.mailbox (Err "")

maybeQuery : Time -> Signal a -> Signal (Maybe a)
maybeQuery time sig =
  Signal.map2 (\b s -> if b then Just s else Nothing) (Signal.map not (time `since` query.signal)) sig

port requests : Signal (Task x ())
port requests =
  flip Signal.map (maybeQuery second query.signal)
    (\q ->
      case q of
        Nothing ->
          Task.succeed ()
        
        Just str ->
          Task.toResult (lookupUser str) `andThen` Signal.send results.address)


lookupUser : String -> Task String (User)
lookupUser query =
  succeed ("http://api.github.com/users/" ++ query)
  `andThen` (mapError (always "User not found :(") << Http.get decodeUser)
  `andThen` \user ->
    (Http.get decodeLanguages user.repos_url `onError` 
      (\msg -> succeed [toString msg]))
  `andThen` \languages -> 
      let 
        user' : User
        user' =  { user | languages <- notEmptyUnique languages }
      in succeed user' 

notEmptyUnique : List String -> List String
notEmptyUnique xs = 
  List.filter (\x -> not <| String.isEmpty x) <| Set.toList <| Set.fromList xs

decodeLanguages : Json.Decoder (List (String))
decodeLanguages = (Json.list  <| Json.oneOf 
  [ (Json.at ["language"] Json.string)
  , (Json.succeed "")
  ])

type alias User = 
  {name: String
  , avatar_url: String
  , repos_url: String
  , languages: List String
  }

decodeUser : Json.Decoder (User)
decodeUser = Json.object4 User
    ("name" := Json.string) 
    ("avatar_url" := Json.string)
    ("repos_url" := Json.string)
    (Json.succeed [])
