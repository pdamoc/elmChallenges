import Char
import Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import String
import Task exposing (..)


-- VIEW

view : String -> Result String (User) -> Html
view string result =
  let field =
        input
          [ placeholder "GitHub username"
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
              , img  [ src user.avatar_url ] []
              ]--:: map (\lang -> div [ myStyle ] [ text lang ]) user.languages
  in
      div [] (field :: messages)


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
  Signal.mailbox ""


results : Signal.Mailbox (Result String (User)) 
results =
  Signal.mailbox (Err "Please Enter a valid GitHub username")


port requests : Signal (Task x ())
port requests =
  query.signal
    |> Signal.map lookupUser 
    |> Signal.map (\task -> Task.toResult task `andThen` Signal.send results.address)


lookupUser : String -> Task String (User)
lookupUser query =
  succeed ("http://api.github.com/users/" ++ query)
  `andThen` (mapError (always "User not found :(") << Http.get decodeUser)
  `andThen` \userData ->
    (Http.get decodeLanguages userData.repos_url `onError` (\msg -> succeed []))
  `andThen` \languages -> 
      let 
        user : User
        user =  { name = userData.name
                , avatar_url = userData.avatar_url
                , repos_url = userData.repos_url
                , languages = languages  
                }
      in succeed user 

decodeLanguages = (Json.list  (Json.at ["language"] Json.string))

type alias User = 
  {name: String
  , avatar_url: String
  , repos_url: String
  , languages: List String
  }


type alias UserData = 
  {name: String
  , avatar_url: String
  , repos_url: String
  }

decodeUser : Json.Decoder (UserData)
decodeUser = Json.object3 UserData
    ("name" := Json.string) 
    ("avatar_url" := Json.string)
    ("repos_url" := Json.string)
