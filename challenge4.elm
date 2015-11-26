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
import Signal exposing (sampleOn, dropRepeats, Address)
import StartApp 
import Effects exposing (Effects)

-- MODEL

type alias User = 
  { name : String
  , avatar_url : String
  , repos_url : String
  , languages : List String
  }

type alias Model = 
  { query : String
  , lastKeyPress : Maybe Time
  , user : Maybe User
  }

init : Model 
init = { query = "evancz", lastKeyPress = Nothing, user = Nothing}

-- UPDATE 

type Action = Update (Maybe User) | UpdateQuery String | Tick Time

lookupUser : String -> Effects Action
lookupUser query =
  ((Http.get decodeUser ("http://api.github.com/users/" ++ query))
  `andThen` \user ->
    (Http.get decodeLanguages user.repos_url `onError` 
      (\msg -> succeed [toString msg]))
  `andThen` \languages -> 
      let 
        user' : User
        user' =  { user | languages = notEmptyUnique languages }
      in succeed user') 
  |> Task.toMaybe 
  |> Task.map Update
  |> Effects.task

notEmptyUnique : List String -> List String
notEmptyUnique xs = 
  List.filter (\x -> not <| String.isEmpty x) <| Set.toList <| Set.fromList xs

decodeLanguages : Json.Decoder (List (String))
decodeLanguages = (Json.list  <| Json.oneOf 
  [ (Json.at ["language"] Json.string)
  , (Json.succeed "")
  ])

decodeUser : Json.Decoder (User)
decodeUser = Json.object4 User
    ("name" := Json.string) 
    ("avatar_url" := Json.string)
    ("repos_url" := Json.string)
    (Json.succeed [])

update : Action -> Model -> (Model, Effects Action)
update action model = 
  case action of
    UpdateQuery str -> 
      ({ model | query = str}, Effects.tick Tick)
    
    Tick time -> 
      case model.lastKeyPress of
        Nothing -> ({model| lastKeyPress = Just time}, Effects.tick Tick)
        Just t ->  
          if (time - t) > second 
          then ({model| lastKeyPress = Just t}, lookupUser model.query)
          else (model, Effects.tick Tick)

    Update user -> 
      ({ model | user = user}, Effects.none)

-- VIEW

view : Address Action  -> Model -> Html
view address model =
  let field =
        input
          [ placeholder "Please enter the GitHub username"
          , value model.query
          , on "input" targetValue (\s -> Signal.message address (UpdateQuery s))
          , myStyle
          ]
          []

      messages =
        case model.user of
          Nothing ->
            let
              msg = case model.lastKeyPress of 
                Nothing -> "Looking for user..."
                Just t -> "User not found :("
            in
              [ div [ myStyle ] [ text msg ] ]

          Just user ->
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


app : StartApp.App Model
app = StartApp.start 
  { init = (init, lookupUser init.query)
  , update = update
  , view = view
  , inputs = [] }

main : Signal Html
main = app.html

port tasks : Signal (Task Effects.Never ())
port tasks = app.tasks


