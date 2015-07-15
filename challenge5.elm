import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Keyboard
import Time exposing (..)
import Window

type Direction = Left | Right | Up | Down


type Status = Start | Active | Paused | End

type alias Model =
  { snake : List (Int, Int)
  , dir : Direction
  , status : Status
  }

pitWidth = 30
pitHeight = 20

startSnake : Model
startSnake = 
  { snake = [(pitWidth//2, pitHeight//2), (pitWidth//2+1, pitHeight//2)]
  , dir = Right
  , status = Start
  }

main = show startSnake