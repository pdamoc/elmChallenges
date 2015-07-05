import Color exposing (lightBlue, purple, white, black)
import Graphics.Element exposing (Element, color, container, middle, show, centered)
import Window
import Mouse
import Text 

coloredText text color = (Text.color color (Text.fromString text))

view: (Int, Int) -> Int -> Element
view (w, h) x = 
      let 
        (bkg_color, text_color, text) = 
            if x < w //2 then (purple, white, "Left") 
                else (lightBlue, black, "Right")
      in 
        color bkg_color (container w h middle 
            (centered (coloredText text text_color)))
      
main : Signal Element
main =
  Signal.map2 view Window.dimensions Mouse.x 