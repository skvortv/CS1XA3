
import Html.Attributes exposing (style)
import Html as Html
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Keyboard as Key
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time)


{- Main -}
main : Program Never Model Msg
main = Html.program
          { init = init,
            view = view,
            update = update,
            subscriptions = subscriptions
            }

type alias Model = {
        time : Time,
        pattern : List Int,
        pattern2 : List Int,
        counter : Int,
        wait : Int,
        pb : Time
                   }
init : (Model, Cmd.Cmd Msg)
init = ({time = 0.000, 
         pattern =   [39,40,38,39,38,40,38,38,37,38,38,39,37,40,37,40,37,37,39,40,40,39,39,37,38,39,38,40,37,37,39,40],
         pattern2 =  [37,40,39,38,37,37,39,37,38,40,40,40,39,38,39,39,37,40,38,40,37,38,39,38,39,37,38,38,40,40,39,37],
         counter = 32,
         wait = 0,
         pb = 9999999999999 }, Cmd.none)


type Msg = KeyMsg Key.KeyCode
           |Tick Time

update : Msg -> Model -> (Model,Cmd.Cmd Msg)
update msg model = case msg of 
                        Tick timeV -> (updateTime timeV model,Cmd.none)
                        KeyMsg keyCode -> (updateMove keyCode model, Cmd.none)

updateTimeCounter : Model -> Model 
updateTimeCounter model = if (List.isEmpty model.pattern) then
        if model.time > model.pb then 
          (model)
        else
          {model | pb = model.time}
    else
      {model | time = model.time + 1 }

waitCheck : Model ->Model
waitCheck model = if model.wait == 0 then 
  (model)
  else 
    {model | pattern = model.pattern2, pattern2 = List.map plusThree model.pattern2 , wait = 0  }

updateTime : Time -> Model -> Model
updateTime timeV model = waitCheck model |> updateTimeCounter  

listHead : List Int -> Int
listHead pattern  = 
    List.head pattern 
       |> Maybe.withDefault 1

plusThree : Int -> Int
plusThree x = case x of 
              40 -> 39
              39 -> 38
              38 -> 37
              x -> 40  

updateMove : Key.KeyCode -> Model -> Model
updateMove keyCode model = if keyCode == listHead model.pattern then
    { model | pattern = List.drop 1 model.pattern, counter = model.counter - 1}
    else
      {model | time = -1, 
      pattern = List.map plusThree [],
      counter = 32,
      wait = 1 }

colume : List Int -> String
colume list = case listHead list of
                37 -> "0"
                38 -> "100"
                39 -> "300"
                40 -> "200"
                _  -> "9000"

timeToString : Time -> String
timeToString x  = case x of
              -1 -> "0"
              x -> toString x  

pbToString : Time -> String
pbToString x = case x of 
            9999999999999 -> ""
            x -> toString x               

view : Model -> Html.Html msg
view model = let
      personalBestTime = [text_ [x "415", y "140", fill "Black"] [text (String.append "Your Personal Best Time : " (pbToString model.pb)) ] ]
      instructions = [text_ [x "415", y "25", fill "Black"] [text "Press the arrow key corresponding to the block on the golden row." ] ]
      instructions2 = [text_ [x "415", y "45", fill "Black"] [text "Complete all 32 blocks as fast as you can!" ] ]
      divider1 = [rect [x "100", y "0", width "2", height "500", fill "Black"] [] ] 
      divider2 = [rect [x "200", y "0", width "2", height "500", fill "Black"] [] ] 
      divider3 = [rect [x "300", y "0", width "2", height "500", fill "Black"] [] ] 
      playBox =  [rect [x "0", y "0", width "400", height "405", fill "honeydew", stroke "Black", strokeWidth "2" ] [] ] 
      hitRow = [rect [x "0", y "400", width "400", height "100", fill "gold", stroke "Black", strokeWidth "5", opacity "0.5" ] [] ] 
      howManyLeft = [text_ [x "145", y "620", fill "Black"] [text (String.append "Blocks Remaining : "  (toString model.counter) ) ] ]
      score = [text_ [x "175", y "600", fill "Black"] [text (String.append "Time: " (timeToString model.time)) ] ]
      row0L = [text_ [x "25", y "550", fill "Black"] [text "LEFT"] ]
      row0U = [text_ [x "140", y "550", fill "Black"] [text "UP"] ]
      row0D = [text_ [x "225", y "550", fill "Black"] [text "DOWN"] ]
      row0R = [text_ [x "325", y "550", fill "Black"] [text "RIGHT"] ]
      row1 =  [rect [x (colume model.pattern), y "400", width "100" ,height"100", rx "5", ry "5", fill "Red", opacity "0.8" ] []] 
      row2 =  [rect [x (colume (List.drop 1 model.pattern)), y "300", width "100" ,height"100",rx "5", ry "5", fill "Red", opacity "0.8"] []] 
      row3 =  [rect [x (colume (List.drop 2 model.pattern)), y "200", width "100" ,height"100",rx "5", ry "5", fill "Red", opacity "0.8"] []]  
      row4 =  [rect [x (colume (List.drop 3 model.pattern)), y "100", width "100" ,height"100",rx "5", ry "5", fill "Red", opacity "0.8"] []] 
      row5 =  [rect [x (colume (List.drop 4 model.pattern)), y "000", width "100" ,height"100",rx "5", ry "5", fill "Red", opacity "0.8"] []] 
    in 
        svg [width "1200",height "625"] 
            (playBox ++ hitRow ++ howManyLeft ++ row0L ++ row0U ++ row0D ++row0R ++ row1 ++ row2 ++ 
             row3 ++ row4 ++ row5 ++ divider1 ++ divider2 ++ divider3  ++  score ++ instructions ++ 
             instructions2 ++ personalBestTime)


subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch [Key.downs KeyMsg, Time.every Time.second Tick]
  