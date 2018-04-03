module Main exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)

headerStyle = style [ ("font-size" , "300%"),("background-color", "teal"), ("color", "Black"), ("padding", "10px 10px 10px 10px"), ("margin", "0px 0px 0px 0px"), ("text-align", "center"), ("font-family", "Times New Roman, Times, serif" ) ]
divStyle = style [("background-color", "honeydew"),  ("padding", "10px 10px 10px 10px"), ("margin", "25px 200px 25px 200px"), ("border-style", "solid" )]
hStyle = style [("background-color", "honeydew"), ("color", "red"), ("padding", "10px 10px 10px 10px"), ("margin", "10px 10px 0px 10px")]
miniHStyle = style [("color", "black"), ("padding", "5px 5px 10px 10px"), ("margin", "5px 5px 5px 5px") ]
listStyle = style [("background-color", "honeydew"), ("color", "black"), ("padding", "0px 0px 0px 0px"), ("margin", "5px 5px 5px 50px")]
asideStyle = style [("background-color", "honeydew"), ("color", "black"), ("padding", "30px 30px 30px 30px"), ("margin", "0px 0px 0px 0px"), ("float", "right"), ("font-size", "150%")]
pStyle = style [("background-color", "honeydew"), ("color", "black"), ("padding", "0px 0px 0px 0px"), ("margin", "0px 0px 0px 25px")]
pStyle2 = style [("background-color", "tomato"), ("color", "white"), ("padding", "5px 5px 5px 5px"), ("margin", "0px 0px 0px 0px"), ("text-indent", "50px")]
main : Html msg
main = div [divStyle]
    [

    header [headerStyle] [ text "Seva Skvortsov"],
    aside [asideStyle] [text "1280 Main St West",
                        br [] [], text "Hamilton, ON", 
                        br [] [], text "L8S 4M6", 
                        br [] [], br [] [],  
                        br [] [],text "647-677-****", 
                        br [] [],text "Seva.sk@gmail.com", 
                        br [] [], a [ href "https://github.com/skvortv" ] [ text "Github"   ],
                        br [] [],
                        img [src "IMG_20180324_141256.jpg", width 200, height 275, style [ ("border-radius", "5px"), ("position" ,"absolute" ), ("right", "240px") ]  ] [] ],
    h2 [hStyle] [text "EDUCATION"] ,
    b [miniHStyle] [text "Bill Crothers Secondary School, Markham, 2013 - 2017 "],
    ul [listStyle] [li [] [text "Graduated with Ontario Scholar"], li [] [text "Achieved Honor Roll"] ],
    b [miniHStyle] [text "Mcmaster University, Hamilton, 2017 - Present, Anticipated Graduatation 2021 "],
    ul [listStyle] [li [] [text "Seeking Degree In Mathematics and Computer Science"] ],
    h2 [hStyle] [text "VOLUNTEER EXPERIENCE"],
    b [miniHStyle] [text "Teacher Assistant, SciX-Science Explorations, Summer Camp at York University, Toronto, 07/2014 - 08/2014" ],
    ul [listStyle] [li [] [text "Supervised 15-20 children "], li [] [text "Helped design lessons for students ages 9-11"], li [] [text "Organized games and activities for students ages 9-11"] ],
    b [miniHStyle] [text "Timer, Richmond Hill Aquatic Club, Richmond Hill, 2013-2014" ],
    ul [listStyle] [li [] [text "Assisted in organizing weekend swimming competitions for ages 8-12"], li [] [text "Oversaw and recorded data to insure the swim meet ran smoothly "] ],
    h2 [hStyle] [text "WORK EXPERIENCE"] ,
    b [miniHStyle] [text "Shop Helper, Multimadic, 19/07/2017-20/08/2017"],
    ul [listStyle] [li [] [text "Buffered metal"], li [] [text "Worked assembly/machine operator putting automotive parts together"] ],
    b [miniHStyle] [text "Warehouse Worker,  Rizopia, 05/07/2017-16/07/2017"],
    ul [listStyle] [li [] [text "Organized and inspected the production of an assembly line"], li [] [text "Performed hourly testing and recording of product composition, weight and physical look, as well as testing with the metal detector"],  li [] [text "Stacked orders onto pallets, ensured proper and secure placement"] ],
    h2 [hStyle] [text "QUALIFICATIONS AND SPECIAL SKILLS"] ,
    ul [listStyle] [li [] [text "Bilingual (English, speaking and reading Russian)"], li [] [text "Proficient in Haskell"] , li [] [text "Familiar with Bash"] , li [] [text "Proficient in Python"], li [] [text "Familiar with Elm"], li [] [text "Working knowledge of Git"]  ],
    h2 [hStyle] [text "ACTIVITIES/INTERESTS"] ,
    ul [listStyle] [li [] [text "Provincial level swimmer"], li [] [text "Computer Programming"] , li [] [a [ href "http://ugweb.cas.mcmaster.ca/~skvortv/elmgame.html" ] [ text "My Elm Game" ] ] ],
    h2 [hStyle] [text "REFERENCES"] ,
    p [pStyle] [text "Available upon request"]
    ]