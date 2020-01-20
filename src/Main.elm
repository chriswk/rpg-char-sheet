module Main exposing (..)

import Browser as B
import Html exposing (..)
import Html.Attributes as A


type Msg
    = NoOp


type alias Skills =
    { strength : Int
    , dexterity : Int
    , constitution : Int
    , intelligence : Int
    , wisdom : Int
    , charisma : Int
    }


type alias Model =
    { skills : Skills
    }



-- MAIN


main =
    B.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toSkills : Int -> Int -> Int -> Int -> Int -> Int -> Skills
toSkills str dex con int wis cha =
    { strength = str
    , dexterity = dex
    , constitution = con
    , intelligence = int
    , wisdom = wis
    , charisma = cha
    }


defaultSkills =
    toSkills 10 10 10 10 10 10


initialModel : Model
initialModel =
    { skills = defaultSkills
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel, Cmd.none )


view : Model -> B.Document Msg
view model =
    { title = "RPG Character Sheet"
    , body =
        [ charHeader model
        , mainBody model
        , bodyFooter model
        ]
    }


charHeader : Model -> Html Msg
charHeader model =
    header []
        [ section [ A.class "charname" ]
            [ div [ A.class "formgroup" ]
                (inputText
                    "charactername"
                    "Character Name: "
                    "Klaxon Firebeard"
                )
            ]
        , section [ A.class "misc" ]
            (miscList model)
        ]


inputText : String -> String -> String -> List (Html Msg)
inputText name labelText placeholder =
    [ label [ A.for name ] [ text labelText ]
    , input [ A.name name, A.type_ "text", A.placeholder placeholder ] []
    ]


miscList : Model -> List (Html Msg)
miscList model =
    [ ul []
        [ li [] (inputText "classlevel" "Class & Level" "Monk 4")
        , li [] (inputText "background" "Background" "Monastic Traveller")
        , li [] (inputText "playername" "Player Name" "Lu-Tze (Deja fu)")
        , li [] (inputText "race" "Race" "Dragonborn")
        , li [] (inputText "alignment" "Alignment" "Chaotic Neutral")
        , li [] (inputText "xp" "Experience Points" "597")
        ]
    ]


mainBody : Model -> Html Msg
mainBody model =
    main_ []
        [ section [] (skillsAndProficiencies model)
        , section [] (statsAttacksAndEquipment model)
        , section [] (personalityAndTraits model)
        ]


skillsAndProficiencies : Model -> List (Html Msg)
skillsAndProficiencies model =
    []


statsAttacksAndEquipment : Model -> List (Html Msg)
statsAttacksAndEquipment model =
    []


personalityAndTraits : Model -> List (Html Msg)
personalityAndTraits model =
    []


bodyFooter : Model -> Html Msg
bodyFooter model =
    footer [] []
