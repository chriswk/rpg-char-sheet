module Main exposing (..)

import Browser as B
import Html exposing (..)
import Html.Attributes as A
import Html.Events as E


type Msg
    = NoOp
    | ChangeStrength String
    | ChangeDexterity String
    | ChangeConstitution String
    | ChangeWisdom String
    | ChangeIntelligence String
    | ChangeCharisma String


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

        ChangeStrength newStr ->
            let
                str =
                    String.toInt newStr

                skills =
                    model.skills

                newS =
                    case str of
                        Just s ->
                            { skills | strength = s }

                        Nothing ->
                            skills
            in
            ( { model | skills = newS }, Cmd.none )

        ChangeDexterity newDex ->
            let
                dex =
                    String.toInt newDex

                skills =
                    model.skills

                newS =
                    case dex of
                        Just d ->
                            { skills | dexterity = d }

                        Nothing ->
                            skills
            in
            ( { model | skills = newS }, Cmd.none )

        ChangeConstitution newCon ->
            let
                con =
                    String.toInt newCon

                skills =
                    model.skills

                newS =
                    case con of
                        Just d ->
                            { skills | constitution = d }

                        Nothing ->
                            skills
            in
            ( { model | skills = newS }, Cmd.none )

        ChangeWisdom newWis ->
            let
                wis =
                    String.toInt newWis

                skills =
                    model.skills

                newS =
                    case wis of
                        Just d ->
                            { skills | wisdom = d }

                        Nothing ->
                            skills
            in
            ( { model | skills = newS }, Cmd.none )

        ChangeIntelligence newInt ->
            let
                int =
                    String.toInt newInt

                skills =
                    model.skills

                newS =
                    case int of
                        Just d ->
                            { skills | intelligence = d }

                        Nothing ->
                            skills
            in
            ( { model | skills = newS }, Cmd.none )

        ChangeCharisma newCha ->
            let
                cha =
                    String.toInt newCha

                skills =
                    model.skills

                newS =
                    case cha of
                        Just d ->
                            { skills | charisma = d }

                        Nothing ->
                            skills
            in
            ( { model | skills = newS }, Cmd.none )


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


textArea : String -> String -> List (Html Msg)
textArea name labelText =
    [ label [ A.for name ] [ text labelText ]
    , textarea [ A.name name ] []
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
    [ section [ A.class "attributes" ] (attributesList model)
    , div [ A.class "passive-perception", A.class "box" ] (passivePerception model)
    , div [ A.class "otherprefs", A.class "box", A.class "textblock" ] (textArea "otherprofs" "Other Proficiencies and Languages")
    ]


applications : Model -> Html Msg
applications model =
    div [ A.class "attr-applications" ] []


attributesList : Model -> List (Html Msg)
attributesList model =
    [ scores model
    , applications model
    ]


scores : Model -> Html Msg
scores model =
    div [ A.class "scores" ]
        [ ul []
            [ li []
                [ div [ A.class "score" ]
                    [ label [ A.for "Strengthscore" ] [ text "Strength" ]
                    , input [ A.type_ "number", A.min "0", A.max "25", E.onInput ChangeStrength, A.name "Strengthscore", A.value (String.fromInt model.skills.strength) ] []
                    ]
                , div [ A.class "modifier" ]
                    [ input [ A.name "Strengthmod", A.placeholder (modifier model.skills.strength) ] []
                    ]
                ]
            , li []
                [ div [ A.class "score" ]
                    [ label [ A.for "Dexterityscore" ] [ text "Dexterity" ]
                    , input [ A.type_ "number", A.min "0", A.max "25", E.onInput ChangeDexterity, A.name "Dexterityscore", A.value (String.fromInt model.skills.dexterity) ] []
                    ]
                , div [ A.class "modifier" ]
                    [ input [ A.name "Dexteritymod", A.placeholder (modifier model.skills.dexterity) ] []
                    ]
                ]
            , li []
                [ div [ A.class "score" ]
                    [ label [ A.for "Constitutionscore" ] [ text "Constitution" ]
                    , input [ A.type_ "number", A.min "0", A.max "25", E.onInput ChangeConstitution, A.name "Constitutionscore", A.value (String.fromInt model.skills.constitution) ] []
                    ]
                , div [ A.class "modifier" ]
                    [ input [ A.name "Constitutionmod", A.placeholder (modifier model.skills.constitution) ] []
                    ]
                ]
            , li []
                [ div [ A.class "score" ]
                    [ label [ A.for "Wisdomscore" ] [ text "Wisdom" ]
                    , input [ A.type_ "number", A.min "0", A.max "25", E.onInput ChangeWisdom, A.name "Wisdomscore", A.value (String.fromInt model.skills.wisdom) ] []
                    ]
                , div [ A.class "modifier" ]
                    [ input [ A.name "Wisdommod", A.placeholder (modifier model.skills.wisdom) ] []
                    ]
                ]
            , li []
                [ div [ A.class "score" ]
                    [ label [ A.for "Intelligencescore" ] [ text "Intelligence" ]
                    , input [ A.type_ "number", A.min "0", A.max "25", E.onInput ChangeIntelligence, A.name "Intelligencescore", A.value (String.fromInt model.skills.intelligence) ] []
                    ]
                , div [ A.class "modifier" ]
                    [ input [ A.name "Intelligencemod", A.placeholder (modifier model.skills.intelligence) ] []
                    ]
                ]
            , li []
                [ div [ A.class "score" ]
                    [ label [ A.for "Charismascore" ] [ text "Charisma" ]
                    , input [ A.type_ "number", A.min "0", A.max "25", E.onInput ChangeCharisma, A.name "Charismascore", A.value (String.fromInt model.skills.charisma) ] []
                    ]
                , div [ A.class "modifier" ]
                    [ input [ A.name "Charismamod", A.placeholder (modifier model.skills.charisma) ] []
                    ]
                ]
            ]
        ]


modifier : Int -> String
modifier skill =
    let
        mod =
            (skill - 10) // 2

        symbol =
            if mod < 0 then
                ""

            else
                "+"
    in
    symbol ++ String.fromInt mod


passivePerception : Model -> List (Html Msg)
passivePerception model =
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
