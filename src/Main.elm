module Main exposing (..)

import Css exposing (..)
import Html.Styled exposing (Html, button, div, h1, input, label, span, styled, text)
import Html.Styled.Attributes exposing (src, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Random exposing (int)
import Time


---- MODEL ----


type alias Participant =
    { name : String
    , selected : Bool
    }


type alias Model =
    { participants : List Participant
    , isPlaying : Bool
    , turns : Int
    , maxTurns : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model
        (buildParticipantList 20)
        False
        0
        20
    , Cmd.none
    )


buildParticipantList : Int -> List Participant
buildParticipantList numberOfParticipant =
    Participant "" False
        |> List.repeat numberOfParticipant
        |> List.indexedMap (\index participant -> { participant | name = toString <| index + 1 })



---- UPDATE ----


type Msg
    = ChangeParticipantsNumber Int
    | Select
    | Play
    | MarkSelected Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeParticipantsNumber participantsNumber ->
            ( { model | participants = buildParticipantList participantsNumber }, Cmd.none )

        Play ->
            ( { model | isPlaying = True }, Cmd.none )

        Select ->
            ( { model
                | participants = resetSelected model.participants
              }
            , Random.generate MarkSelected <| Random.int 0 <| List.length model.participants
            )

        MarkSelected selectedIndex ->
            let
                isPlaying =
                    model.turns < model.maxTurns
            in
            ( { model
                | participants =
                    List.indexedMap
                        (\index participant ->
                            if index == selectedIndex then
                                { participant | selected = True }
                            else
                                participant
                        )
                        model.participants
                , isPlaying = isPlaying
                , turns =
                    if isPlaying then
                        model.turns + 1
                    else
                        0
              }
            , Cmd.none
            )


resetSelected : List Participant -> List Participant
resetSelected participants =
    participants
        |> List.map (\participant -> { participant | selected = False })



---- VIEW ----


theme =
    { colorBackground = hex "000000"
    , color = hex "00caff"
    , fontSize = Css.rem 1.5
    , standardPadding = padding (Css.rem 1)
    , borderWidth = Css.px 2
    }


view : Model -> Html Msg
view model =
    styled div
        [ backgroundColor theme.colorBackground
        , color theme.color
        , textAlign center
        , minHeight (Css.vh 100)
        , width (pct 100)
        , theme.standardPadding
        , boxSizing borderBox
        ]
        []
        ([ styled h1
            [ fontWeight bold
            , textTransform uppercase
            , margin3 (Css.rem 0) (Css.rem 1) (Css.rem 2)
            , theme.standardPadding
            ]
            []
            [ text "Bologna JS lottery" ]
         ]
            ++ [ styled div
                    [ width (pct 100)
                    , marginBottom (Css.rem 3)
                    ]
                    []
                    (model.participants |> List.map (\participant -> renderParticipant participant))
               ]
            ++ [ styled label
                    [marginBottom (Css.rem 1)
                    , display inlineBlock
                    , textTransform uppercase
                    , border3 theme.borderWidth solid theme.color
                    , theme.standardPadding
                    ]
                    []
                    [ div
                        []
                        [ text "participants " ]
                    , inpt
                        [ value <| toString <| List.length model.participants
                        , type_ "number"
                        , onInput (\val -> String.toInt val |> Result.withDefault 20 |> ChangeParticipantsNumber)
                        ]
                        []
                    ]
               , div
                    []
                    [ btn
                        [ onClick Play ]
                        [ text "play" ]
                    ]
               ]
        )


inpt : List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
inpt = 
    styled input
        [padding zero
        , backgroundColor theme.colorBackground
        , color theme.color
        , border zero
        , textAlign center
        ]

btn : List (Html.Styled.Attribute msg) -> List (Html msg) -> Html msg
btn =
    styled button
        [ border3 theme.borderWidth solid theme.color
        , theme.standardPadding
        , minWidth (Css.rem 2)
        , backgroundColor theme.colorBackground
        , color theme.color
        , textTransform uppercase
        ]


participantStyle : Style
participantStyle =
    batch
        [ theme.standardPadding
        , border3 theme.borderWidth solid theme.color
        , margin (Css.rem 1)
        , display inlineBlock
        , minWidth (Css.em 2)
        , property "transition" "all 0.3s"
        ]


selectedStyle : Style
selectedStyle =
    batch
        [ participantStyle
        , backgroundColor theme.color
        , color theme.colorBackground
        , transforms [scale 1.3]
        ]


renderParticipant : Participant -> Html msg
renderParticipant participant =
    styled div
        [ if participant.selected then
            selectedStyle
          else
            participantStyle
        ]
        []
        [ text participant.name ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isPlaying then
        Time.every 300 (\time -> Select)
    else
        Sub.none



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.Styled.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
