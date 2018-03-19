module Main exposing (..)

import Css exposing (..)
import Html.Styled exposing (Html, button, styled, div, h1, input, text)
import Html.Styled.Attributes exposing (src, type_, value)
import Html.Styled.Events exposing (onInput, onClick)
import Random exposing (int)


---- MODEL ----


type alias Participant =
    { name : String
    , selected : Bool
    }


type alias Model =
    { participants : List Participant
    }


init : ( Model, Cmd Msg )
init =
    ( Model <| buildParticipantList 20, Cmd.none )


buildParticipantList : Int -> List Participant
buildParticipantList numberOfParticipant =
    Participant "" False
        |> List.repeat numberOfParticipant
        |> List.indexedMap (\index participant -> { participant | name = toString <| index + 1 })



---- UPDATE ----


type Msg
    = ChangeParticipantsNumber Int
    | Play
    | MarkSelected Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeParticipantsNumber participantsNumber ->
            ( Model <| buildParticipantList participantsNumber, Cmd.none )

        Play ->
            ( {model | participants = resetSelected model.participants}, Random.generate MarkSelected <| Random.int 0 <| List.length model.participants )

        MarkSelected selectedIndex ->
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
              }
            , Cmd.none
            )


resetSelected : List Participant -> List Participant
resetSelected participants =
    participants
        |> List.map (\participant -> {participant | selected = False})

---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        ([ h1
            []
            [ text "Bologna JS lottery" ]
         ]
        ++ [ styled div
                [ displayFlex ]
                []
                (model.participants |> List.map (\participant -> renderParticipant participant))
            ]
        ++ [ input
                [ value <| toString <| List.length model.participants
                , type_ "number"
                , onInput (\val -> String.toInt val |> Result.withDefault 20 |> ChangeParticipantsNumber)
                ]
                []
            , button
                [ onClick Play ]
                [ text "play" ]
            ]
        )


participantStyle =
    batch
        [ padding (Css.rem 1) ]


selectedStyle =
    batch
        [ participantStyle
        , backgroundColor <| hex "ff0000"
        , color <| hex "ffffff"
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



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.Styled.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
