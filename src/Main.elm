module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, height, style)
import Html.Events exposing (onClick)
import List.Extra exposing (groupsOf)
import Minesweeper exposing (Board, Cell, Celltype(..), Game, Pos, State(..))
import Random exposing (Generator)
import Random.List exposing (shuffle)
import Set exposing (Set)


type alias Model =
    { width : Int
    , height : Int
    , bombs : Int
    , game : Maybe Game
    }


type Msg
    = GameStarted Game
    | CellClicked Pos
    | PlayAgain


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : () -> ( Model, Cmd Msg )
init _ =
    { width = 10
    , height = 10
    , bombs = 10
    , game = Nothing
    }
        |> startNewGame


startNewGame : Model -> ( Model, Cmd Msg )
startNewGame model =
    ( { model | game = Nothing }
    , Minesweeper.startNewGame model.width model.height model.bombs GameStarted
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameStarted newGame ->
            ( { model | game = Just newGame }, Cmd.none )

        CellClicked pos ->
            case model.game of
                Just game ->
                    ( { model | game = Just (Minesweeper.cellClicked pos game) }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        PlayAgain ->
            startNewGame model


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "content" ]
            [ case model.game of
                Just game ->
                    Minesweeper.getCurrentBoard game |> gameView

                Nothing ->
                    text "No game yet..."
            ]
        ]


gameView : Board -> Html Msg
gameView board =
    div
        []
        [ topPanelView
        , boardView board
        , bottomPanelView board
        ]


bottomPanelView : Board -> Html Msg
bottomPanelView board =
    div
        [ class "button-container"
        ]
        [ div
            [ style "display"
                (if displayPlayAgain board then
                    "inline-block"

                 else
                    "none"
                )
            ]
            [ case board.state of
                Winner ->
                    div [ class "result-win" ] [ text "SUCCESS!" ]

                GameOver _ ->
                    div [ class "result-loose" ] [ text "YOU LOOSE!" ]

                _ ->
                    div [ class "result-loose" ] [ text "-" ]
            , button
                [ class "play-again-btn"
                , onClick PlayAgain
                ]
                [ text "Play again!" ]
            ]
        ]


displayPlayAgain : Board -> Bool
displayPlayAgain board =
    case board.state of
        GameOver _ ->
            True

        Winner ->
            True

        _ ->
            False


topPanelView : Html Msg
topPanelView =
    h1 [] [ text "MINESWEEPER" ]


boardView : Board -> Html Msg
boardView board =
    div [ class "minesweeper-grid" ] (List.map (cellView board.state) board.cells)


cellView : State -> Minesweeper.Cell -> Html Msg
cellView state cell =
    case cell.celltype of
        Bomb ->
            divWith state cell.pos [ class "bomb" ]

        BombExploded ->
            divWith state cell.pos [ class "bomb", class "exploded" ]

        Empty n ->
            divWith state cell.pos [ class ("empty" ++ String.fromInt n) ]

        Hidden ->
            divWith state cell.pos [ class "hidden", onClick (CellClicked cell.pos) ]


divWith : State -> Pos -> List (Html.Attribute Msg) -> Html Msg
divWith state ( x, y ) attrs =
    let
        flashClass =
            case state of
                GameOver _ ->
                    [ class "gameOver" ]

                Winner ->
                    [ class "winner" ]

                _ ->
                    []
    in
    div
        (List.concat
            [ attrs
            , [ Html.Attributes.attribute "data-col" (String.fromInt (x + 1))
              , Html.Attributes.attribute "data-row" (String.fromInt (y + 1))
              , class "cell"
              ]
            , flashClass
            ]
        )
        []
