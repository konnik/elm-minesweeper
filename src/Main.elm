module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Minesweeper exposing (Board, Cell, Celltype(..), Game, Pos, State(..))
import Random exposing (Generator)
import Random.List exposing (shuffle)
import Set exposing (Set)


type alias Model =
    { progress : GameState }


type GameState
    = WaitingForFirstClick Int Int Int
    | Playing Game


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
    startNewGame


startNewGame : ( Model, Cmd Msg )
startNewGame =
    ( { progress = WaitingForFirstClick 10 10 10 }
    , Cmd.none
    )



-- Minesweeper.startNewGame width height bombs GameStarted


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GameStarted newGame ->
            ( { model | progress = Playing newGame }, Cmd.none )

        CellClicked pos ->
            case model.progress of
                WaitingForFirstClick w h b ->
                    ( model, Minesweeper.startNewGame w h b pos GameStarted )

                Playing game ->
                    ( { model | progress = Playing (Minesweeper.cellClicked pos game) }, Cmd.none )

        PlayAgain ->
            startNewGame


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "content" ]
            [ case model.progress of
                Playing game ->
                    Minesweeper.getCurrentBoard game |> gameView

                WaitingForFirstClick w h _ ->
                    Minesweeper.getDummyBoard w h |> gameView
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
    div [ class "button-container" ]
        [ button
            [ class "play-again-btn"
            , style "display"
                (if displayPlayAgain board then
                    "inline-block"

                 else
                    "none"
                )
            , onClick PlayAgain
            ]
            [ text "Play again!" ]
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
    h1 [] [ text "Elm Minesweeper" ]


boardView : Board -> Html Msg
boardView state =
    div [ class "minesweeper-grid" ] (List.map cellView state.cells)


cellView : Minesweeper.Cell -> Html Msg
cellView cell =
    case cell.celltype of
        Bomb ->
            divWith cell.pos [ class "bomb" ]

        BombExploded ->
            divWith cell.pos [ class "bomb", class "exploded" ]

        Empty n ->
            divWith cell.pos [ class ("empty" ++ String.fromInt n) ]

        Hidden ->
            divWith cell.pos [ class "hidden", onClick (CellClicked cell.pos) ]


divWith : Pos -> List (Html.Attribute Msg) -> Html Msg
divWith ( x, y ) attrs =
    div
        (List.concat
            [ attrs
            , [ Html.Attributes.attribute "data-col" (String.fromInt (x + 1))
              , Html.Attributes.attribute "data-row" (String.fromInt (y + 1))
              , class "cell"
              ]
            ]
        )
        []
