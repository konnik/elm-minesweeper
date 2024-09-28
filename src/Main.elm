module Main exposing (main)

import Board exposing (Board, Cell, Celltype(..), GameState(..), Pos)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, h1, text)
import Html.Attributes exposing (class, style)
import Html.Events exposing (onClick)
import Random exposing (Generator)
import Random.List exposing (shuffle)
import Set exposing (Set)


type alias Model =
    { board : Maybe Board }


type Msg
    = BoardInitialized Board
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
    ( { board = Nothing }
    , Board.init 10 10 10 BoardInitialized
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BoardInitialized newBoard ->
            ( { model | board = Just newBoard }, Cmd.none )

        CellClicked pos ->
            updateBoard model (Board.clickedCell pos)

        PlayAgain ->
            startNewGame


updateBoard : Model -> (Board -> Board) -> ( Model, Cmd Msg )
updateBoard model updateFunc =
    case model.board of
        Just board ->
            ( { model | board = Just (updateFunc board) }, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "content" ]
            [ case model.board of
                Just board ->
                    Board.state board |> gameView

                Nothing ->
                    text "No board yet..."
            ]
        ]


gameView : Board.State -> Html Msg
gameView state =
    div
        [ style "align-items" "center"
        , style "justify-content" "center"
        , style "font-size" "30px"
        ]
        [ gameStateView state.state
        , boardView state
        , bottomPanel state
        ]


bottomPanel : Board.State -> Html Msg
bottomPanel state =
    let
        display =
            case state.state of
                GameOver _ ->
                    "inline-block"

                Win ->
                    "inline-block"

                _ ->
                    "none"
    in
    div [ class "button-container" ]
        [ button
            [ class "play-again-btn"
            , style "display" display
            , onClick PlayAgain
            ]
            [ text "Play again!" ]
        ]


gameStateView : Board.GameState -> Html Msg
gameStateView state =
    h1 [] [ text "Elm Minesweeper" ]


boardView : Board.State -> Html Msg
boardView state =
    div
        [ class "minesweeper-grid" ]
    <|
        List.map cellView state.cells


cellView : Board.Cell -> Html Msg
cellView cell =
    -- <div class="cell empty c6" data-row="1"
    -- data-col="1"></div>
    let
        attrs =
            [ Html.Attributes.attribute "data-col" (String.fromInt (Tuple.first cell.pos + 1))
            , Html.Attributes.attribute "data-row" (String.fromInt (Tuple.second cell.pos + 1))
            , class "cell"
            ]
    in
    case cell.celltype of
        Bomb ->
            div (attrs ++ [ class "bomb" ]) []

        BombExploded ->
            div (attrs ++ [ class "bomb", class "exploded" ]) []

        Empty n ->
            div
                (attrs
                    ++ [ class ("empty" ++ String.fromInt n)
                       ]
                )
                []

        Hidden ->
            div
                (attrs
                    ++ [ class "hidden"
                       , onClick (CellClicked cell.pos)
                       ]
                )
                []



--        button (styles ++ [ onClick (CellClicked cell.pos) ]) [ text "" ]


surroundingBombsText : Int -> String
surroundingBombsText n =
    case n of
        0 ->
            ""

        _ ->
            String.fromInt n
