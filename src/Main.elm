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


updateBoard : Model -> (Board -> Board) -> ( Model, Cmd Msg )
updateBoard model updateFunc =
    case model.board of
        Just board ->
            ( { model | board = Just (updateFunc board) }, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ case model.board of
            Just board ->
                Board.state board |> gameView

            Nothing ->
                text "No board yet..."
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
        ]


gameStateView : Board.GameState -> Html Msg
gameStateView state =
    case state of
        InProgress ->
            text "Be careful, it's a dangerous world..."

        GameOver _ ->
            text "Game over!"

        Win ->
            text "All mines cleared!!!"


boardView : Board.State -> Html Msg
boardView state =
    div
        [ style "display" "grid"
        , style "grid-template-columns" ("repeat(" ++ String.fromInt state.width ++ ",50px)")
        , style "grid-template-rows" ("repeat(" ++ String.fromInt state.height ++ ",50px)")
        , style "gap" "5px"
        ]
    <|
        List.map cellView state.cells


cellView : Board.Cell -> Html Msg
cellView cell =
    let
        styles =
            [ style "display" "flex"
            , style "align-items" "center"
            , style "justify-content" "center"
            , style "font-size" "30px"
            , style "border" "1px solid #000"
            , style "width" "50px"
            , style "height" "50px"
            ]
    in
    case cell.celltype of
        Bomb ->
            div styles [ text "💣" ]

        BombExploded ->
            div (styles ++ [ style "background-color" "red" ]) [ text "💣" ]

        Empty n ->
            div styles [ text (surroundingBombsText n) ]

        Hidden ->
            button (styles ++ [ onClick (CellClicked cell.pos) ]) [ text "" ]


surroundingBombsText : Int -> String
surroundingBombsText n =
    case n of
        0 ->
            ""

        _ ->
            String.fromInt n
