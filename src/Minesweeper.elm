module Minesweeper exposing
    ( Board
    , Cell
    , Celltype(..)
    , Game
    , Pos
    , State(..)
    , cellClicked
    , getCurrentBoard
    , startNewGame
    )

import Random exposing (Generator)
import Random.List
import Set exposing (Set)
import Task


type alias Game =
    { width : Int
    , height : Int
    , numBombs : Int
    , shuffledPositions : List Pos
    , bombs : Set Pos
    , visible : Set Pos
    , gameState : State
    , clicks : Int
    }


type State
    = NotStarted
    | InProgress
    | GameOver Pos
    | Winner


type alias Pos =
    ( Int, Int )


type alias Board =
    { width : Int
    , height : Int
    , cells : List Cell
    , state : State
    }


type alias Cell =
    { index : Int
    , pos : Pos
    , celltype : Celltype
    }


type Celltype
    = Bomb
    | BombExploded
    | Empty Int
    | Hidden


startNewGame : Int -> Int -> Int -> (Game -> msg) -> Cmd msg
startNewGame width height numBombs toMsg =
    let
        numBombsClamped =
            max 0 (min (width * height) numBombs)
    in
    positions width height
        |> Random.List.shuffle
        |> Random.map
            (\shuffledPos ->
                { width = width
                , height = height
                , bombs = Set.empty
                , numBombs = numBombs
                , visible = Set.empty
                , gameState = InProgress
                , clicks = 0
                , shuffledPositions = shuffledPos
                }
            )
        |> Random.generate toMsg


positions : Int -> Int -> List Pos
positions width height =
    List.range 0 (width * height - 1)
        |> List.map (\i -> ( modBy width i, i // width ))


cellClicked : Pos -> Game -> Game
cellClicked pos game =
    case game.gameState of
        InProgress ->
            game
                |> initBombsOnFirstClick pos
                |> countClicks
                |> winOrLoose pos

        _ ->
            game


winOrLoose : Pos -> Game -> Game
winOrLoose pos game =
    if isBomb game pos then
        gameOver game pos

    else
        reveal pos game |> checkWin


initBombsOnFirstClick : Pos -> Game -> Game
initBombsOnFirstClick pos game =
    case game.clicks of
        0 ->
            { game | bombs = game.shuffledPositions |> List.filter (\p -> p /= pos) |> List.take game.numBombs |> Set.fromList }

        _ ->
            game


countClicks : Game -> Game
countClicks game =
    { game | clicks = game.clicks + 1 }


checkWin : Game -> Game
checkWin board =
    if Set.size board.visible + Set.size board.bombs == board.width * board.height then
        { board
            | gameState = Winner
            , visible = Set.fromList (positions board.width board.height)
        }

    else
        board


gameOver : Game -> Pos -> Game
gameOver board pos =
    { board
        | gameState = GameOver pos
        , visible = Set.fromList (positions board.width board.height)
    }


reveal : Pos -> Game -> Game
reveal pos board =
    let
        newGame =
            { board | visible = Set.insert pos board.visible }
    in
    if isVisible board pos || not (insideGame board pos) then
        board

    else
        case adjacentBombs board pos of
            0 ->
                Set.foldl reveal newGame (adjacentCells pos)

            _ ->
                newGame


adjacentCells : Pos -> Set Pos
adjacentCells pos =
    [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]
        |> List.map (offset pos)
        |> Set.fromList


insideGame : Game -> Pos -> Bool
insideGame board ( x, y ) =
    x >= 0 && x < board.width && y >= 0 && y < board.height


adjacentBombs : Game -> Pos -> Int
adjacentBombs board pos =
    Set.intersect (adjacentCells pos) board.bombs
        |> Set.size


offset : Pos -> Pos -> Pos
offset ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


isBomb : Game -> Pos -> Bool
isBomb board pos =
    Set.member pos board.bombs


isVisible : Game -> Pos -> Bool
isVisible board pos =
    Set.member pos board.visible


getCurrentBoard : Game -> Board
getCurrentBoard board =
    let
        celltypeAt : Pos -> Celltype
        celltypeAt pos =
            case ( isVisible board pos, isBomb board pos ) of
                ( False, _ ) ->
                    Hidden

                ( True, False ) ->
                    Empty (adjacentBombs board pos)

                ( True, True ) ->
                    case board.gameState of
                        GameOver gameOverCell ->
                            if gameOverCell == pos then
                                BombExploded

                            else
                                Bomb

                        _ ->
                            Bomb

        cells : List Cell
        cells =
            positions board.width board.height
                |> List.indexedMap (\i p -> Cell i p (celltypeAt p))
    in
    { width = board.width
    , height = board.height
    , cells = cells
    , state = board.gameState
    }
