module Board exposing
    ( Board
    , Cell
    , Celltype(..)
    , GameState(..)
    , Pos
    , State
    , clickedCell
    , init
    , state
    )

import Random exposing (Generator)
import Random.List
import Set exposing (Set)
import Task


type alias Board =
    { width : Int
    , height : Int
    , bombs : Set Pos
    , visible : Set Pos
    , gameState : GameState
    }


type GameState
    = InProgress
    | GameOver Pos
    | Win


type alias Pos =
    ( Int, Int )


type alias State =
    { width : Int
    , height : Int
    , cells : List Cell
    , state : GameState
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


init : Int -> Int -> Int -> (Board -> msg) -> Cmd msg
init width height numBombs toMsg =
    let
        numBombsClamped =
            max 0 (min (width * height) numBombs)
    in
    positions width height
        |> Random.List.shuffle
        |> Random.map (List.take numBombsClamped >> Set.fromList)
        |> Random.map
            (\bombs ->
                { width = width
                , height = height
                , bombs = bombs
                , visible = Set.empty
                , gameState = InProgress
                }
            )
        |> Random.generate toMsg


positions : Int -> Int -> List Pos
positions width height =
    List.range 0 (width * height - 1)
        |> List.map (\i -> ( modBy width i, i // width ))


clickedCell : Pos -> Board -> Board
clickedCell pos board =
    case board.gameState of
        InProgress ->
            if isBomb board pos then
                gameOver board pos

            else
                reveal pos board |> checkWin

        _ ->
            board


checkWin : Board -> Board
checkWin board =
    if Set.size board.visible + Set.size board.bombs == board.width * board.height then
        { board
            | gameState = Win
            , visible = Set.fromList (positions board.width board.height)
        }

    else
        board


gameOver : Board -> Pos -> Board
gameOver board pos =
    { board
        | gameState = GameOver pos
        , visible = Set.fromList (positions board.width board.height)
    }


reveal : Pos -> Board -> Board
reveal pos board =
    let
        newBoard =
            { board | visible = Set.insert pos board.visible }
    in
    if isVisible board pos || not (insideBoard board pos) then
        board

    else
        case adjacentBombs board pos of
            0 ->
                Set.foldl reveal newBoard (adjacentCells pos)

            _ ->
                newBoard


adjacentCells : Pos -> Set Pos
adjacentCells pos =
    [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]
        |> List.map (offset pos)
        |> Set.fromList


insideBoard : Board -> Pos -> Bool
insideBoard board ( x, y ) =
    x >= 0 && x < board.width && y >= 0 && y < board.height


adjacentBombs : Board -> Pos -> Int
adjacentBombs board pos =
    Set.intersect (adjacentCells pos) board.bombs
        |> Set.size


offset : Pos -> Pos -> Pos
offset ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


isBomb : Board -> Pos -> Bool
isBomb board pos =
    Set.member pos board.bombs


isVisible : Board -> Pos -> Bool
isVisible board pos =
    Set.member pos board.visible


state : Board -> State
state board =
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
