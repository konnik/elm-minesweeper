module Minesweeper exposing
    ( Board
    , Cell
    , Celltype(..)
    , Game
    , Pos
    , Result(..)
    , State(..)
    , reveal
    , show
    , start
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
    = InProgress
    | Ended Result


type Result
    = Looser Pos
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
    { pos : Pos
    , celltype : Celltype
    }


type Celltype
    = Bomb
    | BombExploded
    | Empty Int
    | Hidden



-- PUBLIC API


{-| Start a new game width the specified board size and number of bombs.
-}
start :
    { width : Int
    , height : Int
    , bombs : Int
    }
    -> (Game -> msg)
    -> Cmd msg
start { width, height, bombs } toMsg =
    let
        bombsClamped =
            max 0 (min (width * height) bombs)
    in
    boardPositions { width = width, height = height }
        |> Random.List.shuffle
        |> Random.map
            (\shuffledPos ->
                { width = width
                , height = height
                , bombs = Set.empty
                , numBombs = bombsClamped
                , visible = Set.empty
                , gameState = InProgress
                , clicks = 0
                , shuffledPositions = shuffledPos
                }
            )
        |> Random.generate toMsg


{-| Reveal a the contents of a hidden cell.

If the cell contains a bomb the game is over.
If all empty cells are revealed the game is won.

-}
reveal : Pos -> Game -> Game
reveal pos game =
    case game.gameState of
        InProgress ->
            game
                |> initBombsOnFirstClick pos
                |> countClicks
                |> winOrLoose pos

        _ ->
            game


{-| Returns a description of the current game that can be used to
render the game board as html.
-}
show : Game -> Board
show board =
    let
        celltypeAt : Pos -> Celltype
        celltypeAt pos =
            case ( isRevealed board pos, isBomb board pos ) of
                ( False, _ ) ->
                    Hidden

                ( True, False ) ->
                    Empty (countAdjacentBombs board pos)

                ( True, True ) ->
                    case board.gameState of
                        Ended (Looser gameOverCell) ->
                            if gameOverCell == pos then
                                BombExploded

                            else
                                Bomb

                        _ ->
                            Bomb

        cells : List Cell
        cells =
            boardPositions
                { width = board.width
                , height = board.height
                }
                |> List.indexedMap (\i p -> Cell p (celltypeAt p))
    in
    { width = board.width
    , height = board.height
    , cells = cells
    , state = board.gameState
    }



-- PRIVATE API


{-| Generates a list of Pos for all positions of a game board of size
width\*height.
-}
boardPositions : { width : Int, height : Int } -> List Pos
boardPositions { width, height } =
    List.range 0 (width * height - 1)
        |> List.map (\i -> ( modBy width i, i // width ))


{-| Checks if the game has ended.
-}
winOrLoose : Pos -> Game -> Game
winOrLoose pos game =
    if isBomb game pos then
        game
            |> revealAll
            |> endGameWithResult (Looser pos)

    else
        autoReveal pos game |> checkWin


{-| Checks if the this is the first click in the game.

If so it generates the bomb locations making sure that
the clicked position does not contain a bomb. The first click should always
be safe.

-}
initBombsOnFirstClick : Pos -> Game -> Game
initBombsOnFirstClick pos game =
    let
        randomBombs =
            game.shuffledPositions
                |> List.filter (\p -> p /= pos)
                |> List.take game.numBombs
                |> Set.fromList
    in
    case game.clicks of
        0 ->
            { game
                | bombs = randomBombs
            }

        _ ->
            game


{-| Add one to the number of clicks in the current game
-}
countClicks : Game -> Game
countClicks game =
    { game | clicks = game.clicks + 1 }


{-| Check if all empty cells have been revealed.
-}
checkWin : Game -> Game
checkWin game =
    let
        allRevealed =
            Set.size game.visible + Set.size game.bombs == game.width * game.height
    in
    case ( game.gameState, allRevealed ) of
        ( InProgress, True ) ->
            game |> endGameWithResult Winner

        _ ->
            game


{-| Ends the game with the provided result.
-}
endGameWithResult : Result -> Game -> Game
endGameWithResult result game =
    { game
        | gameState = Ended result
    }


{-| Reveal all cells on the game board.
-}
revealAll : Game -> Game
revealAll game =
    { game
        | visible = Set.fromList (boardPositions { width = game.width, height = game.height })
    }


{-| Automatically reveals all neighbouring cells of a cell that has
a bomb count of 0.
-}
autoReveal : Pos -> Game -> Game
autoReveal pos board =
    let
        newBoard =
            { board | visible = Set.insert pos board.visible }
    in
    if isRevealed board pos || not (isInsideGameArea board pos) then
        board

    else
        case countAdjacentBombs board pos of
            0 ->
                Set.foldl autoReveal newBoard (adjacentCells pos)

            _ ->
                newBoard


{-| Generates the 8 adjacent positions of a cell.
-}
adjacentCells : Pos -> Set Pos
adjacentCells pos =
    [ ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( -1, 0 ), ( 1, 0 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]
        |> List.map (offset pos)
        |> Set.fromList


{-| Count the number of bombs adjacent to a particular cell position.
-}
countAdjacentBombs : Game -> Pos -> Int
countAdjacentBombs board pos =
    Set.intersect (adjacentCells pos) board.bombs
        |> Set.size


{-| Adds two positions together.
-}
offset : Pos -> Pos -> Pos
offset ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


{-| Predicate that tells if a position is inside the game area.
-}
isInsideGameArea : Game -> Pos -> Bool
isInsideGameArea board ( x, y ) =
    x >= 0 && x < board.width && y >= 0 && y < board.height


{-| Predicate that tells if a cell position contains a bomb.
-}
isBomb : Game -> Pos -> Bool
isBomb board pos =
    Set.member pos board.bombs


{-| Predicate that tells if a cell position has been revealed or not.
-}
isRevealed : Game -> Pos -> Bool
isRevealed board pos =
    Set.member pos board.visible
