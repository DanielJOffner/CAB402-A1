namespace QUT

    module FSharpImpureTicTacToeModel =

        // type to represent the two players: Noughts and Crosses
        type Player = Nought | Cross | NoPlayer

        // returns a string to represent a player piece on the board
        // "X" for Cross, "O" for Nought and "" for none
        let getPiece (player: Player) : string =
            match player with
            | Nought -> "O"
            | Cross -> "X"
            | NoPlayer -> ""

        // type to represent a single move specified using (row, column) coordinates of the selected square
        type Move = 
            { mutable Row: int; mutable Column: int }
            interface ITicTacToeMove with
                member this.Row with get() = this.Row
                member this.Col with get() = this.Column

        // type to represent the current state of the game, including the size of the game (NxN), who's turn it is and the pieces on the board
        type GameState = 
            { mutable Turn: Player; Size: int; Board: array<array<Player>> }
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.Turn
                member this.Size with get()    = this.Size
                member this.getPiece(row, col) = this.Board.[row].[col] |> getPiece
                

        // returns a string representing which player occupies a given row col
        // "X" for cross, "O" for nought, "" for no player
        let getPlayerStringAt (game:GameState) row col : string =
            game.Board.[row].[col] |> getPiece

        // takes a sequence of outcomes seq<TicTacToeOutcome>)
        // returns true if a payer has won or false if no player has won 
        let isGameWon outcomes =
            outcomes
            |> Seq.exists (fun outcome -> 
                 match outcome with
                 | Win (a, b) -> a = Nought || a = Cross
                 | _ -> false)

        // takes a sequence of outcomes seq<TicTacToeOutcome>
        // returns the player that has won the game
        let getWinner outcomes =
            outcomes 
            |> Seq.find (fun outcome -> 
                match outcome with
                | Win (a, b) -> a = Nought || a = Cross
                | _ -> false)


        // create a record type representing a move on the board
        let CreateMove row col = {Row = row; Column = col}


        // swaps the players turn
        let swapTurn turn =
            match turn with
            | Nought -> Cross
            | Cross -> Nought
            | NoPlayer -> raise (System.Exception("game should only ever be started with Nought or Cross as a player"))

        // undo a move on the board and swap back to the last player
        let UndoMove (game: GameState) (move: Move) =
            game.Board.[move.Row].[move.Column] <- Player.NoPlayer
            game.Turn <- (game.Turn |> swapTurn)

        // apply a given move to the board and swap the turn
        let ApplyMove (game: GameState) (move: Move) = 
            game.Board.[move.Row].[move.Column] <- game.Turn
            game.Turn <- (game.Turn |> swapTurn)
            game

        // creates a new line on the board by applying a filter to a given set of coordinates
        let getLine coordinates filter : seq<int*int> =
            coordinates
            |> Seq.filter filter

        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        // each line is a sequence of row,col coordinates 
        // example output : seq [seq[(0,0);(0,1)];seq[(1,0);(1,1) 
        let Lines (size:int) : seq<seq<int*int>> = 
            let allPossibleCoordinates = seq { for row in 0 .. size-1 do for col in 0 .. size-1 do yield(row, col)}

            let horizontalLines = seq { for i in 0 .. size-1 do yield getLine allPossibleCoordinates (fun (row,col) -> row = i)}
            let verticalLines = seq { for i in 0 .. size-1 do yield getLine allPossibleCoordinates (fun (row,col) -> col = i)}
            let diagonalLeftToRight = seq { for i in 0 .. size-1 do yield getLine allPossibleCoordinates (fun (row,col) -> row = col)}
            let diagonalRightToLeft = seq { for i in 0 .. size-1 do yield getLine allPossibleCoordinates (fun (row,col) -> row + col = size-1)}

            Seq.concat [ horizontalLines; verticalLines; diagonalLeftToRight; diagonalRightToLeft]


        // finds the players along a line (sequence of row,col coordinates)
        // "O" for nought, "X" for cross and "" for no player
        let findPlayersAlongLine game line =
            line
            |> Seq.map(fun (row, col) -> getPlayerStringAt game row col)

        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = 
            let lineAsPlayerStrings = findPlayersAlongLine game line
            let noughtCount = 
                lineAsPlayerStrings
                |> Seq.filter(fun (playerString) -> playerString = "O")
                |> fun noughts -> Seq.length noughts
            let crossCount =
                lineAsPlayerStrings
                |> Seq.filter(fun (playerString) -> playerString = "X")
                |> fun crosses -> Seq.length crosses

            if (noughtCount = game.Size) then Win (winner = Nought, line = line)
            else if (crossCount = game.Size) then Win (winner = Cross, line = line)
            else if (noughtCount >= 1 && crossCount >= 1) then Draw
            else Undecided


        // evaluate the current outcome of the game Win, Daw or Undecided
        let GameOutcome (game:GameState) : TicTacToeOutcome<Player> = 
            let outcomes =
                Lines game.Size
                |> Seq.map (fun line -> CheckLine game line)

            if outcomes |> Seq.forall (fun outcome -> outcome = Draw) then Draw 
            else if outcomes |> isGameWon then outcomes |> getWinner
            else Undecided

        // generate an empty board of array<array<Player>> where each row,col = None
        let GetEmptyBoard size =
            [| for row in 1 .. size -> 
                [| for col in 1 .. size ->
                    NoPlayer |] |]
                    

        // returns a new game where each square is empty
        let GameStart first size = { 
            Turn = first; 
            Size = size; 
            Board = GetEmptyBoard size 
            }                                            

        //*****************************************************************************************//
        //*************************  Helper functions for MiniMax only ****************************//
        //*****************************************************************************************//

        // heuristic to evalute the outcome of a game  
        // +1 for a win, -1 for a loss, 0 for a draw 
        let heuristic game perspectve =
            match GameOutcome game with
            | Draw -> 0
            | Win (player, line) ->
                if player = perspectve then +1
                else -1
            | Undecided -> raise (System.Exception("A heuristic should only be evaluted when the game is over (not undecided)"))

        // returns the current players turn from a given game
        let getTurn game = game.Turn

        // determine if a game is over
        let gameOver game =
            match GameOutcome game with
            | Undecided -> false
            | _ -> true

        // enumarate all possible moves from a given board state 
        let moveGenerator game : seq<Move> =
            let gameCoordinates = seq {for row in 0 ..game.Size-1 do for col in 0 .. game.Size-1 do yield(row, col)}
            let moves =
                gameCoordinates
                |> Seq.map (fun (row,col) -> CreateMove row col)
                |> Seq.filter (fun move -> game.Board.[move.Row].[move.Column] = NoPlayer)
            moves

        let MinInt = -2147483648
        let MaxInt = 2147483647

        let printboard (board:array<array<Player>>) =
            for row in 0 ..2 do 
                printf "["
                for col in 0 .. 2 do 
                    let piece = getPiece board.[row].[col]
                    if piece = "X" || piece = "O" then
                        printf "%s" piece
                    else printf "  "
                printfn "]"

        //*****************************************************************************************//
        //*********************  END helper functions for MiniMax only ****************************//
        //*****************************************************************************************//


        // recursive MiniMax function with Alpha Beta pruning https://en.wikipedia.org/wiki/Minimax
        // returns the best move and the score associated with that move 
        let rec MiniMax2 game perspective alpha beta = 
            NodeCounter.Increment()
            if gameOver game then (None, heuristic game perspective)
            else
                let maximisingPlayer = perspective = (getTurn game)
                let moves = moveGenerator game |> Seq.toArray
                let mutable _score = 0
                let mutable _alpha = alpha
                let mutable _beta = beta
                let mutable _i = 0
                let mutable bestMove = CreateMove 0 0
                if maximisingPlayer then
                    _score <- MinInt
                    _i <- 0
                    while _i < moves.Length do
                        let move = moves.[_i]
                        let nodeScore = MiniMax2 (ApplyMove game move) perspective _alpha beta |> fun (move, score) -> score 
                        if nodeScore > _score then                  //  if the best move so far was found - update it
                            bestMove.Row <- move.Row 
                            bestMove.Column <- move.Column
                            //bestMove <- CreateMove 0 1
                        UndoMove game move                          //  undo the move to preserve the board state

                        
                        _score <- max _score nodeScore
                        _alpha <- max _alpha _score
                        if _beta <= _alpha then _i <- moves.Length  //  *if* there are no better moves available -> stop searching
                        else _i <- _i + 1                           //  *else* -> test the the next move
                    (Some(bestMove),_score)
                else //Minimising player
                    _score <- MaxInt
                    _i <- 0
                    while _i < moves.Length do
                        let move = moves.[_i]
                        let nodeScore = MiniMax2 (ApplyMove game move) perspective alpha _beta |> fun (move, score) -> score 
                        if nodeScore < _score then                  //  if the best move so far was found - update it
                            bestMove.Row <- move.Row 
                            bestMove.Column <- move.Column
                        UndoMove game move                          //  undo the move to preserve the board state
                        
                        _score <- min _score nodeScore
                        _beta <- min _beta _score
                        if _beta <= _alpha then _i <- moves.Length  //  *if* there are no better moves available -> stop searching
                        else _i <- _i + 1                           //  *else* -> test the the next move
                    (Some(bestMove),_score)


        let MiniMaxWithPruning game = 
            NodeCounter.Reset()
            let bestMove = MiniMax2 game game.Turn -1 1
            match bestMove with
            | (move, score) ->
              match move with
              | Some move -> move  
              | _ -> raise (System.Exception("MiniMax should not be called if there are no moves left"))    

       

        [<AbstractClass>]
        type Model() =
            abstract member FindBestMove : GameState -> Move
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross 
                member this.Nought with get()            = Nought 
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game
                member this.ApplyMove(game, move)        = ApplyMove game move 
                member this.FindBestMove(game)           = this.FindBestMove game
                

        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = MiniMaxWithPruning game      


 
