namespace QUT

    module FSharpPureTicTacToeModel =
    
        // type to represent the two players: Noughts and Crosses
        type Player = Nought | Cross 
       
        // type to represent a single move specified using (row, column) coordinates of the selected square
        type Move = 
            { Row: int; Column: int }
            interface ITicTacToeMove with
                member this.Row with get() = this.Row
                member this.Col with get() = this.Column

        // type to represent the current state of the game, including the size of the game (NxN), who's turn it is and the pieces on the board
        type GameState = 
            { Turn: Player; Size: int; Board: list<int*int*string>}
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.Turn
                member this.Size with get()    = this.Size
                member this.getPiece(row, col) = 
                    this.Board
                    |> List.filter (fun (a,b,c) -> a = row && b = col)
                    |> fun list -> list.[0]
                    |> fun (a,b,c) -> c

        //**START helper functions**

        // Returns a string to represent a player piece on the board
        // "X" for Cross, "O" for Nought and "" for none
        let getPiece (player: Player) : string =
            match player with
            | Nought -> "O"
            | Cross -> "X"

        // Returns a string representing which player occupies a given row col
        // "O" for Nought, "X" for Cross, "" for none
        let getPlayerAt (gameState:GameState) row col : string =
            gameState.Board
            |> List.filter (fun (a,b,c) -> a = row && b = col)
            |> fun list -> list.[0]
            |> fun (a,b,c) -> c

        // takes a sequence of outcomes (seq<TicTacToeOutcome>) 
        //returns true if a payer has won or false if no player has won 
        let isGameWon outcomes =
            outcomes
            |> Seq.exists (fun outcome -> 
                 match outcome with
                 | Win (a, b) -> a = Nought || a = Cross
                 | _ -> false)

        // takes a sequence of outcomes (seq<TicTacToeOutcome>) 
        //returns the player that has won the game
        let getWinner outcomes =
            outcomes 
            |> Seq.find (fun outcome -> 
                match outcome with
                | Win (a, b) -> a = Nought || a = Cross
                | _ -> false)

        //**END helper functions**

        let CreateMove row col = {Row = row; Column = col}

        let ApplyMove (oldState:GameState) (move: Move) : GameState = 
            let currentPlayerTurn = oldState.Turn
            let newPlayerTurn =
                match oldState.Turn with
                | Nought -> Cross
                | Cross -> Nought
            let newBoardState =
                oldState.Board
                |> List.map (fun (row,col,piece) -> 
                    if row = move.Row && col = move.Column 
                    then (row,col, getPiece currentPlayerTurn) 
                    else (row,col,piece))
            {
            Turn = newPlayerTurn; 
            Size = oldState.Size; 
            Board = newBoardState
            }

        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        // The number of lines returned should always be (size*2+2)
        // the number of squares in each line (represented by (row,column) coordinates) should always be equal to size
        // For example, if the input size = 2, then the output would be: 
        //     seq [seq[(0,0);(0,1)];seq[(1,0);(1,1)];seq[(0,0);(1,0)];seq[(0,1);(1,1)];seq[(0,0);(1,1)];seq[(0,1);(1,0)]]
        // The order of the lines and the order of the squares within each line does not matter
        let Lines (size:int) : seq<seq<int*int>> = 
            let allPossibleCoordinates = seq { for row in 0 .. size-1 do for col in 0 .. size-1 do yield(row, col)}
            let getLine filter : seq<int*int> =
                allPossibleCoordinates
                |> Seq.filter filter

            let horizontalLines = seq { for i in 0 .. size-1 do yield getLine (fun (a,b) -> a = i)}
            let verticalLines = seq { for i in 0 .. size-1 do yield getLine (fun (a,b) -> b = i)}
            let diagonalLeftToRight = seq { for i in 0 .. size-1 do yield getLine (fun (a,b) -> a = b)}
            let diagonalRightToLeft = seq { for i in 0 .. size-1 do yield getLine (fun (a,b) -> a + b = size-1)}
            Seq.concat [ horizontalLines; verticalLines; diagonalLeftToRight; diagonalRightToLeft]

        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = 
            line
            |> Seq.map (fun (row,col) -> getPlayerAt game row col)
            |> Seq.reduce (+)
            |> fun reducedLine ->
                match reducedLine with
                | "OOO" -> Win (winner = Nought, line = line)
                | "XXX" -> Win (winner = Cross, line = line)
                | "XX" -> Undecided
                | "OO" -> Undecided
                | "X" -> Undecided
                | "O" -> Undecided
                | "" -> Undecided
                | _ -> Draw
            //let noughtCount = 
            //    line
            //    |> Seq.map (fun (row,col) -> getPlayerAt game row col)
            //    |> Seq.countBy (fun player -> player = "O") //[(true, count:int); (false, count:int)]
            //    |> Seq.filter (fun (boolean, count) -> boolean = true)
            //    |> Seq.toList 
            //    |> fun list ->
            //        match list.[0]  with
            //        | (boolean,count) -> count 
                    
            //let crossCount = 
            //    line
            //    |> Seq.map (fun (row,col) -> getPlayerAt game row col)
            //    |> Seq.countBy (fun player -> player = "X") //[(true, count:int); (false, count:int)] 
            //    |> Seq.filter (fun (boolean, count) -> boolean = true)
            //    |> Seq.toList 
            //    |> fun list ->
            //        match list.[0]  with
            //        | (boolean,count) -> count 

            //if noughtCount = game.Size then Win (winner = Nought, line = line)
            //else if crossCount = game.Size then Win (winner = Cross, line = line)
            //else if noughtCount + crossCount >= 2 then Draw
            //else Undecided

        let GameOutcome (game:GameState) : TicTacToeOutcome<Player> = 
            let outcomes =
                Lines game.Size
                |> Seq.map (fun line -> CheckLine game line)

            if outcomes |> Seq.forall (fun outcome -> outcome = Draw) then Draw 
            else if outcomes |> isGameWon then outcomes |> getWinner
            else Undecided


 
        let GameStart (firstPlayer:Player) size =
            { 
            Turn = firstPlayer; 
            Size = size; 
            Board = Seq.toList <| seq {for row in 0 ..size-1 do for col in 0 .. size-1 do yield(row, col, "")}
            }                                              

        //**START helper functions for MiniMax//
        // +1 for a win, -1 for a loss, 0 for a draw 
        let heuristic game player =
            match GameOutcome game with
            | Draw -> 0
            | Win (player, line) ->
                if player = player then 1
                else -1

        // determine which player's turn it is for a given game state
        let getTurn game = game.Turn

        // determine whether a game is over
        let gameOver game =
            match GameOutcome game with
            | Undecided -> false
            | _ -> true

        // enumarate all possible moves from a given game situation
        let moveGenerator game : seq<Move> =
            game.Board
            |> Seq.filter (fun (row,col,player) -> player = "") //find only empty spaces on the board
            |> Seq.map (fun (row, col, player) -> CreateMove row col) 
            
        //**END helper functions for MiniMax//

        let MiniMax game = GameTheory.MiniMaxGenerator heuristic getTurn gameOver moveGenerator ApplyMove
        
        let MiniMaxWithPruning game = raise (System.NotImplementedException("MiniMaxWithPruning"))      // calculates a heuristic score for any game situation

        // plus other helper functions ...
        

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

        type BasicMiniMax() =
            inherit Model()
            override this.ToString()         = "Pure F# with basic MiniMax";
            override this.FindBestMove(game) = 
                                    let miniMaxFunction = MiniMax game
                                    miniMaxFunction game game.Turn 
                                    |> fun (move, score) -> 
                                        match move with
                                        | Some move -> move

                                             



        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = raise (System.NotImplementedException("FindBestMove"))