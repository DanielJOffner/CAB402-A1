namespace QUT

    module FSharpImpureTicTacToeModel =

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
                    |> List.find(fun (rowInt,colInt,playerString) -> rowInt = row && colInt = col)
                    |> fun(rowInt,colInt,playerString) -> playerString
                    

        // returns a string to represent a player piece on the board
        // "X" for Cross, "O" for Nought and "" for none
        let getPiece (player: Player) : string =
            match player with
            | Nought -> "O"
            | Cross -> "X"

        // returns a string representing which player occupies a given row col
        let getPlayerStringAt (game:GameState) row col : string =
            game.Board
            |> List.find(fun (rowInt,colInt,playerString) -> rowInt = row && colInt = col)
            |> fun(rowInt,colInt,playerString) -> playerString

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
        // each line is a sequence of row,col coordinates 
        // example output : seq [seq[(0,0);(0,1)];seq[(1,0);(1,1) 
        let Lines (size:int) : seq<seq<int*int>> = 
            let allPossibleCoordinates = seq { for row in 0 .. size-1 do for col in 0 .. size-1 do yield(row, col)}
            let getLine filter : seq<int*int> =
                allPossibleCoordinates
                |> Seq.filter filter

            let horizontalLines = seq { for i in 0 .. size-1 do yield getLine (fun (row,col) -> row = i)}
            let verticalLines = seq { for i in 0 .. size-1 do yield getLine (fun (row,col) -> col = i)}
            let diagonalLeftToRight = seq { for i in 0 .. size-1 do yield getLine (fun (row,col) -> row = col)}
            let diagonalRightToLeft = seq { for i in 0 .. size-1 do yield getLine (fun (row,col) -> row + col = size-1)}

            Seq.concat [ horizontalLines; verticalLines; diagonalLeftToRight; diagonalRightToLeft]

        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = 
            let lineAsPlayerStrings =
                line
                |> Seq.map(fun (row, col) -> getPlayerStringAt game row col) //represent line is seq<string> "", "X" or "O"

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


 
        let GameStart (firstPlayer:Player) size =
            { 
            Turn = firstPlayer; 
            Size = size; 
            Board = Seq.toList <| seq {for row in 0 ..size-1 do for col in 0 .. size-1 do yield(row, col, "")}
            }                                              

        //**START helper functions for MiniMax//
        // +1 for a win, -1 for a loss, 0 for a draw 
        let heuristic game perspectve =
            match GameOutcome game with
            | Draw -> 0
            | Win (player, line) ->
                if player = perspectve then +1
                else -1
            | Undecided -> raise (System.Exception("A heuristic should only be evaluted when the game is over (not undecided)"))

        // determine which player's turn it is for a given game state
        let getTurn game = game.Turn

        // determine if a game is over
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

        let MiniMax game = 
            let MiniMaxFunction = GameTheory.MiniMaxGenerator heuristic getTurn gameOver moveGenerator ApplyMove
            let bestMove = MiniMaxFunction game game.Turn
            match bestMove with
            | (move, score) ->
                match move with
                | Some move -> move
                | _ -> raise (System.Exception("MiniMax should not be called if there are no moves left"))
                


        let MiniMaxWithPruning game = 
            let MiniMaxFunction = GameTheory.MiniMaxWithAlphaBetaPruningGenerator heuristic getTurn gameOver moveGenerator ApplyMove
            let bestMove = MiniMaxFunction -1 1 game game.Turn
            match bestMove with
            | (move, score) ->
              match move with
              | Some move -> move  
              | _ -> raise (System.Exception("MiniMax should not be called if there are no moves left"))    

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
            override this.FindBestMove(game) = MiniMax game
            

        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = MiniMaxWithPruning game


 
 //type Player = Nought | Cross | None

 ////  Returns a player represented as a string: "O" for Nought, "X" for Cross, "" for None
 //let getPlayerPiece player =
 //    match player with
 //    | Nought -> "O"
 //    | Cross -> "X"
 //    | None -> ""
   

 //type GameState = 
 //    { Turn: Player; Size: int; Board: array<array<Player>>} 
 //    interface ITicTacToeGame<Player> with
 //        member this.Turn with get()    = this.Turn
 //        member this.Size with get()    = this.Size
 //        member this.getPiece(row, col) = this.Board.[row].[col] |> getPlayerPiece

 //type Move = 
 //    { Row: int; Column: int }
 //    interface ITicTacToeMove with
 //        member this.Row with get() = this.Row
 //        member this.Col with get() = this.Column

 //// Swap Turns from Nought to Cross or Cross to Nought
 //let SwapPlayerTurn turn =
 //    match turn with
 //    | Nought -> Cross
 //    | Cross -> Nought
 //    | _ -> raise (System.Exception("Expected Nought or Cross"))

 //// Generate an empty board of array<array<Player>> where each row,col = None
 //let GetEmptyBoard size =
 //    [| for row in 1 .. size -> 
 //        [| for col in 1 .. size ->
 //            None |] |]

 //// takes a sequence of outcomes seq<TicTacToeOutcome>)
 //// returns true if a payer has won or false if no player has won 
 //let isGameWon outcomes =
 //    outcomes
 //    |> Seq.exists (fun outcome -> 
 //         match outcome with
 //         | Win (a, b) -> a = Nought || a = Cross
 //         | _ -> false)

 //// takes a sequence of outcomes seq<TicTacToeOutcome>
 //// returns the player that has won the game
 //let getWinner outcomes =
 //    outcomes 
 //    |> Seq.find (fun outcome -> 
 //        match outcome with
 //        | Win (a, b) -> a = Nought || a = Cross
 //        | _ -> false)

 //// Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
 //// each line is a sequence of row,col coordinates 
 //// example output : seq [seq[(0,0);(0,1)];seq[(1,0);(1,1) 
 //let Lines (size:int) : seq<seq<int*int>> = 
 //    let allPossibleCoordinates = seq { for row in 0 .. size-1 do for col in 0 .. size-1 do yield(row, col)}
 //    let getLine filter : seq<int*int> =
 //        allPossibleCoordinates
 //        |> Seq.filter filter

 //    let horizontalLines = seq { for i in 0 .. size-1 do yield getLine (fun (row,col) -> row = i)}
 //    let verticalLines = seq { for i in 0 .. size-1 do yield getLine (fun (row,col) -> col = i)}
 //    let diagonalLeftToRight = seq { for i in 0 .. size-1 do yield getLine (fun (row,col) -> row = col)}
 //    let diagonalRightToLeft = seq { for i in 0 .. size-1 do yield getLine (fun (row,col) -> row + col = size-1)}

 //    Seq.concat [ horizontalLines; verticalLines; diagonalLeftToRight; diagonalRightToLeft]

 //// Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
 //// has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
 //let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = 
 //    let lineAsPlayerStrings =
 //        line
 //        |> Seq.map(fun (row, col) -> game.Board.[row].[col] |> getPlayerPiece) //represent line is seq<string> "", "X" or "O"

 //    let noughtCount =
 //        lineAsPlayerStrings
 //        |> Seq.filter(fun (playerString) -> playerString = "O")
 //        |> fun noughts -> Seq.length noughts

 //    let crossCount =
 //        lineAsPlayerStrings
 //        |> Seq.filter(fun (playerString) -> playerString = "X")
 //        |> fun crosses -> Seq.length crosses

 //    if (noughtCount = game.Size) then Win (winner = Nought, line = line)
 //    else if (crossCount = game.Size) then Win (winner = Cross, line = line)
 //    else if (noughtCount >= 1 && crossCount >= 1) then Draw
 //    else Undecided
     

 //let GameOutcome game = 
 //    let outcomes =
 //        Lines game.Size
 //        |> Seq.map (fun line -> CheckLine game line)

 //    if outcomes |> Seq.forall (fun outcome -> outcome = Draw) then Draw 
 //    else if outcomes |> isGameWon then outcomes |> getWinner
 //    else Undecided

 //let ApplyMove game move  = 
 //    // change the board state to apply the move at position Row,Col
 //    let newBoardState = game.Board
 //    newBoardState.[move.Row].[move.Column] <- game.Turn

 //    // Return the new game and swap the player
 //    {
 //    Turn = game.Turn |> SwapPlayerTurn; 
 //    Size = game.Size;
 //    Board = newBoardState
 //    }

 //let CreateMove row col   =  {Row = row; Column = col}
 

 //let GameStart first size = { 
 //    Turn = first; 
 //    Size = size; 
 //    Board = GetEmptyBoard size 
 //    }   

 ////**START helper functions for MiniMax//
 //// +1 for a win, -1 for a loss, 0 for a draw 
 //let heuristic game perspectve =
 //    match GameOutcome game with
 //    | Draw -> 0
 //    | Win (player, line) ->
 //        if player = perspectve then +1
 //        else -1
 //    | Undecided -> raise (System.Exception("A heuristic should only be evaluted when the game is over (not undecided)"))

 //// determine which player's turn it is for a given game state
 //let getTurn game = game.Turn

 //// determine if a game is over
 //let gameOver game =
 //    match GameOutcome game with
 //    | Undecided -> false
 //    | _ -> true


 //let moveGenerator game : seq<Move> =
 //    let gameCoordinates = seq {for row in 0 ..game.Size-1 do for col in 0 .. game.Size-1 do yield(row, col)}
 //    let moves =
 //        gameCoordinates
 //        |> Seq.map (fun (row,col) -> CreateMove row col)
 //        |> Seq.filter (fun move -> game.Board.[move.Row].[move.Column] = None)
 //    moves
      

 ////**END helper functions for MiniMax//

 //let FindBestMove game    = 
 //    let MiniMaxFunction = GameTheory.MiniMaxWithAlphaBetaPruningGenerator heuristic getTurn gameOver moveGenerator ApplyMove
 //    let bestMove = MiniMaxFunction -1 1 game game.Turn
 //    match bestMove with
 //    | (move, score) ->
 //      match move with
 //      | Some move -> move  
 //      | _ -> raise (System.Exception("MiniMax should not be called if there are no moves left"))   

      
 //// plus other helper functions ...





 //type WithAlphaBetaPruning() =
     //override this.ToString()         = "Impure F# with Alpha Beta Pruning";
     //interface ITicTacToeModel<GameState, Move, Player> with
         //member this.Cross with get()             = Cross
         //member this.Nought with get()            = Nought
         //member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
         //member this.CreateMove(row, col)         = CreateMove row col
         //member this.GameOutcome(game)            = GameOutcome game 
         //member this.ApplyMove(game, move)        = ApplyMove game  move
         //member this.FindBestMove(game)           = FindBestMove game
