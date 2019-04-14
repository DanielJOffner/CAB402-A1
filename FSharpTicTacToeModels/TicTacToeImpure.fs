namespace QUT

    module FSharpImpureTicTacToeModel =

        type Player = Nought | Cross | None

        //  Returns a player represented as a string: "O" for Nought, "X" for Cross, "" for None
        let getPlayerPiece player =
            match player with
            | Nought -> "O"
            | Cross -> "X"
            | None -> ""
          

        type GameState = 
            { Turn: Player; Size: int; Board: array<array<Player>>} 
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.Turn
                member this.Size with get()    = this.Size
                member this.getPiece(row, col) = this.Board.[row].[col] |> getPlayerPiece

        type Move = 
            { Row: int; Column: int }
            interface ITicTacToeMove with
                member this.Row with get() = this.Row
                member this.Col with get() = this.Column

        // Swap Turns from Nought to Cross or Cross to Nought
        let swapPlayerTurn turn =
            match turn with
            | Nought -> Cross
            | Cross -> Nought
            | _ -> raise (System.Exception("Expected Nought or Cross"))

        // Generate an empty board of array<array<Player>> where each row,col = None
        let getEmptyBoard size =
            [| for row in 1 .. size -> 
                [| for col in 1 .. size ->
                    None |] |]
               
        

        let GameOutcome game     = raise (System.NotImplementedException("GameOutcome"))

        let ApplyMove game move  = 
            // change the board state to apply the move at position Row,Col
            let newBoardState = game.Board
            newBoardState.[move.Row].[move.Column] <- game.Turn

            // Return the new game and swap the player
            {
            Turn = game.Turn |> swapPlayerTurn; 
            Size = game.Size;
            Board = newBoardState
            }

        let CreateMove row col   =  {Row = row; Column = col}

        let FindBestMove game    = raise (System.NotImplementedException("FindBestMove"))

        let GameStart first size = { 
            Turn = first; 
            Size = size; 
            Board = getEmptyBoard size 
            }   


             
        // plus other helper functions ...





        type WithAlphaBetaPruning() =
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross
                member this.Nought with get()            = Nought
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game 
                member this.ApplyMove(game, move)        = ApplyMove game  move
                member this.FindBestMove(game)           = FindBestMove game