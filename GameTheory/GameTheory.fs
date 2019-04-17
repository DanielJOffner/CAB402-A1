namespace QUT

    module GameTheory =

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =
            // Basic MiniMax algorithm without using alpha beta pruning
            let rec MiniMax game perspective =
                NodeCounter.Increment()
                // If we are at a leaf (terminal) node then return the heuristic score
                // +1 for a win, -1 for a loss, 0 for a draw
                if gameOver game then (None, heuristic game perspective)
                else
                    let currentTurn = getTurn game
                    let maximisingPlayer = perspective = currentTurn
                    let possibleMoves = Seq.toList <| moveGenerator game //enumerate all possible moves (child nodes) 
                    if maximisingPlayer then 
                        let max =
                            possibleMoves
                            |> List.map (fun move -> (move, applyMove game move))                                                       //output: list[('Move, 'GameState)]
                            |> List.map (fun (move, gameState) -> (move, MiniMax gameState perspective))                                //output: list[('Move, ('Move, 'Score))]
                            |> List.map (fun (move, miniMaxResult) -> match miniMaxResult with | (nextMove, score) -> (move,score))     //output: list[('Move, 'Score)]
                            |> List.maxBy (fun (move, score) -> score)                                                                  //output: ('Move, 'Score)
                            
                        match max with
                        | (move, score) -> (Some(move), score)

                    else // Minimizing player
                        let min =
                            possibleMoves
                            |> List.map (fun move -> (move, applyMove game move))                                                       //output: list[('Move, 'GameState)]
                            |> List.map (fun (move, gameState) -> (move, MiniMax gameState perspective))                                //output: list[('Move, ('Move, 'Score))]
                            |> List.map (fun (move, miniMaxResult) -> match miniMaxResult with | (nextMove, score) -> (move,score))     //output: list[('Move, 'Score)]
                            |> List.minBy (fun (move, score) -> score)                                                                  //output: ('Move, 'Score)

                        match min with
                        | (move, score) -> (Some(move), score)

            NodeCounter.Reset()
            MiniMax

            
        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =       
            let rec MiniMax alpha beta game perspective =
                NodeCounter.Increment()
                // If we are at a leaf (terminal) node then return the heuristic score
                // +1 for a win, -1 for a loss, 0 for a draw
                if gameOver game then (None, heuristic game perspective)
                else
                    let currentTurn = getTurn game
                    let maximisingPlayer = perspective = currentTurn
                    let possibleMoves = Seq.toList <| moveGenerator game //enumerate all possible moves (child nodes) 
                    if maximisingPlayer then 
                        //let rec pruneChildNodes childNodes =
                        let rec pruneNodes childNodes alpha beta =
                            match childNodes with
                            | [] -> raise (System.Exception("Minimax should not be called if the game is over (if there are no more moves available"))
                            | [head] ->  head |> (fun (move, gameState) -> [(move, MiniMax alpha beta gameState perspective)])  // If there only one move available there is nothing to prune 
                            | head::tail ->
                                let lastMove = head |> (fun (move, gameState) -> move)                                          // The last move that lead to this game state 
                                let node = head |> fun (move, gameState) -> MiniMax alpha beta gameState perspective            // Recursively evaluate the score of the first node
                                let nodeScore = node |> fun (nextMove, score) -> score                                                                                            // Extract only the score element of the node
                                let alpha = max nodeScore alpha                                                                 // Get the maximum of the maxScore vs the current alpha 
                                if(beta <= alpha) then [(lastMove, node)] //break 
                                else [(lastMove, node)] @ (pruneNodes tail alpha beta)


                        let childNodes = possibleMoves |> List.map (fun move -> (move, applyMove game move))                      //output: list[('Move, 'GameState)]
                        

                        let max2 =
                            pruneNodes childNodes alpha beta
                            |> List.map (fun (move, miniMaxResult) -> match miniMaxResult with | (nextMove, score) -> (move,score))       //output: list[('Move, 'Score)]
                            |> List.maxBy (fun (move, score) -> score)                                                                    //output: ('Move, 'Score)

                        match max2 with
                        | (move, score) -> (Some(move), score)

                    else // Minimizing player
                        let rec pruneNodes childNodes alpha beta =
                            match childNodes with
                            | [] -> raise (System.Exception("Minimax should not be called if the game is over (if there are no more moves available"))
                            | [head] ->  head |> (fun (move, gameState) -> [(move, MiniMax alpha beta gameState perspective)])  // If there only one move available there is nothing to prune 
                            | head::tail ->
                                let lastMove = head |> (fun (move, gameState) -> move)                                          // The last move that lead to this game state 
                                let node = head |> fun (move, gameState) -> MiniMax alpha beta gameState perspective            // Recursively evaluate the score of the first node
                                let nodeScore = node |> fun (nextMove, score) -> score                                        // Extract only the score element of the node
                                let beta = min nodeScore beta                                                          // Get the maximum of the maxScore vs the current alpha 
                                if(beta <= alpha) then [(lastMove, node)] //break 
                                else [(lastMove, node)] @ (pruneNodes tail alpha beta)

                        let childNodes =  possibleMoves |> List.map (fun move -> (move, applyMove game move)) 

                        let min2 =
                            pruneNodes childNodes alpha beta
                            |> List.map (fun (move, miniMaxResult) -> match miniMaxResult with | (nextMove, score) -> (move,score))
                            |> List.minBy (fun (move, score) -> score)  

                        match min2 with
                        | (move, score) -> (Some(move), score)

            NodeCounter.Reset()
            MiniMax