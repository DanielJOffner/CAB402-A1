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
                        //let rec pruneChildNodes move gameStates =
                        // Recursively evaluate child nodes and stop if alpha >= beta 
                        // Variable definitions:
                        // bestScore - the current best score achieved by any of the child nodes
                        // lastMove - the last move which lead to the current game state 
                        // node - tuple containing the next move and the score of that board state (move, score)
                        // nodeScore - score given to an individual node 
                        // maxScore - the maximum result between the current best score and the nodeScore
                        // maxAlpha - the maximum result between the maxScore and the current alpha value 
                        let rec pruneNodes childNodes bestScore =
                            match childNodes with
                            | [] -> raise (System.Exception("Minimax should not be called if the game is over (if there are no move moves available"))
                            | [head] ->  head |> (fun (move, gameState) -> [(move, MiniMax alpha beta gameState perspective)])  // If there only one move available there is nothing to prune 
                            | head::tail ->
                                let lastMove = head |> (fun (move, gameState) -> move)                                          // The last move that lead to this game state 
                                let node = head |> fun (move, gameState) -> MiniMax alpha beta gameState perspective            // Recursively evaluate the score of the first node
                                let nodeScore = node |> fun (nextMove, score) -> score                                          // Extract only the score element of the node
                                let maxScore = max bestScore nodeScore                                                          // Get the maximum of the current best score vs the node's score
                                let maxAlpha = max nodeScore alpha                                                               // Get the maximum of the maxScore vs the current alpha 
                                if(maxAlpha >= beta) then [(lastMove, node)]
                                else [(lastMove, node)] @ (pruneNodes tail maxScore)


                        // solution is to implement recusive function like map so that each node can be evaluated individually 
                        // rather than mappig the whole List you could 'break' the loop by returning None
                        let childNodes =  possibleMoves |> List.map (fun move -> (move, applyMove game move))                           //output: list[('Move, 'GameState)]

                        //let max =
                            //possibleMoves
                            //|> List.map (fun move -> (move, applyMove game move))                                                       //output: list[('Move, 'GameState)]
                            //|> List.map (fun (move, gameState) -> (move, MiniMax alpha beta gameState perspective))                     //output: list[('Move, ('Move, 'Score))]
                            //|> List.map (fun (move, miniMaxResult) -> match miniMaxResult with | (nextMove, score) -> (move,score))     //output: list[('Move, 'Score)]
                            //|> List.maxBy (fun (move, score) -> score)                                                                  //output: ('Move, 'Score)

                        let max2 =
                            pruneNodes childNodes -100000000
                            |> List.map (fun (move, miniMaxResult) -> match miniMaxResult with | (nextMove, score) -> (move,score))
                            |> List.maxBy (fun (move, score) -> score)  

                        match max2 with
                        | (move, score) -> (Some(move), score)

                    else // Minimizing player
                        let rec pruneNodes childNodes worstScore =
                            match childNodes with
                            | [] -> raise (System.Exception("Minimax should not be called if the game is over (if there are no move moves available"))
                            | [head] ->  head |> (fun (move, gameState) -> [(move, MiniMax alpha beta gameState perspective)])  // If there only one move available there is nothing to prune 
                            | head::tail ->
                                let lastMove = head |> (fun (move, gameState) -> move)                                          // The last move that lead to this game state 
                                let node = head |> fun (move, gameState) -> MiniMax alpha beta gameState perspective            // Recursively evaluate the score of the first node
                                let nodeScore = node |> fun (nextMove, score) -> score                                          // Extract only the score element of the node
                                let minScore = min worstScore nodeScore                                                          // Get the maximum of the current best score vs the node's score
                                let minBeta = min nodeScore beta                                                                 // Get the maximum of the maxScore vs the current alpha 
                                if(alpha >= minBeta) then [(lastMove, node)]
                                else [(lastMove, node)] @ (pruneNodes tail minScore)

                        let childNodes =  possibleMoves |> List.map (fun move -> (move, applyMove game move)) 

                        let min2 =
                            pruneNodes childNodes 100000000
                            |> List.map (fun (move, miniMaxResult) -> match miniMaxResult with | (nextMove, score) -> (move,score))
                            |> List.minBy (fun (move, score) -> score)  

                        match min2 with
                        | (move, score) -> (Some(move), score)

            NodeCounter.Reset()
            MiniMax