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
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored
            //function alphabeta(node, depth, α, β, maximizingPlayer) is
                            //if depth = 0 or node is a terminal node then
                            //    return the heuristic value of node
                            //if maximizingPlayer then
                            //    value := −∞
                            //    for each child of node do
                            //        value := max(value, alphabeta(child, depth − 1, α, β, FALSE))
                            //        α := max(α, value)
                            //        if α ≥ β then
                            //            break (* β cut-off *)
                            //    return value
                            //else
                                //value := +∞
                                //for each child of node do
                                //    value := min(value, alphabeta(child, depth − 1, α, β, TRUE))
                                //    β := min(β, value)
                                //    if α ≥ β then
                                //        break (* α cut-off *)
                                //return value            
            let rec MiniMax alpha beta oldState perspective = raise (System.NotImplementedException("MiniMaxWithPruning"))
                //NodeCounter.Increment()
                //// If we are at a leaf (terminal) node then return the heuristic score
                //// +1 for a win, -1 for a loss, 0 for a draw
                //if gameOver oldState then (None, heuristic oldState perspective)
                //else 
                    //let maximisingPlayer = (perspective = getTurn oldState)
                    //let possibleMoves = Seq.toList <| moveGenerator oldState //enumerate all possible moves (child nodes) 
                    //if maximisingPlayer then 
                    //    let max = // <- Tuple containing the index of and value of the move with the highest score//
                    //        possibleMoves
                    //        |> List.map (fun move -> applyMove oldState move)
                    //        |> List.iter  
                    //        //|> List.map (fun game -> MiniMax game perspective)
                    //        //|> fun list ->
                    //            //let maxScore =
                    //            //    list
                    //            //    |> List.map (fun (move, score) -> score)
                    //            //    |> List.max
                    //            //let indexOfMaxScore = 
                    //            //    list
                    //            //    |> List.findIndex (fun (move, score) -> score = maxScore)
                    //            //(indexOfMaxScore, maxScore)

                    //    match max with
                    //    | (i, score) -> (Some(possibleMoves.[i]), score)

                    //else // Minimizing player
                        //let min =  // <- Tuple containing the index of and value of the move with the lowest score// 
                        //    possibleMoves
                        //    |> List.map (fun move -> applyMove game move) 
                        //    |> List.map (fun game -> MiniMax game perspective)
                        //    |> fun list ->
                        //        let minScore =
                        //            list
                        //            |> List.map (fun (move, score) -> score)
                        //            |> List.min
                        //        let indexOfMinScore = 
                        //            list
                        //            |> List.findIndex (fun (move, score) -> score = minScore)
                        //        (indexOfMinScore, minScore)

                        //match min with
                        //| (i, score) -> (Some(possibleMoves.[i]), score)
            NodeCounter.Reset()
            MiniMax
