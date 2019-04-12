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
                        let rec checkChildNodes move gameStates =
                        // solution is to implement recusive function like map so that each node can be evaluated individually 
                        // rather than mappig the whole List you could 'break' the loop by returning None
                        let max =
                            possibleMoves
                            |> List.map (fun move -> (move, applyMove game move))                                                       //output: list[('Move, 'GameState)]
                            |> List.map (fun (move, gameState) -> (move, MiniMax alpha beta gameState perspective))                     //output: list[('Move, ('Move, 'Score))]
                            |> List.map (fun (move, miniMaxResult) -> match miniMaxResult with | (nextMove, score) -> (move,score))     //output: list[('Move, 'Score)]
                            |> List.maxBy (fun (move, score) -> score)                                                                  //output: ('Move, 'Score)
                            
                        match max with
                        | (move, score) -> (Some(move), score)

                    else // Minimizing player
                        let min =
                            possibleMoves
                            |> List.map (fun move -> (move, applyMove game move))                                                       //output: list[('Move, 'GameState)]
                            |> List.map (fun (move, gameState) -> (move, MiniMax alpha beta gameState perspective))                     //output: list[('Move, ('Move, 'Score))]
                            |> List.map (fun (move, miniMaxResult) -> match miniMaxResult with | (nextMove, score) -> (move,score))     //output: list[('Move, 'Score)]
                            |> List.minBy (fun (move, score) -> score)                                                                  //output: ('Move, 'Score)

                        match min with
                        | (move, score) -> (Some(move), score)

            NodeCounter.Reset()
            MiniMax