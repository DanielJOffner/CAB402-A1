namespace QUT

    module GameTheory =

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =
            // Basic MiniMax algorithm without using alpha beta pruning
            let rec MiniMax game perspective =
                NodeCounter.Increment()
                //if we are at a leaf (terminal) node then return the heuristic score
                // +1 for a win, -1 for a loss, 0 for a draw
                if gameOver game then (None, heuristic game perspective)
                else 
                    let maximisingPlayer = (perspective = getTurn game)
                    let possibleMoves = Seq.toList <| moveGenerator game //enumerate all possible moves (child nodes) 
                    if maximisingPlayer then 
                        //Tuple containing the index of and value of the move with the highest score 
                        let max = 
                            possibleMoves
                            |> List.map (fun move -> applyMove game move) 
                            |> List.map (fun game -> MiniMax game perspective)
                            |> fun list ->
                                let maxScore =
                                    list
                                    |> List.map (fun (move, score) -> score)
                                    |> List.max
                                let indexOfMaxScore = 
                                    list
                                    |> List.findIndex (fun (move, score) -> score = maxScore)
                                (indexOfMaxScore, maxScore)

                        match max with
                        | (i, score) -> (Some(possibleMoves.[i]), score)

                    else //minimizing player
                        let min = 
                            possibleMoves
                            |> List.map (fun move -> applyMove game move) 
                            |> List.map (fun game -> MiniMax game perspective)
                            |> fun list ->
                                let minScore =
                                    list
                                    |> List.map (fun (move, score) -> score)
                                    |> List.min
                                let indexOfMinScore = 
                                    list
                                    |> List.findIndex (fun (move, score) -> score = minScore)
                                (indexOfMinScore, minScore)

                        match min with
                        | (i, score) -> (Some(possibleMoves.[i]), score)

            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()
                raise (System.NotImplementedException("Alpha Beta Pruning"))
            NodeCounter.Reset()
            MiniMax
