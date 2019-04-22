using System;
using System.Collections.Generic;

namespace QUT.CSharpTicTacToe
{
    public class WithAlphaBetaPruning : ITicTacToeModel<Game, Move, Player>
    {
        public Player Cross => Player.Cross; 
        public Player Nought => Player.Nought;

        public override string ToString()
        {
            return "Impure C# with Alpha Beta Pruning";
        }

        public Game ApplyMove(Game game, Move move)
        {
            game.applyMove(move);
            return game;
        }

        public Move CreateMove(int row, int col)
        {
            return new Move(row, col);
        }

        public Move FindBestMove(Game game)
        {
            NodeCounter.Reset(); 
            var result = MiniMax(game, game.Turn, -1, 1);
            return result.Item1; //Item1 is the Move in tuple returned from MiniMax: (Move,Score)  
        }

        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {
            return game.getGameOutcome();
        }

        public Game GameStart(Player first, int size)
        {
            return new Game(size, first);
        }

        private Player GetTurn(Game game)
        {
            return game.Turn;
        }


        //*****************************************************************************************//
        //*************************  Helper functions for MiniMax only ****************************//
        //*****************************************************************************************//

        // returns a list containing all available moves from a given game state 
        private List<Move> MoveGenerator(Game game) 
        {
            List<Move> moves = new List<Move>();
            for(int row = 0; row < game.Size; row++)
            {
                for(int col = 0; col < game.Size; col++)
                {
                    if(game.getPiece(row,col) == "") 
                    {
                        moves.Add(CreateMove(row, col));
                    }
                }
            }
            return moves;
        }

        // return a heuristic score for a given game state 
        // +1 for a win, -1 for a loss and 0 for a draw
        private int Heuristic(Game game, Player perspective)
        {
            switch (game.getGameOutcome())
            {
                case TicTacToeOutcome<Player>.Win w:
                    if (w.winner == perspective)
                    {
                        return 1;
                    }
                    return -1;
                default:
                    return 0;
            }
        }

        // determine if the game is over from a given game state
        private bool GameOver(Game game)
        {
            var outcome = game.getGameOutcome();
            if (outcome.IsWin)
            {
                return true;
            }
            if (outcome.IsDraw)
            {
                return true;
            }
            return false;
        }

        //*****************************************************************************************//
        //*********************  END helper functions for MiniMax only ****************************//
        //*****************************************************************************************//


        // recursive MiniMax function with Alpha Beta pruning https://en.wikipedia.org/wiki/Minimax
        // returns the best move and the score associated with that move 
        private ValueTuple<Move,int> MiniMax(Game game, Player perspective, int alpha, int beta)
        {
            NodeCounter.Increment();
            var maximisingPlayer = GetTurn(game) == perspective;
            var moves = MoveGenerator(game); // enumerate all possible moves from the given game state 

            if (GameOver(game)){
                return (null, Heuristic(game, perspective));
            }
            if (maximisingPlayer){
                var score = int.MinValue;
                var bestMove = new Move(0, 0); 

                // apply each move and evaluate a score for that move (child node)
                foreach (Move move in moves) 
                {
                    var nodeScore = MiniMax(ApplyMove(game, move), perspective, alpha, beta).Item2;
                    if (nodeScore > score) { bestMove.row = move.Row; bestMove.col = move.Col; } // if the best move so far was found - update it
                    game.undoMove(move); // undo the move to preserve the game state 

                    score = Math.Max(score, nodeScore); 
                    alpha = Math.Max(alpha, score);
                    if (beta <= alpha) { break; } // if there are no better moves available stop searching
                }

                return (bestMove, score);
            }else{ //minimisingPlayer
                var score = int.MaxValue;
                var bestMove = new Move(0, 0);

                // apply each move and evaluate a score for that move (child node)
                foreach (Move move in moves) 
                {
                    var nodeScore = MiniMax(ApplyMove(game, move), perspective, alpha, beta).Item2;
                    if (nodeScore < score) { bestMove.row = move.Row; bestMove.col = move.Col; } // if the best move so far was found - update it
                    game.undoMove(move); // undo the move to preserve the game state

                    score = Math.Min(score, nodeScore); 
                    beta = Math.Min(beta, score);
                    if (beta <= alpha) { break; } // if there are no better moves available stop searching
                }
                return (bestMove, score);
            }
        }

    }
}