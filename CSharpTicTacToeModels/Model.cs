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
            //var gamestateAfterMove = new Game(game.size, game.player, game.board);
            //gamestateAfterMove.applyMove(move);
            //return gamestateAfterMove;
            game.applyMove(move);
            return game;
        }

        public Move CreateMove(int row, int col)
        {
            return new Move(row, col);
        }

        public Move FindBestMove(Game game)
        {
            Console.WriteLine("Minimax called");
            NodeCounter.Reset();
            var result = MiniMax(game, game.Turn, -1, 1);
            return result.Item1; //Item1 is Move
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

        private bool GameOver(Game game)
        {
            var outcome = game.getGameOutcome();
            if (outcome.IsWin){
                return true;
            }
            if (outcome.IsDraw){
                return true;
            }
            return false;
        }

        // Returns a list containing all available moves from a given game state 
        private List<Move> MoveGenerator(Game game) 
        {
            List<Move> moves = new List<Move>();
            for(int row = 0; row < game.size; row++)
            {
                for(int col = 0; col < game.size; col++)
                {
                    if(game.board[row][col] == Player.None) 
                    {
                        moves.Add(CreateMove(row, col));
                    }
                }
            }
            return moves;
        }

        // Return a heuristic value for a game state 
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
        
        // Return the greater of two int values
        private int Max(int int1, int int2)
        {
            if (int1 > int2)
            {
                return int1;
            }
            else if (int2 > int1)
            {
                return int2;
            }
            //return either value if both are the same
            return int1;
        }

        // Return the lesser of two int values
        private int Min(int int1, int int2)
        {
            if (int1 < int2)
            {
                return int1;
            }
            else if (int2 < int1)
            {
                return int2;
            }
            //return either value if both are the same
            return int1;
        }

        //function alphabeta(node, depth, α, β, maximizingPlayer) is
        //if depth = 0 or node is a terminal node then
        //    return the heuristic value of node
        //if maximizingPlayer then
        //    value := −∞
        //    for each child of node do
        //        value := max(value, alphabeta(child, depth − 1, α, β, FALSE))
        //        α := max(α, value)
        //        if α ≥ β then
        //            break (* β cut-off*)
        //    return value
        //else
        //value := +∞
        //for each child of node do
        //    value := min(value, alphabeta(child, depth − 1, α, β, TRUE))
        //    β := min(β, value)
        //    if α ≥ β then
        //        break (* α cut-off*)
        //return value

        private ValueTuple<Move,int> MiniMax(Game game, Player perspective, int alpha, int beta)
        {
            NodeCounter.Increment();
            var maximisingPlayer = GetTurn(game) == perspective;
            var moves = MoveGenerator(game);

            if (GameOver(game)){
                //Console.WriteLine(Heuristic(game, perspective));
                return (null, Heuristic(game, perspective));
            }
            if (maximisingPlayer){
                var score = int.MinValue;
                var bestMove = new Move(0,0);
                foreach(Move move in moves)
                {
                    Game newGameState = new Game(game.size, game.player, game.board);
                    ApplyMove(newGameState, move);
                    var nodeScore = MiniMax(newGameState, perspective, alpha, beta).Item2;
                    score = Max(score, nodeScore); //Item2 is the score of the node
                    alpha = Max(alpha, score);

                    //if (score == nodeScore.Item2) { bestMove = new Move(nodeScore.Item1.row, nodeScore.Item1.col); } 
                    if (beta <= alpha) { break; }
                }
                return (bestMove, score);
            }else{ //minimisingPlayer
                var score = int.MaxValue;
                Move bestMove = new Move(0, 0);
                foreach (Move move in moves)
                {
                    Game newGameState = new Game(game.size, game.player, game.board);
                    ApplyMove(newGameState, move);
                    var nodeScore = MiniMax(newGameState, perspective, alpha, beta).Item2;
                    score = Min(score, nodeScore); //Item2 is the score of the node
                    beta = Min(beta, score);

                    //if (score == nodeScore.Item2) { bestMove = new Move(nodeScore.Item1.row, nodeScore.Item1.col); }
                    if (beta <= alpha) { break; }
                }
                return (bestMove, score);
            }
        }

    }
}