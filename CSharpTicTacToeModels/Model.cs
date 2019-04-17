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
            //heuristic getTurn gameOver moveGenerator ApplyMove
            // let MiniMaxFunction = GameTheory.MiniMaxWithAlphaBetaPruningGenerator heuristic getTurn gameOver moveGenerator ApplyMove
            //let bestMove = MiniMaxFunction - 1 1 game game.Turn
            //match bestMove with
            //| (move, score)->
            //match move with
            //| Some move->move
            //| _->raise(System.Exception("MiniMax should not be called if there are no moves left"))


            //GameTheory.MiniMaxWithAlphaBetaPruningGenerator(Heuristic, GetTurn, GameOver, MoveGenerator, ApplyMove);
            return new Move(0, 0);
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
        //GetTurn, GameOver, MoveGenerator, ApplyMove);
        private Func<Game, Player, int> Heuristic2()
        {
            return Heuristic;
        }
        private Func<Game, Player> GetTurn2()
        {
            return GetTurn;
        }
        private Func<Game, bool> GameOver2()
        {
            return GameOver;
        }
        private Func<Game, List<Move>> MoveGenerator2()
        {
            return MoveGenerator;
        }
        private Func<Game, Move, Game> ApplyMove2()
        {
            return ApplyMove;
        }

    }
}