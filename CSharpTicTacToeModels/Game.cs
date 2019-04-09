using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        public int size; 
        public Player player;
        private List<List<string>> board;

        public Game(int size, Player player)
        {
            this.size = size;
            this.player = player;
            board = new List<List<string>>();

            //populate empty board [["",""...size],["","",""]...size]
            for (int row = 0; row < size; row++)
            {
                List<string> rowLine = new List<string>();
                for (int col = 0; col < size; col++)
                {
                    rowLine.Add("");
                }
                board.Add(rowLine);
            }
        }

        private string getPieceFromPlayer(Player player)
        {
            switch (player)
            {
                case Player.Nought:
                    return "O";
                case Player.Cross:
                    return "X";
                default:
                    return "";
            }
        }

        private Player changePlayer(Player player)
        {
            switch (player)
            {
                case Player.Nought:
                    return Player.Cross;
                case Player.Cross:
                    return Player.Nought;
                default:
                    throw new Exception("Expected either Player.Nought or Player.Cross");
                
            }
        }

        public void applyMove(Move move)
        {
            board[move.row][move.col] = getPieceFromPlayer(player);
            player = changePlayer(player);
        }

        //** Start ITicTacToeGame interface method requirements
        public int Size => size;
        public Player Turn => player;
        public string getPiece(int row, int col)
        { 
            return board[row][col];
        }
        //** End interface method requirements 

    }
}