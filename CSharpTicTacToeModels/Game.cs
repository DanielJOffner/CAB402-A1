using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        public int size; 
        public Player player;
        private ValueTuple[] board;

        public Game(int size, Player player)
        {
            this.size = size;
            this.player = player;
        }

        //private ValueTuple[] getNewBoard(int size){
        //    //(int row, int col, int)[] result = new 
        //    (int row, int col, int)[] result = new (int row, int col, int)[size * size];
        //    for (int row = 0; row < size; row++) { 
        //        for(int col = 0; col < size; col++)
        //        {
                    
        //        }
        //    }

        //}

        //** Start ITicTacToeGame interface method requirements
        public int Size => this.size;
        public Player Turn => this.player;
        public string getPiece(int row, int col)
        {
            return "";
        }
        //** End interface method requirements 

    }
}