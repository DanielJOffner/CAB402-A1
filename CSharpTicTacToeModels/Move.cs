namespace QUT.CSharpTicTacToe
{
    public class Move : ITicTacToeMove
    {
        public int row;
        public int col;
        public Move(int row, int col)
        {
            this.row = row;
            this.col = col;
        }

        //** Start ITicTacToeMove interface methods
        public int Row => this.row;
        public int Col => this.col;
        //** End interface methods 
    }
}
