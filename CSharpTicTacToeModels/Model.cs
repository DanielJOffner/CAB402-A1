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
            throw new System.NotImplementedException("FindBestMove");
        }
        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {
            return game.getGameOutcome();
        }
        public Game GameStart(Player first, int size)
        {
            return new Game(size, first);
        }
    }
}