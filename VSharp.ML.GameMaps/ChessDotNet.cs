using ChessDotNet;
using ChessDotNet.Pieces;

namespace VSharp.ML.GameMaps
{
    public class ChessDotNet
    {
        public static bool ApplyMoveAndCheckOtherValidMoves()
        {
            var game = new ChessGame();
            var e2e4 = new Move("E2", "E4", Player.White);
            MoveType type = game.ApplyMove(e2e4, true);
            return game.HasAnyValidMoves(Player.Black) || type == MoveType.Invalid;
        }
        
        public static GameCreationData CreateDataForCheckMate()
        {
            var whiteKing = (Piece) new King(Player.White);
            var blackKing = (Piece) new King(Player.Black);
            var whiteQueen = (Piece) new Queen(Player.White);
            var piece = (Piece) null;
            var Board = new Piece[8][]
            {
                new Piece[8] { blackKing, piece, piece, piece, piece, piece, piece, piece },
                new Piece[8] { piece, piece, whiteQueen, piece, piece, piece, piece, piece },
                new Piece[8] { piece, piece, whiteKing, piece, piece, piece, piece, piece },
                new Piece[8] { piece, piece, piece, piece, piece, piece, piece, piece },
                new Piece[8] { piece, piece, piece, piece, piece, piece, piece, piece },
                new Piece[8] { piece, piece, piece, piece, piece, piece, piece, piece },
                new Piece[8] { piece, piece, piece, piece, piece, piece, piece, piece },
                new Piece[8] { piece, piece, piece, piece, piece, piece, piece, piece },
            };
            var data = new GameCreationData();
            data.Board = Board;
            data.WhoseTurn = Player.White;
            return data;
        }
        
        public static bool CheckMate1(bool f)
        {
            var data = CreateDataForCheckMate();
            var game = new ChessGame(data);
            Move c7b7 = new Move("C7", "B7", Player.White);
            Move c7d7 = new Move("C7", "D7", Player.White);
            Move move;
            if (f)
                move = c7b7;
            else
                move = c7d7;
            game.ApplyMove(move, true);
            return game.IsCheckmated(Player.Black);
        }
        
        public static bool CheckMoveIsValidAndApply()
        {
            var game = new ChessGame();
            var e2e4 = new Move("E2", "E4", Player.White);
            bool isValid = game.IsValidMove(e2e4);
            MoveType type = game.ApplyMove(e2e4, true);
            return type == MoveType.Invalid && isValid;
        }
    }
}