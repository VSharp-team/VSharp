using System;
using System.Collections.Generic;
using System.Linq;
using ChessDotNet;
using ChessDotNet.Pieces;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public class ChessDotNet
    {
        private static void Example()
        {
            var game = new ChessGame();
            Piece pieceAtA1 = game.GetPieceAt(new Position("A1")); // Or "a1", the casing doesn't matter
            Console.WriteLine("What piece is there at A1? {0}", pieceAtA1.GetFenCharacter());
            // GetFenCharacter() returns the FEN character for the given piece. White piece: uppercase, black piece: lowercase. The character is the first char of a piece's name (exception: Knight -> N/n because King -> K/k).
            // The Piece class is the abstract base class for pieces. All piece classes (e.g. Rook) derive from this class.

            // White has to make a turn. They want to move their E2 pawn to E4. Is that valid?
            var e2e4 = new Move("E2", "E4", Player.White);
            bool isValid = game.IsValidMove(e2e4);
            Console.WriteLine("E2-E4 for white is valid: {0}", isValid);

            // Great, it's valid! So white wants to actually make that move.
            MoveType type = game.ApplyMove(e2e4, true);
            // The first argument is the move, the second argument indicates whether it's already validated. Here it is, so pass 'true'. If it's not validated yet, ApplyMove will do it. **Only pass `true` if it's really validated! If you pass `true`, ApplyMove won't do ANY validity checks.**
            // The return type is the MoveType enum. It holds one, or a combination, of these values: Invalid, Move, Capture, Castling, Promotion
            // Each valid move will always carry the 'Move' value. If it's also something else, it will carry both values (e.g. if the move is a capture, `type` will have the value MoveType.Move | MoveType.Capture).
            // MoveType is a flags enumeration. https://msdn.microsoft.com/en-us/library/ms229062%28v=vs.100%29.aspx
            // e4 is just a normal move, so `type` will just be MoveType.Move.
            Console.WriteLine("Move type: {0}", type);

            // ChessGame provides methods to check whether a player is in check, checkmated... Here is an example:
            Console.WriteLine("Black in check? {0}", game.IsInCheck(Player.Black));
            // Here IsInCheck returns 'false' because black is not in check.

            // Now it's black's turn.
            Console.WriteLine("It's this color's turn: {0}", game.WhoseTurn);

            // You can figure out all valid moves using GetValidMoves.
            IEnumerable<Move> validMoves = game.GetValidMoves(Player.Black);
            // Here it returns all valid moves for black, but you can also find all valid moves *from a certain position* by passing a Position instance as argument.
            Console.WriteLine("How many valid moves does black have? {0}", validMoves.Count());

            // It might happen that you don't really care about all valid moves, but just want to know if there are valid moves. Chess.NET also has a method for that:
            bool hasValidMoves = game.HasAnyValidMoves(Player.Black);
            // Again, you can also pass a Position instance here.
            Console.WriteLine("Black has any valid moves: {0}", hasValidMoves);

            // Congratulations! You have learned about the most important methods of Chess.NET. Enjoy using the library :)
            Console.ReadKey();
        }

        [Ignore("takes too much time")]
        public static bool ComplexTest()
        {
            var game = new ChessGame();
            Piece pieceAtA1 = game.GetPieceAt(new Position("A1"));
            var e2e4 = new Move("E2", "E4", Player.White);
            bool isValid = game.IsValidMove(e2e4);
            MoveType type = game.ApplyMove(e2e4, true);
            return game.HasAnyValidMoves(Player.Black) || type == MoveType.Invalid || isValid;
        }

        [TestSvm]
        public static ChessGame CreateGame()
        {
            return new ChessGame();
        }

        [TestSvm]
        public static Position CreatePosition()
        {
            return new Position("A1");
        }

        [TestSvm]
        public static Piece GetPiece()
        {
            var game = new ChessGame();
            Piece pieceAtA1 = game.GetPieceAt(new Position("A1"));
            return pieceAtA1;
        }

        [TestSvm]
        public static bool HasAnyValidMoves()
        {
            var game = new ChessGame();
            return game.HasAnyValidMoves(Player.Black);
        }

        [TestSvm(100)]
        public static bool ApplyMoveAndCheckOtherValidMoves()
        {
            var game = new ChessGame();
            var e2e4 = new Move("E2", "E4", Player.White);
            MoveType type = game.ApplyMove(e2e4, true);
            return game.HasAnyValidMoves(Player.Black) | type == MoveType.Invalid;
        }

        [TestSvm]
        public static bool CheckEquality()
        {
            var p1 = new Pawn(Player.Black);
            var p2 = new Pawn(Player.White);
            return p1 == p2;
        }

        [TestSvm(100)]
        public static bool ApplyMoveAndCheckValid()
        {
            var game = new ChessGame();
            var e2e4 = new Move("E2", "E4", Player.White);
            Piece pawn = game.GetPieceAt(new Position("E2"));
            MoveType type = game.ApplyMove(e2e4, true);
            bool isValid = pawn.IsValidMove(e2e4, game);
            return isValid & type == MoveType.Invalid;
        }

        [Ignore("Not ready")]
        public static bool GetOwnerSymbolic(int dst)
        {
            var data = CreateDataForCheckMate();
            var game = new ChessGame(data);
            string s = "B" + dst;
            Move move = new Move("C7", s, Player.White);
            MoveType type = game.ApplyMove(move, true);
            Piece p = game.GetPieceAt(new Position("B7"));
            return p.Owner == Player.Black;
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

        [TestSvm(100)]
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

        [Ignore("Not ready")]
        public static bool CheckMate2(int dst)
        {
            var data = CreateDataForCheckMate();
            var game = new ChessGame(data);
            string s = "B" + dst;
            Move move = new Move("C7", s, Player.White);
            MoveType type = game.ApplyMove(move, true);
            return game.IsCheckmated(Player.Black);
        }

        [TestSvm(100)]
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
