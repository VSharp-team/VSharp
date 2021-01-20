using NUnit.Framework;
using System;
using System.Collections.Generic;
using ChessDotNet;
using ChessDotNet.Pieces;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    [Ignore("Marshaling is not implemented")]
    public sealed class Marshaling
    {
        public class RecursiveClass
        {
            private int _value;
            private RecursiveClass next;

            public RecursiveClass(int value)
            {
                _value = value;
                next = this;
            }

            public void SetValue(int x)
            {
                _value = x;
            }

            public int Value()
            {
                return _value;
            }

            public int Multiply(int x)
            {
                return x * _value;
            }
        }

        public class A
        {
            private int _x, _y;
            public A(int x, int y)
            {
                _x = x;
                _y = y;
            }

            public void SetX(int newX)
            {
                _x = newX;
            }

            public void SetY(int newX)
            {
                _x = newX;
            }

            public int Sum()
            {
                return _x + _y;
            }
        }

        [TestSvm]
        public static A CreateA()
        {
            var a = new A(5, 10);
            return a;
        }

        [TestSvm]
        public static int ConcreteExecution_1()
        {
            var a = new A(5, 10);
            a.SetX(32);
            return a.Sum();
        }

        [TestSvm]
        public static int ConcreteExecution_2(int x)
        {
            var a = new A(5, 10);
            a.SetX(x);
            a.SetX(0);
            return a.Sum();
        }

        [TestSvm]
        public static int SymbolicExecution_1(int x)
        {
            var a = new A(5, 10);
            a.SetX(x);
            return a.Sum();
        }


        [TestSvm]
        public static RecursiveClass CreateRecursiveObject()
        {
            var a = new RecursiveClass(42);
            return a;
        }

        [TestSvm]
        public static int WasSymbolic_BecameConcrete_RecursiveObject(int v)
        {
            var a = new RecursiveClass(v);
            a.SetValue(50);
            return a.Multiply(2);
        }

        private static RecursiveClass F()
        {
            return new RecursiveClass(45);
        }
        [TestSvm]
        public static RecursiveClass CreateRecursive_FromScratch()
        {
            var a = F();
            return a;
        }

        [TestSvm]
        public static Dictionary<char, Piece> CreateDictionary_Empty()
        {
            Dictionary<char, Piece> fenMappings = new Dictionary<char, Piece>();
            return fenMappings;
        }

        [TestSvm]
        public static King Create_King()
        {
            return new King(Player.White);
        }

        private static int[] H()
        {
            var array = new int[7];
            array[1] = 42;
            array[4] = -100;
            array[5] = 12;
            array[4] = -200;
            return array;
        }

        [TestSvm]
        public static int[] Create_SimpleArray()
        {
            return H();
        }

        private static int[,] H2()
        {
            var array = new int[10,5];
            array[0,4] = 4;
            array[4,0] = 90;
            array[9, 2] = 12;
            array[0,0] = -200;
            return array;
        }

        [TestSvm]
        public static int[,] Create_2D_Array()
        {
            return H2();
        }

        private static Piece[][] G()
        {
            var array = new Piece[4][];
            array[1] = new Piece[10];
            array[1][9] = new Bishop(Player.None);
            array[1][0] = new King(Player.White);
            array[3] = new Piece[5];
            array[3][3] = new Knight(Player.Black);
            return array;
        }

        [TestSvm]
        public static Piece[][] Create_ArrayOfArray()
        {
            return G();
        }

        private static void ChangeVar(ref int x)
        {
            x = 42;
        }

        [TestSvm]
        public static int Indirect_Change()
        {
            int x = 10;
            ChangeVar(ref x);
            return x;
        }

        [TestSvm]
        public static Dictionary<char, Piece> CreateRepeatingDictionary()
        {
            Dictionary<char, Piece> fenMappings = new Dictionary<char, Piece>()
            {
                { 'a', new King(Player.White) },
                { 'b', new King(Player.White) },
                { 'c', new King(Player.White) }
            };

            return fenMappings;
        }

        [Ignore("GetTypeFromHandle() is not implemented")]
        public static Dictionary<char, Piece> CreateDictionary()
        {
            Dictionary<char, Piece> fenMappings = new Dictionary<char, Piece>()
            {
                { 'K', new King(Player.White) },
                { 'k', new King(Player.Black) },
                { 'Q', new Queen(Player.White) },
                { 'q', new Queen(Player.Black) },
                { 'R', new Rook(Player.White) },
                { 'r', new Rook(Player.Black) },
                { 'B', new Bishop(Player.White) },
                { 'b', new Bishop(Player.Black) },
                { 'N', new Knight(Player.White) },
                { 'n', new Knight(Player.Black) },
                { 'P', new Pawn(Player.White) },
                { 'p', new Pawn(Player.Black) },
            };

            return fenMappings;
        }

        [Ignore("Works very long")]
        public static Dictionary<char, Piece> CreateSymbolicDictionary(char c)
        {
            Dictionary<char, Piece> fenMappings = new Dictionary<char, Piece>();
            fenMappings.Add(c, new King(Player.White));

            return fenMappings;
        }

    }
}
