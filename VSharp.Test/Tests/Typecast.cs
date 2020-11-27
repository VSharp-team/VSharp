using NUnit.Framework;
using System;
using VSharp.Test.Tests.Typecast;

namespace VSharp.Test.Tests.Typecast
{
    public class Celsius
    {
        public Celsius(float temp)
        {
            Degrees = temp;
        }
        public static explicit operator Fahrenheit(Celsius c)
        {
            return new Fahrenheit((9.0f / 5.0f) * c.Degrees + 32);
        }
        public float Degrees { get; }
    }

    public class Fahrenheit
    {
        public Fahrenheit(float temp)
        {
            Degrees = temp;
        }

        public static explicit operator Celsius(Fahrenheit fahr)
        {
            return new Celsius((5.0f / 9.0f) * (fahr.Degrees - 32));
        }
        public float Degrees { get; }
    }

    public interface INormalize
    {
        double Norm();
    }

    public struct Coord : INormalize
    {
        public int X;
        public int Y;

        public Coord(int x, int y)
        {
            X = x;
            Y = y;
        }

        [TestSvm]
        public double Norm()
        {
            //TODO: Incorrect printed string of elements in the array
            return Math.Sqrt(X * X + Y * Y);
        }
    }

    public struct Coord2 : INormalize
    {
        public int X;
        public int Y;

        public Coord2(int x, int y)
        {
            X = x;
            Y = y;
        }

        public double Norm()
        {
            X = X + Y * X;
            return X;
        }
    }

    [TestSvmFixture]
    public static class Typecast
    {
        [TestSvm]
        public static int DownCastObject(object obj)
        {
            bool a = obj is Piece;
            return a ? 5 : 6;
        }

        [TestSvm]
        public static int DownCastObject2(object obj1, object obj2)
        {
            bool a = obj1 is Piece & obj2 is Pawn;
            bool b = obj1 is Piece & obj1 is Pawn;
            return a | b ? 5 : 6;
        }

        [TestSvm]
        public static int UpCast()
        {
            Pawn a = new Pawn(1, 1, 25);
            Object obj = a;
            Piece b = a;
            return DownCastObject(obj) + DownCastPiece(b);
        }

        [TestSvm]
        public static int DownCastPiece(Piece piece)
        {
            bool a = piece is Pawn;
            return a ? 10 : 20;
        }

        [TestSvm]
        public static int CheckCastNullWithTrick()
        {
            return DownCastPiece(null);
        }

        // always 38, because the null reference is not assigned any type at all
        [TestSvm]
        public static int CastAfterNull()
        {
            Piece a = new Piece(1, 3);
            a = null;
            Piece b = (Piece) a;
            return b is Object ? 33 : 38;
        }

        [TestSvm]
        public static Pawn TypeCast(Object obj)
        {
            Pawn pawn = (Pawn)obj;
            return pawn;
        }

        [TestSvm]
        public static Pawn TypeCastConcreteNull()
        {
            return TypeCast(null);
        }


        [TestSvm]
        public static int Unboxing(Object obj)
        {
            return obj is int ? 13 : 23;
        }

        [TestSvm]
        public static int TryCast(Object obj)
        {
            Piece a = obj as Piece;
            if (a != null)
            {
                return 33;
            }
            return 42;
        }

        [TestSvm]
        public static int TryUpCast(Piece piece)
        {
            return TryCast(piece);
        }
    }

    public interface IMovable
    {
        IMovable MakeMove(Coord c);
    }

    [TestSvmFixture]
    public class Piece : IMovable
    {
        protected int _xCoord;
        protected int _yCoord;
        protected int Rate = 0;

        public Piece()
        {
            _xCoord = 0;
            _yCoord = 0;
        }

        public Piece(int x, int y)
        {
            _xCoord = x;
            _yCoord = y;
        }

        public Piece(Coord coord)
        {
            _xCoord = coord.X;
            _yCoord = coord.Y;
        }

        [TestSvm]
        public Coord GetCoord()
        {
            Coord coord;
            coord.X = _xCoord;
            coord.Y = _yCoord;
            return coord;
        }

        [TestSvm]
        public int GetRate()
        {
            return Rate;
        }

        [Ignore("Exceptions handling")]
        public int RetRate(object obj)
        {
            var a = (Piece)obj;
            return a.Rate;
        }

        [TestSvm]
        public virtual IMovable MakeMove(Coord c)
        {
            _xCoord = c.X;
            _yCoord = c.Y;
            return this;
        }
    }

    [TestSvmFixture]
    public class Pawn : Piece
    {
        private int _newField;

        private void Constructor(int newField)
        {
            _newField = newField;
            Rate = 1;
        }
        public Pawn(int x, int y, int newField) : base(x, y)
        {
            Constructor(newField);
        }
        public Pawn(Coord coord, int newField) : base(coord)
        {
            Constructor(newField);
        }

        [TestSvm]
        public int GetNewField()
        {
            return _newField;
        }

        [TestSvm]
        public void SetNewField(int field)
        {
            _newField = field;
        }

        [TestSvm]
        public override IMovable MakeMove(Coord c)
        {
            _xCoord = c.X + c.Y;
            _yCoord = c.X - c.Y;
            return this;
        }
    }

    [TestSvmFixture]
    public class BlackPawn : Pawn
    {
        public BlackPawn(int x, int y, int newField) : base(x, y, newField)
        {
        }

        public BlackPawn(Coord coord, int newField) : base(coord, newField)
        {
        }

        [TestSvm]
        public new IMovable MakeMove(Coord c)
        {
            _xCoord = c.X + c.Y;
            _yCoord = c.X - c.Y;
            return this;
        }
    }

    [TestSvmFixture]
    public class WhitePawn : Pawn {
        public WhitePawn(int x, int y, int newField) : base(x, y, newField)
        {
        }

        public WhitePawn(Coord coord, int newField) : base(coord, newField)
        {
        }
    }

    [TestSvmFixture]
    public class Knight : Piece
    {
        [TestSvm]
        public new IMovable MakeMove(Coord c)
        {
            _xCoord = c.X + c.Y;
            _yCoord = c.X - c.Y;
            return this;
        }

        public override bool Equals(object obj)
        {
            if (obj == null || obj.GetType() != this.GetType())
                return false;
            return true;
        }

        [Ignore("Calling virtual method is not implemented")]
        public static bool ReferenceIdentity()
        {
            var knight = new Knight();
            return knight.Equals(knight);
        }


        [Ignore("GetType() is not implemented")]
        public static bool KnightIsKnight()
        {
            var knight = new Knight();
            var knight2 = new Knight();
            return knight.Equals(knight2);
        }

        [Ignore("GetType() is not implemented")]
        public static bool KnightIsNotPawn()
        {
            var knight = new Knight();
            var blackPawn = new BlackPawn(1, 1, 1);
            return knight.Equals(blackPawn);
        }

        [Ignore("GetType() is not implemented")]
        public static Type UnionGetTypeSymbolic(bool f, object o1, object o2)
        {
            if (o1 == null || o2 == null)
                return null;
            if (f)
            {
                return o1.GetType();
            }

            return o2.GetType();
        }

        [Ignore("GetType() is not implemented")]
        public static Type UnionGetType(bool f, object o1, object o2)
        {
            if (o1 == null || o2 == null)
                return null;
            if (f)
            {
                return o1.GetType();
            }

            return o2.GetType();
        }
    }

    interface IPromotion
    {
        void Promote();
    }

    struct Employee : IPromotion
    {
        public string Name;
        public int JobGrade;

        public void Promote()
        {
            JobGrade++;
        }

        public Employee(string name, int jobGrade)
        {
            Name = name;
            JobGrade = jobGrade;
        }

        public override string ToString()
        {
            return $"{Name} ({JobGrade})";
        }

        public static void DoSomething()
        {
            Employee employee = new Employee("Cool Guy", 65);
            IPromotion p = employee;

            Console.WriteLine(employee);
            p.Promote();
            Console.WriteLine(employee);
        }
    }

    [TestSvmFixture]
    public static class Helper
    {
        [Ignore("Calling virtual method is not implemented")]
        public static double CastStructToInterface(Coord arg)
        {
            INormalize tmp = arg;
            return tmp.Norm();
        }

        [Ignore("Calling virtual method is not implemented")]
        public static int CastStructToInterfaceAndWriteInBoxed(Coord2 arg)
        {
            INormalize tmp = arg;
            var y = (int) tmp.Norm();
            return arg.X + y; // arg.X should not change
        }

        [Ignore("Exceptions handling")]
        public static int UnboxingInt(Object obj)
        {
            return (int)obj;
        }

        [Ignore("primitive cast: unreachable")]
        public static int BoxingInt(int obj)
        {
            return UnboxingInt(obj);
        }
    }
}
