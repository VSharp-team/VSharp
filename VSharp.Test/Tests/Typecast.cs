using NUnit.Framework;
using System;
using VSharp.Test;
#pragma warning disable CS0659

namespace IntegrationTests.Typecast
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

        public double Norm()
        {
            return Math.Sqrt(X + Y);
        }

        public double Norm2()
        {
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
        [TestSvm(100)]
        public static int DownCastObject(object obj)
        {
            bool a = obj is Piece;
            return a ? 5 : 6;
        }

        [TestSvm(100)]
        public static bool CheckIs(ValueType x)
        {
            return x is Double;
        }

        public enum MyEnum
        {
            A,
            B
        }

        public enum MyEnum1
        {
            C,
            D
        }

        private static bool Check(object o)
        {
            return o is MyEnum1;
        }

        [TestSvm(75)]
        public static int CheckIs1(MyEnum d)
        {
            if (Check(d))
                return 1;
            return 2;
        }

        [TestSvm(100)]
        public static int DownCastObject2(object obj1, object obj2)
        {
            bool a = obj1 is Piece & obj2 is Pawn;
            bool b = obj1 is Piece & obj1 is Pawn;
            return a | b ? 5 : 6;
        }

        [TestSvm(92)]
        public static int DownCastObject3(object obj)
        {
            if (obj is int d)
            {
                if (obj is long)
                {
                    return -42;
                }
                return d + 10;
            }

            if (obj is string s)
            {
                return s.Length;
            }

            return 0;
        }

        [TestSvm(100)]
        public static object DownCastObject4(object obj)
        {
            if (obj is int d)
            {
                if (d > 10)
                    return obj;
            }

            if (obj is decimal dec)
            {
                if (dec > 10)
                    return dec + 10;
            }

            if (obj is string s)
            {
                return s.Length;
            }

            return 0;
        }

        [TestSvm(86)]
        public static object DownCastObject5(object obj1, object obj2)
        {
            if (ReferenceEquals(obj1, obj2))
            {
                switch (obj1, obj2)
                {
                    case (int i, int j) when i == 10:
                        return j;
                    case (12, int j):
                        return j;
                    case (long i, long j) when (i + 1).GetHashCode() == 3000:
                        return j.GetHashCode();
                    case (Coord c1, Coord c2) when c1.Norm() != c2.Norm():
                        return -1;
                    case (Coord c1, Coord c2):
                        return c1.X + c2.X;
                    case (Coord, int):
                        return -42;
                    case (int, long):
                        // Unreachable case
                        return -3;
                }
            }

            return 0;
        }

        [Ignore("Solver do not terminate")]
        public static int DownCastObjectWithFP(object o1, object o2)
        {
            switch (o1, o2)
            {
                case (Coord s1, Coord s2) when s1.Norm2() != s2.Norm2() && ReferenceEquals(o1, o2):
                {
                    throw new ArgumentException();
                }
                default:
                    return 1;
            }
        }

        [TestSvm(100)]
        public static object DownCastObject6(object obj)
        {
            if (obj is decimal d)
            {
                return d + 10;
            }

            return 0;
        }

        [Ignore("SMT-solver takes too much time")]
        public static object DownCastObject7(object obj)
        {
            if (obj is decimal d)
            {
                if (d > 10)
                    return d * 10 + 10;
                return d / 10 % 10;
            }

            return 0;
        }

        [Ignore("takes too much time")]
        public static object DownCastObject8(object obj)
        {
            if (obj is decimal d)
            {
                if (d.GetHashCode() == 3000)
                    return d * 10 + 10;
                return d / 10 % 10;
            }

            return 0;
        }

        [TestSvm]
        public static int UpCast()
        {
            Pawn a = new Pawn(1, 1, 25);
            Object obj = a;
            Piece b = a;
            return DownCastObject(obj) + DownCastPiece(b);
        }

        public static String DownCastToString(object str)
        {
            return (String) str;
        }

        [TestSvm]
        public static String UpCastDownCastString()
        {
            object str = "literal";
            return DownCastToString(str);
        }

        [TestSvm(100)]
        [IgnoreFuzzer("(Known bug) coverage tool assertion failed")]
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

        [Ignore("Incorrect result")]
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


        [TestSvm(100)]
        public static int Unboxing(Object obj)
        {
            return obj is int ? 13 : 23;
        }

        [TestSvm(100)]
        public static int TryCast(Object obj)
        {
            Piece a = obj as Piece;
            if (a != null)
            {
                return 33;
            }
            return 42;
        }

        [TestSvm(100)]
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

        [TestSvm]
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

        [Ignore("GetType() is not implemented")]
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
        [TestSvm]
        public static double CastStructToInterface(Coord arg)
        {
            INormalize tmp = arg;
            return tmp.Norm();
        }

        [TestSvm(100)]
        public static int WriteInStructUsingNorm(Coord2 arg)
        {
            var y = (int) arg.Norm();
            return arg.X + y; // arg.X should change
        }

        [TestSvm]
        public static int CastStructToInterfaceAndWriteInBoxed(Coord2 arg)
        {
            INormalize tmp = arg;
            var y = (int) tmp.Norm();
            return arg.X + y; // arg.X should not change
        }

        [TestSvm]
        public static int UnboxingInt(Object obj)
        {
            return (int)obj;
        }

        [TestSvm]
        public static int BoxingInt(int obj)
        {
            return UnboxingInt(obj);
        }
    }
}
