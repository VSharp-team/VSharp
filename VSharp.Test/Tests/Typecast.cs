using System;
using VSharp.CSharpUtils.Tests.Typecast;

namespace VSharp.CSharpUtils.Tests.Typecast
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
            //TODO: Incorrect printed string of elements in the array
            return Math.Sqrt(X * X + Y * Y);
        }
    }

    public static class Typecast
    {
        public static int DownCastObject(object obj)
        {
            bool a = obj is Piece;
            return a ? 5 : 6;
        }

        public static int DownCastObject2(object obj1, object obj2)
        {
            bool a = obj1 is Piece & obj2 is Pawn;
            bool b = obj1 is Piece & obj1 is Pawn;
            return a | b ? 5 : 6;
        }

        public static int UpCast()
        {
            Pawn a = new Pawn(1, 1, 25);
            Object obj = a;
            Piece b = a;
            return DownCastObject(obj) + DownCastPiece(b);
        }

        public static int DownCastPiece(Piece piece)
        {
            bool a = piece is Pawn;
            return a ? 10 : 20;
        }

        public static int CheckCastNullWithTrick()
        {
            return DownCastPiece(null);
        }

        // always 38, because the null reference is not assigned any type at all
        public static int CastAfterNull()
        {
            Piece a = new Piece(1, 3);
            a = null;
            Piece b = (Piece) a;
            return b is Object ? 33 : 38;
        }

        public static Pawn TypeCast(Object obj)
        {
            Pawn pawn = (Pawn)obj;
            return pawn;
        }

        public static int Unboxing(Object obj)
        {
            return obj is int ? 13 : 23;
        }

        public static int TryCast(Object obj)
        {
            Piece a = obj as Piece;
            if (a != null)
            {
                return 33;
            }
            return 42;
        }

        public static int TryUpCast(Piece piece)
        {
            return TryCast(piece);
        }
    }

    public interface IMovable
    {
        IMovable MakeMove(Coord c);
    }

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

        public Coord GetCoord()
        {
            Coord coord;
            coord.X = _xCoord;
            coord.Y = _yCoord;
            return coord;
        }

        public int GetRate()
        {
            return Rate;
        }

        public int RetRate(object obj)
        {
            var a = (Piece)obj;
            return a.Rate;
        }

        public virtual IMovable MakeMove(Coord c)
        {
            _xCoord = c.X;
            _yCoord = c.Y;
            return this;
        }
    }

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

        public int GetNewField()
        {
            return _newField;
        }

        public void SetNewField(int field)
        {
            _newField = field;
        }

        public override IMovable MakeMove(Coord c)
        {
            _xCoord = c.X + c.Y;
            _yCoord = c.X - c.Y;
            return this;
        }
    }

    public class BlackPawn : Pawn
    {
        public BlackPawn(int x, int y, int newField) : base(x, y, newField)
        {
        }

        public BlackPawn(Coord coord, int newField) : base(coord, newField)
        {
        }

        public new IMovable MakeMove(Coord c)
        {
            _xCoord = c.X + c.Y;
            _yCoord = c.X - c.Y;
            return this;
        }
    }

    public class WhitePawn : Pawn {
        public WhitePawn(int x, int y, int newField) : base(x, y, newField)
        {
        }

        public WhitePawn(Coord coord, int newField) : base(coord, newField)
        {
        }
    }

    public class Knight : Piece
    {
        public new IMovable MakeMove(Coord c)
        {
            _xCoord = c.X + c.Y;
            _yCoord = c.X - c.Y;
            return this;
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

    public static class Helper
    {
        public static double CastStructToInterface(Coord arg)
        {
            INormalize tmp = arg;
            return tmp.Norm();
        }

        public static int UnboxingInt(Object obj)
        {
            return (int)obj;
        }

        public static int BoxingInt(int obj)
        {
            return UnboxingInt(obj);
        }
    }
}
