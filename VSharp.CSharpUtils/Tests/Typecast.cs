using System;

namespace VSharp.CSharpUtils.Tests
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

        public double Norm()
        {
            return Math.Sqrt(X * X + Y * Y);
        }

        public double Apply(Func<int, int, double> fun)
        {
            return fun(X, Y);
        }
    }

    public static class Typecast
    {
        public static int DownCastObject(object obj)
        {
            bool a = obj is Piece;
            return a ? 5 : 6;
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

        public static int Unboxing(Object obj)
        {
            return obj is int ? 13 : 23;
        }
    }

    public class Piece : IComparable
    {
        private int _xCoord;
        private int _yCoord;
        protected int Rate = 0;

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

        public int CompareTo(object obj)
        {
            var a = (Piece)obj;
            return a.Rate;
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
            this.Name = name;
            this.JobGrade = jobGrade;
        }

        public override string ToString()
        {
            return $"{Name} ({JobGrade})";
        }

        static void DoSomething()
        {
            Employee employee = new Employee("Cool Guy", 65);
            IPromotion p = employee;

            Console.WriteLine(employee);
            p.Promote();
            Console.WriteLine(employee);
        }
    }

    static class Helper
    {
        static double CastStructToInterface(Coord arg)
        {
            INormalize tmp = arg;
            return tmp.Norm();
        }

        static int UnboxingInt(Object obj)
        {
            return (int)obj;
        }

        static int BoxingInt(int obj)
        {
            return UnboxingInt(obj);
        }
    }
}
