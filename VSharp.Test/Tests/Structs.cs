using System;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public struct Point
    {
        private int _x;
        private int _y;

        public Point(int x, int y)
        {
            _x = x;
            _y = y;
        }

        [TestSvm(100)]
        public bool IsInRect(int left, int top, int right, int bottom)
        {
            if (_x > left && _x < right && _y > bottom && _y < top)
            {
                return true;
            }

            return false;
        }

        [TestSvm(100)]
        public void ThrowIfNotOnXAxis()
        {
            if (_y != 0)
            {
                throw new ArgumentException();
            }
        }
    }
}
