using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public struct Wrapper<T>
    {
        private T _value;
        private int _anotherValue;

        public Wrapper(T value)
        {
            _value = value;
            _anotherValue = 0;
        }

        [TestSvm(100)]
        public bool AddToAnotherValue(int n)
        {
            _anotherValue += n;

            if (_anotherValue % 2 == 0)
            {
                return true;
            }

            return false;
        }
    }

    [TestSvmFixture]
    public struct Wrapper2<T>
    {
        private int _anotherValue;

        public Wrapper2(int anotherValue)
        {
            _anotherValue = anotherValue;
        }

        [TestSvm(100)]
        public bool AddToAnotherValue2(int n)
        {
            _anotherValue += n;

            if (_anotherValue % 2 == 0)
            {
                return true;
            }

            return false;
        }
    }

    [TestSvmFixture]
    public struct Wrapper3<T> where T : class
    {
        private T _value;
        private int _anotherValue;

        public Wrapper3(T value)
        {
            _value = value;
            _anotherValue = 0;
        }

        [TestSvm(100)]
        public bool AddToAnotherValue3(int n)
        {
            _anotherValue += n;

            if (_anotherValue % 2 == 0)
            {
                return true;
            }

            return false;
        }
    }

    [TestSvmFixture]
    public struct Wrapper4<T, U, V> where T : class where U : struct where V : new()
    {
        private T _value0;
        private U _value1;
        private V _value2;

        private int _anotherValue;

        public Wrapper4(T value0, U value1, V value2)
        {
            _value0 = value0;
            _value1 = value1;
            _value2 = value2;
            _anotherValue = 0;
        }

        [TestSvm(100)]
        public bool AddToAnotherValue4(int n)
        {
            _anotherValue += n;

            if (_anotherValue % 2 == 0)
            {
                return true;
            }

            return false;
        }
    }
}
