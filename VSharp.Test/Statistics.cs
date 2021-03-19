using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

namespace VSharp.Test
{
    public class Statistics
    {
        private int _methodsNumber;
        private readonly Dictionary<Type, List<Exception>> _allExceptions = new Dictionary<Type, List<Exception>>();

        private readonly Dictionary<string, List<MethodBase>> _notImplementedExceptions =
            new Dictionary<string, List<MethodBase>>();

        private readonly Dictionary<string, List<MethodBase>> _unreachableExceptions =
            new Dictionary<string, List<MethodBase>>();

        private readonly Dictionary<string, List<MethodBase>> _internalFailExceptions =
            new Dictionary<string, List<MethodBase>>();

        private readonly Dictionary<string, List<MethodBase>> _unhandledExceptions =
            new Dictionary<string, List<MethodBase>>();

        public void SetupBeforeMethod(MethodBase m)
        {
            // UNCOMMENT THIS FOR DEBUGGING AND FINDING METHODS NAME THAT FAILS WITH StackOverflowException
            // METHOD == via binary search try to find vary constant, and just brute force it when range is small

            // const int maxNumber = 2520;
            // if (_methodsNumber > maxNumber)
            // {
            //     return;
            // }
            //
            // if (_methodsNumber == maxNumber - 1)
            // {
            //     Console.WriteLine($@"Got it: {m.Name}");
            // }

            _methodsNumber++;
        }

        public void AddSucceededMethod(MethodBase m)
        {
            Console.WriteLine("DONE: {0}", m);
        }

        public void AddException(Exception e, MethodBase m)
        {
            Type t = e.GetType();
            if (t == typeof(NotImplementedException))
            {
                Console.WriteLine("NotImplementedException in {0} occured: {1}", m, e.StackTrace);
                AddException(_notImplementedExceptions, e, m);
            }
            else if (t == typeof(UnreachableException))
            {
                Console.WriteLine("Unreachable Exception in {0} occured: {1}", m, e.Message);
                AddException(_unreachableExceptions, e, m);
            }
            else if (t == typeof(InternalException))
            {
                Console.WriteLine("InternalFail Exception in {0} occured: {1}", m, e.Message);
                AddException(_internalFailExceptions, e, m);
            }
            else
            {
                Console.WriteLine($@"Unhandled Exception occured:
                                      method = {m.Name}
                                      message = {e.Message}
                                      StackTrace: {e.StackTrace}");
                AddException(_unhandledExceptions, e, m);
            }
        }

        private void Print(Type type, string exceptionName, Dictionary<string, List<MethodBase>> exceptions)
        {
            if (_allExceptions.ContainsKey(type))
            {
                Console.WriteLine(exceptionName + "Exceptions");
                Console.WriteLine($"INFO: {exceptionName} number: {_allExceptions[type].Count.ToString()}");
                Console.WriteLine($"INFO: {exceptionName} Types: {exceptions.Keys.Count.ToString()}");
                foreach (var message in exceptions.Keys.OrderByDescending(message => exceptions[message].Count))
                {
                    Console.WriteLine(message);
                    Console.WriteLine("CNT = " + exceptions[message].Count);
                    string fullName = Reflection.getFullMethodName(exceptions[message][0]);
                    Console.WriteLine($@"Method For Debugging = {fullName}");
                    Console.WriteLine("");
                }
                Console.WriteLine("END");
            }
            else
            {
                Console.WriteLine("No " + exceptionName + " exceptions found");
            }
        }

        public void PrintExceptionsStats()
        {
            Console.WriteLine($"INFO: exceptions types number: {_allExceptions.Keys.Count.ToString()}");
            Console.WriteLine($"INFO: methods number: {_methodsNumber}");
            Print(typeof(NotImplementedException), "NOT_IMPL ", _notImplementedExceptions);
            Print(typeof(UnreachableException), "UNREACHABLE ", _unreachableExceptions);
            Print(typeof(InternalException), "Internal ", _internalFailExceptions);
        }

        private void AddException(Dictionary<string, List<MethodBase>> exceptions, Exception e, MethodBase m)
        {
            Type t = e.GetType();
            if (!_allExceptions.ContainsKey(t))
            {
                _allExceptions[t] = new List<Exception>();
            }

            _allExceptions[t].Add(e);
            string stackTrace = e.StackTrace;
            if (stackTrace == null)
            {
                Console.WriteLine($@"While analyzing {m.Name} occured exception with null stackTrace");
                return;
            }

            if (!exceptions.ContainsKey(stackTrace))
            {
                exceptions[stackTrace] = new List<MethodBase>();
            }

            exceptions[stackTrace].Add(m);
        }
    }
}