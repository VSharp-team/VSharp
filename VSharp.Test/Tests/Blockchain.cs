using System.Collections.Generic;
using NBlockchain;
using NBlockchain.Models;
using NBlockchain.Services;
using System;
using System.Collections.Generic;
using System.Text;
using NBlockchain.Models;
using NBlockchain.Interfaces;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    public class Blockchain
    {
        public class TestInstruction : Instruction
        {
            public string Data { get; set; }

            public override ICollection<byte[]> ExtractSignableElements()
            {
                return new List<byte[]>() { Encoding.UTF8.GetBytes(Data) };
            }

            public override int GetHashCode()
            {
                return Data.GetHashCode();
            }
        }

        [TestSvm]
        public static ICollection<Instruction> BuildInstructions(KeyPair builderKeys, ICollection<Transaction> transactions)
        {
            var instructions = new HashSet<Instruction>();
            var i1 = new TestInstruction {Data = "test", PublicKey = builderKeys.PublicKey};
            instructions.Add(i1);

            return instructions;
        }

        [TestSvm]
        public static int Test1()
        {
            // new NBlockchain.Models.;
            return 0;
        }
    }
}
