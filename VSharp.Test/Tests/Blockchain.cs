using System.Collections.Generic;
using System.Linq;
using VSharp.Test;

namespace IntegrationTests
{
    public class Transaction
    {
        public int Amount { get; set; }
    }

    public class Block
    {
        public long Index { get; set; }
        public long TimeStamp { get; set; }
        public long Hash { get; set; }
        public long PrevHash { get; set; }
        public long Nounce { get; set; }
        public List<Transaction> TransactionList { get; set; }
    }

    public class BlockMiner
    {
        public List<Block> Blockchain { get; }
        private readonly List<Transaction> _transactionPool;
        private static int MINING_REWARD = 2;

        public BlockMiner()
        {
            Blockchain = new List<Block>();
            _transactionPool = new List<Transaction>();
        }

        public void Mine(long startTime, uint blocksNumber)
        {
            var time = 0;
            while (time < blocksNumber)
            {
                GenerateBlock(startTime + time);
                time++;
            }
        }

        private void GenerateBlock(long time)
        {
            var lastBlock = Blockchain.LastOrDefault();
            var transactionList = _transactionPool;
            transactionList.Add(new Transaction()
            {
                Amount = MINING_REWARD
            });
            var block = new Block()
            {
                TimeStamp = time,
                Nounce = 0,
                TransactionList = transactionList,
                Index = lastBlock?.Index + 1 ?? 0,
                PrevHash = lastBlock?.Hash ?? 0
            };
            MineBlock(block);
            Blockchain.Add(block);
        }

        private void MineBlock(Block block)
        {
            var merkleRootHash = block.TransactionList.Aggregate(0, (x, y) => x + y.Amount, res => res);
            long nounce = -1;
            var hash = 0;
            do
            {
                nounce++;
                var rowData = block.Index + block.PrevHash + block.TimeStamp + nounce + merkleRootHash;
                hash = rowData.GetHashCode();
            } while (hash >= 10000000);

            block.Hash = hash;
            block.Nounce = nounce;
        }
    }

    [TestSvmFixture]
    public class Blockchain
    {
        [TestSvm(100, strat: SearchStrategy.DFS)]
        public static long test(long time)
        {
            var miner = new BlockMiner();
            miner.Mine(time, 1);
            var a = miner.Blockchain.OrderBy(block => block.Hash).First().Hash;
            if (a > 1000)
                return a;
            return a - 1;
        }

        [TestSvm(100, strat: SearchStrategy.BFS)]
        public static int test2(long time)
        {
            var a = new BlockMiner();
            a.Mine(time, 1);
            // a.Blockchain.First();
            return 0;
        }

        [TestSvm(100, strat: SearchStrategy.BFS)]
        public static int test6(long time, bool f)
        {
            var a = new BlockMiner();
            if (f)
                a.Mine(time, 1);
            else
            {
                a.Mine(time * 5, 1);
            }

            if (a.Blockchain.First().Hash > 100)
                return 0;
            return 1;
        }

        [TestSvm(100, strat: SearchStrategy.BFS)]
        public static int test3(long time)
        {
            var a = new BlockMiner();
            a.Mine(time, 1);
            a.Blockchain.First();
            return 0;
        }

        [TestSvm(100, strat: SearchStrategy.BFS)]
        public static long test4(long time)
        {
            var a = new BlockMiner();
            a.Mine(time, 1);
            return a.Blockchain.First().Hash;
        }
    }
}
