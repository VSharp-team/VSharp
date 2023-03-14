Framework for neural network training

## Steps to start server
 1. [Build V#](https://github.com/gsvgit/VSharp/tree/mlSearcher#how-to-build)
 2. Go to `VSharp.ML.GameServer.Runner/bin/Release/net6.0/`
 3. ```dotnet VSharp.ML.GameServer.Runner.dll``` to start server on default port (8080) or ```cmd dotnet VSharp.ML.GameServer.Runner.dll --port [specific_port]``` to start server on `specific_port`.

## Steps to run client

python version >= 3.10 required

### 1. Game server connection + Torch installation using conda/miniconda

Works on ARM proc, macOS v13.1
Python 3.10.8

```sh
cd VSharp.ML.AIAgent

# load env from condig file:
conda env create -f environment.yml
# optional: specify your path by adding -p <your env path>
# by default env with name 'agent_env' will be created

# activate env with your script:
conda activate <enter your system path>/VSharp/VSharp.ML.AIAgent/.env
cd ..
```

### 1.1 Dumping env config

use
```sh
conda env export | grep -v "^prefix: " > conda_env.yml
```
to dump current env info into config file

### 2. Launch training

(you are in ```/VSharp/VSharp.ML.AIAgent/```, conda env is activated)
```sh
python3 main.py
```
## Evaluation

| Map                      | V# default settings (steps,coverage) | V# + AI (steps,coverage)|
|--------------------------|--------------------------------------|-------------------------|
| Loan Exam (1)            | 14284, 90                            |                         |
| BinarySearch             | 299, 100                             |                         |
| KMPSearch.Search         | 1985, 100                            |                         |
| AhoCorasickMain          | 183373, 100                          |                         | 
| BellmanFord              | 8700, 100                            |                         |
| ApplyMoveAndCheckValid   | 70419, 100                           |                         |
| CheckMate1               | 64736 , 100                          |                         |
| CheckMoveIsValidAndApply | 62225, 100                           |                         | 
| BridgesGraph.bridge      | 9555, 100                            |                         |

(1) -- Manual tuning of settings: [TestSvm(90, 0, 20, false, strat: SearchStrategy.Interleaved, coverageZone: CoverageZone.Class, guidedMode: false)]
