Framework for Genetic NN training

## Steps to start server
 1. [Build V#](https://github.com/gsvgit/VSharp/tree/mlSearcher#how-to-build)
 2. Go to `VSharp.ML.GameServer.Runner/bin/Release/net6.0/`
 3. ```dotnet VSharp.ML.GameServer.Runner.dll``` to start server on default port (8100) or ```cmd dotnet VSharp.ML.GameServer.Runner.dll --port [specific_port]``` to start server on `specific_port`.

## Steps to run client

### 1. Create venv, install deps
Python 3.10.8

In ```VSharp/VSharp.ML.AIAgent/```
- create virtualenv or conda env
- activate venv
- install dependencies


### 2. Launch training

(you are in ```/VSharp/VSharp.ML.AIAgent/```, conda env is activated)

1. configure MAX_STEPS in [common.constants](./common/constants.py)
2. set up `server_count` in [main.py](./main.py)
3. `python3 launch_servers.py -n <server_count>`
4. `python3 main.py`

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
