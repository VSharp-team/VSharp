Agent for neural network training.

## Steps to start server
1. [Build V#](https://github.com/gsvgit/VSharp/tree/mlSearcher#how-to-build)
2. Go to `VSharp.ML.GameServer.Runner/bin/Release/net6.0/`
3. ```dotnet VSharp.ML.GameServer.Runner.dll``` to start server on default port (8080) or ```cmd dotnet VSharp.ML.GameServer.Runner.dll --port [specific_port]``` to start server on `specific_port`.

## Steps to run client

python version >= 3.10 required

Usage of virtual env is recommended:


```sh
cd VSharp.ML.AIAgent  # if not in this folder already
python3 -m venv .env
[Windows]:   .\.env\Scripts\activate
[Linux/Mac]: .env/bin/activate
```

To install pre-commit checks do:

```sh
pip install pre-commit
pre-commit install
```

To load required packages for python do:

```sh
pip install -r requirements.txt
```

```sh
python agent.py
``` 

