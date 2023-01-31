Framework for neural network training.

python version >= 3.10 required

# Agent connection setup (no torch)

Usage of virtual env is recommended:

```sh
cd VSharp.ML.AIAgent  # if not in this folder already
python3 -m venv .env
[Windows]:   .\.env\Scripts\activate
[Linux/Mac]: source .env/bin/activate
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

# Agent connection + Torch installation using conda/miniconda

Works on ARM proc, macOS v13.1

```sh
cd VSharp.ML.AIAgent
conda create -p .env

# activate env with your script:
conda activate <enter your system path>/VSharp/VSharp.ML.AIAgent/.env

# then install deps
source conda_env_install.sh
```