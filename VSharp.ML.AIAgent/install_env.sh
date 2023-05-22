conda create -n "agent_env" python=3.10.9
conda activate agent_env

conda install numpy pandas tabulate
pip3 install torch torchvision torchaudio
pip install torch_geometric
pip install multiprocessing_logging
conda install -c conda-forge dataclasses-json websocket-client pre_commit
pre-commit install
