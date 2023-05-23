conda create -n "agent_env" python=3.10.9
conda activate agent_env
conda install numpy pandas tabulate

pip3 install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/rocm5.4.2
pip install torch_geometric

pip install multiprocessing-logging
conda install -c conda-forge dataclasses-json websocket-client pre_commit
pre-commit install
