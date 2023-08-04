conda install numpy pandas tabulate
conda install -c pytorch pytorch=1.13.1 torchvision=0.14.1 torchaudio=0.13.1 
python3 -m pip install --force-reinstall -v "torch-scatter==2.1.0" "torch-geometric==2.2.0" "torch-sparse==0.6.16"
python3 -m pip install func_timeout
conda install -c conda-forge dataclasses-json websocket-client pre_commit aiohttp cchardet pygad httplib2 onnx onnxruntime
pre-commit install
