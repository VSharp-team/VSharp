import argparse
import os
import signal
import subprocess


def main():
    parser = argparse.ArgumentParser(description="V# instances launcher")
    parser.add_argument(
        "-n", "--num_inst", type=int, help="number of instances to launch"
    )
    args = parser.parse_args()

    start_port = 8100

    # assuming we start from ~/gsv/VSharp/VSharp.ML.AIAgent
    working_dir = "../VSharp.ML.GameServer.Runner/bin/Release/net6.0/"
    launch_server = [
        "dotnet",
        "VSharp.ML.GameServer.Runner.dll",
        "--checkactualcoverage",
        "--port",
    ]

    procs = []
    for i in range(args.num_inst):
        proc = subprocess.Popen(
            launch_server + [str(start_port + i)],
            start_new_session=True,
            cwd=working_dir,
        )
        procs.append(proc)
        print(f"{proc.pid}: " + " ".join(launch_server + [str(start_port + i)]))

    input("Press any key to kill server processes... ")

    for proc in procs:
        os.kill(proc.pid, signal.SIGTERM)
        print(f"killed {proc.pid}")


if __name__ == "__main__":
    main()
