import argparse
import json
import os
import signal
import subprocess
from queue import Queue

from aiohttp import web

from common.constants import BROKER_SERVER_PORT, VSHARP_INSTANCES_START_PORT

routes = web.RouteTableDef()


@routes.get("/get_ws")
async def dequeue_ws(request):
    try:
        ws = WS_URLS.get(timeout=0.1)
        print(f"issued {ws}")
        return web.Response(text=ws)
    except Exception as e:
        print(e)
        return web.Response(text=str(e))


@routes.post("/post_ws")
async def enqueue_ws(request):
    returning_ws_raw = await request.read()
    returning_ws = returning_ws_raw.decode("utf-8")
    print(f"put back {returning_ws}")
    WS_URLS.put(returning_ws)
    return web.HTTPOk()


@routes.post("/send_res")
async def append_results(request):
    global RESULTS
    data = await request.read()
    decoded = data.decode("utf-8")
    RESULTS.append(decoded)
    return web.HTTPOk()


@routes.get("/recv_res")
async def send_and_clear_results(request):
    global RESULTS
    if not RESULTS:
        raise RuntimeError("Must play a game first")
    rst = json.dumps(RESULTS)
    RESULTS = []
    return web.Response(text=rst)


def main():
    global WS_URLS, RESULTS
    parser = argparse.ArgumentParser(description="V# instances launcher")
    parser.add_argument(
        "-n", "--num_inst", type=int, help="number of instances to launch"
    )
    args = parser.parse_args()

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
            launch_server + [str(VSHARP_INSTANCES_START_PORT + i)],
            start_new_session=True,
            cwd=working_dir,
        )
        procs.append(proc)
        print(
            f"{proc.pid}: "
            + " ".join(launch_server + [str(VSHARP_INSTANCES_START_PORT + i)])
        )

    SOCKET_URLS = [
        f"ws://0.0.0.0:{VSHARP_INSTANCES_START_PORT + i}/gameServer"
        for i in range(args.num_inst)
    ]

    WS_URLS = Queue()
    RESULTS = []

    for ws_url in SOCKET_URLS:
        WS_URLS.put(ws_url)

    app = web.Application()
    app.add_routes(routes)
    web.run_app(app, port=BROKER_SERVER_PORT)

    for proc in procs:
        os.kill(proc.pid, signal.SIGTERM)
        print(f"killed {proc.pid}")


if __name__ == "__main__":
    main()
