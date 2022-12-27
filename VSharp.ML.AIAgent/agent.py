import websocket
from messages import *
from game import *
import argparse


def main():
    default_server_url = "ws://0.0.0.0:8080/gameServer"
    argParser = argparse.ArgumentParser()
    argParser.add_argument("-u", "--url", help="game server url")
    args = argParser.parse_args()
    url = args.url if args.url != None else default_server_url

    ws = websocket.WebSocket()
    ws.connect(url)

    requestAllMapsMessage = ClientMessage(GetAllMapsMessageBody())
    print(requestAllMapsMessage)
    ws.send(requestAllMapsMessage.to_json())
    recieved = ws.recv()
    mapSettings = ServerMessage.from_json(recieved)
    print("Received 1: ", mapSettings, end="\n")

    startMessage = ClientMessage(StartMessageBody(MapId=0, StepsToPlay=10))
    print(startMessage)
    ws.send(startMessage.to_json())
    recieved = ws.recv()
    data = GameState.from_json(recieved)
    print("Received 2: ", data, end="\n")

    doStepMessage = ClientMessage(
        StepMessageBody(StateId=0, PredictedStateUsefulness=1.0)
    )
    print(doStepMessage)
    ws.send(doStepMessage.to_json())
    recieved = ws.recv()
    data = ServerMessage.from_json(recieved)
    print("Received 3: ", data, end="\n")
    ws.close()


if __name__ == "__main__":
    main()
