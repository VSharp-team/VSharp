import websocket
import argparse
from contextlib import closing

from messages import ClientMessage
from messages import GetAllMapsMessageBody
from messages import StartMessageBody
from messages import StepMessageBody
from messages import ServerMessage
from messages import ServerMessageType
from game import GameState
from game import State


def get_states(game_state: GameState) -> set[State]:
    states = set()
    for edge in game_state.Map:
        for state in edge.VertexFrom.States:
            states.add(state)
        for state in edge.VertexTo.States:
            states.add(state)

    return states


def choose_state_id(game_state: GameState) -> int:
    return get_states(game_state).pop()


def decode(msg) -> ServerMessage | GameState | str:
    for ServerMessageClass in [ServerMessage, GameState]:
        try:
            return ServerMessageClass.from_json(msg)
        except KeyError:
            pass

    return msg


class Agent:
    def __init__(self, url) -> None:
        self.url = url

    def play(self, steps=10):
        with closing(websocket.create_connection(self.url)) as ws:
            request_all_maps_message = ClientMessage(GetAllMapsMessageBody())
            print("-->", request_all_maps_message)
            ws.send(request_all_maps_message.to_json())

            map_settings = ServerMessage.from_json(ws.recv())
            chosen_map_id = map_settings.MessageBody[0]["Id"]

            start_message = ClientMessage(
                StartMessageBody(MapId=chosen_map_id, StepsToPlay=steps)
            )
            print("-->", start_message, "\n")
            ws.send(start_message.to_json())

            while True:
                received = decode(ws.recv())

                match received:
                    case GameState() as game_state:
                        print(f"next_states={[x.Id for x in get_states(game_state)]}")
                        next_state_id = choose_state_id(game_state).Id
                        print(f"{next_state_id=}")
                        do_step_message = ClientMessage(
                            StepMessageBody(
                                StateId=next_state_id, PredictedStateUsefulness=42.0
                            )
                        )
                        print("-->", do_step_message)
                        ws.send(do_step_message.to_json())

                    case ServerMessage() as server_msg:
                        print("<--", server_msg, end="\n\n")
                        if server_msg.MessageType in (
                            ServerMessageType.GAMEOVER,
                            ServerMessageType.INCORRECT_PREDICTED_STATEID,
                        ):
                            print("Game was stopped")
                            break


def main():
    default_server_url = "ws://0.0.0.0:8080/gameServer"
    argParser = argparse.ArgumentParser()
    argParser.add_argument("-u", "--url", help="game server url")
    args = argParser.parse_args()
    url = args.url if args.url != None else default_server_url

    agent = Agent(url)
    agent.play()


if __name__ == "__main__":
    main()
