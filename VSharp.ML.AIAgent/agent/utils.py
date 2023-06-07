import logging
import queue
from contextlib import closing
from enum import Enum

import websocket

from common.game import GameMap

from .messages import (
    ClientMessage,
    GetTrainMapsMessageBody,
    GetValidationMapsMessageBody,
    MapsServerMessage,
)


class MapsType(Enum):
    TRAIN = "Train"
    VALIDATION = "Validation"


def switch_maps_type(
    ws_strings_queue: queue.Queue,
    type: MapsType,
):
    websocket_strings = []
    while not ws_strings_queue.empty():
        websocket_strings.append(ws_strings_queue.get())

    def send_all(message_body: ClientMessage):
        for ws_string in websocket_strings:
            with closing(websocket.create_connection(ws_string)) as ws:
                ws.send(message_body.to_json())
                ws.recv()

    match type:
        case MapsType.TRAIN:
            request_all_maps_message = ClientMessage(GetTrainMapsMessageBody())
        case MapsType.VALIDATION:
            request_all_maps_message = ClientMessage(GetValidationMapsMessageBody())

    send_all(request_all_maps_message)

    for ws_string in websocket_strings:
        ws_strings_queue.put(ws_string)
    logging.info(f"switched servers to {type.value} mode")


def get_maps(ws_strings_queue: queue.Queue, type: MapsType) -> list[GameMap]:
    match type:
        case MapsType.TRAIN:
            request_all_maps_message = ClientMessage(GetTrainMapsMessageBody())
        case MapsType.VALIDATION:
            request_all_maps_message = ClientMessage(GetValidationMapsMessageBody())

    any_map_socket_url = ws_strings_queue.get()
    with closing(websocket.create_connection(any_map_socket_url)) as ws:
        ws.send(request_all_maps_message.to_json())
        maps_message = ws.recv()
    ws_strings_queue.put(any_map_socket_url)

    return MapsServerMessage.from_json_handle(
        maps_message, expected=MapsServerMessage
    ).MessageBody.Maps
