import logging
import os
import time
from contextlib import contextmanager, suppress

import websocket

from config import GameServerConnectorConfig
from connection.broker_conn.classes import ServerInstanceInfo

from .requests import acquire_instance, return_instance


def wait_for_connection(server_instance: ServerInstanceInfo):
    ws = websocket.WebSocket()

    retries_left = GameServerConnectorConfig.WAIT_FOR_SOCKET_RECONNECTION_MAX_RETRIES

    while retries_left:
        with suppress(
            ConnectionRefusedError,
            ConnectionResetError,
            websocket.WebSocketTimeoutException,
        ):
            ws.settimeout(GameServerConnectorConfig.CREATE_CONNECTION_TIMEOUT_SEC)
            ws.connect(
                server_instance.ws_url,
                skip_utf8_validation=GameServerConnectorConfig.SKIP_UTF_VALIDATION,
            )
        if ws.connected:
            return ws
        time.sleep(GameServerConnectorConfig.CREATE_CONNECTION_TIMEOUT_SEC)
        logging.info(
            f"Try connecting to {server_instance.ws_url}, {retries_left} attempts left; {server_instance}"
        )
        retries_left -= 1
    raise RuntimeError(
        f"Retries exsausted wnen trying to connect to {server_instance.ws_url}: {retries_left} left"
    )


@contextmanager
def game_server_socket_manager():
    server_instance = acquire_instance()

    socket = wait_for_connection(server_instance)

    try:
        socket.settimeout(GameServerConnectorConfig.RESPONCE_TIMEOUT_SEC)
        yield socket
    finally:
        socket.close()
        return_instance(server_instance)
