import time
from contextlib import contextmanager, suppress

import websocket

from config import GameServerConnectorConfig
from connection.broker_conn.classes import WSUrl

from .requests import acquire_instance, return_instance


def wait_for_connection(url: WSUrl):
    ws = websocket.WebSocket()

    retries_left = 60

    while retries_left:
        with suppress(ConnectionRefusedError, ConnectionResetError):
            ws.settimeout(GameServerConnectorConfig.CREATE_CONNECTION_TIMEOUT)
            ws.connect(
                url, skip_utf8_validation=GameServerConnectorConfig.SKIP_UTF_VALIDATION
            )
        if ws.connected:
            return ws
        time.sleep(GameServerConnectorConfig.CREATE_CONNECTION_TIMEOUT)
        retries_left -= 1
    raise RuntimeError("Retries exsausted")


@contextmanager
def game_server_socket_manager():
    server_instance = acquire_instance()

    socket = None
    try:
        socket = wait_for_connection(server_instance.ws_url)
        socket.settimeout(GameServerConnectorConfig.RESPONCE_TIMEOUT_SEC)
        yield socket
    finally:
        socket.close()
        return_instance(server_instance)
