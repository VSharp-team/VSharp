from contextlib import contextmanager

import websocket

from .requests import aquire_ws, return_ws


@contextmanager
def game_server_socket_manager():
    socket_url = aquire_ws()
    socket = websocket.create_connection(socket_url, skip_utf8_validation=True)
    try:
        yield socket
    finally:
        socket.close()
        return_ws(socket_url)
