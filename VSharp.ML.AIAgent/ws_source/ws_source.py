from contextlib import contextmanager

from .requests import aquire_ws, return_ws


@contextmanager
def game_server_sock():
    socket = aquire_ws()
    try:
        yield socket
    finally:
        return_ws(socket)
