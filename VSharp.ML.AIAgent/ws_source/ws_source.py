from .requests import aquire_ws, return_ws


class WebsocketSource:
    def __init__(self) -> None:
        self.websocket = aquire_ws()

    def close(self):
        return_ws(self.websocket)
