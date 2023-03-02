from queue import Queue

import websocket


class ConnectionManager:
    def __init__(self, urls: list[str]) -> None:
        self.socket_q: Queue[websocket.WebSocket] = Queue()
        for url in urls:
            self.socket_q.put(websocket.create_connection(url))

    def issue(self) -> websocket.WebSocket:
        return self.socket_q.get()

    def release(self, ws: websocket.WebSocket):
        self.socket_q.put(ws)

    def close(self):
        self.socket_q.empty()
        while not self.socket_q.empty():
            s = self.socket_q.get()
            s.close()
