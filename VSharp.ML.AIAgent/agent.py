import websocket

ws = websocket.WebSocket()
ws.connect("ws://0.0.0.0:8080/gameServer")
ws.send('{"MessageType":"getallmaps","MessageBody":"dd"}')
data = ws.recv()
print("Received 1", repr(data))
ws.send('{"MessageType":"start","MessageBody":"{\\"MapId\\":0,\\"StepsToPlay\\":10}"}')
data = ws.recv()
print("Received 2", repr(data))
ws.send(
    '{"MessageType":"step","MessageBody":"{\\"StateId\\":0,\\"PredictedStateUsefulness\\":1.0}"}'
)
data = ws.recv()
print("Received 3", repr(data))
ws.close()
