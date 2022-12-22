import websocket

ws = websocket.WebSocket()
ws.connect("ws://0.0.0.0:8080/gameServer")
ws.send('{"MessageType":"getallmaps","MessageBody":"dd"}')
data = ws.recv()
ws.send('{"MessageType":"start","MessageBody":"{\\"MapId\\":0,\\"StepsToPlay\\":10}"}')
ws.close()
print("Received", repr(data))
