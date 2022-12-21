import websocket

#client_sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
#client_sock.connect(("0.0.0.0", 8080))
ws = websocket.WebSocket()
ws.connect("ws://0.0.0.0:8080/gameServer")
ws.send(u'{"MessageType":"getallmaps","MessageBody":"dd"}')
data = ws.recv()
ws.send(u'{"MessageType":"start","MessageBody":"{\\"MapId\\":0,\\"StepsToPlay\\":10}"}')
ws.close()
print("Received", repr(data))
