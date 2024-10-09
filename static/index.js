const ws = new WebSocket("ws://localhost:8080/")

ws.addEventListener("open", () => {
	ws.send("World")
})

ws.addEventListener("message", (e) => {
	console.log(e.data)
})
