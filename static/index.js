const ws = new WebSocket("ws://localhost:8080/")

ws.addEventListener("open", () => {
	ws.send(JSON.stringify({
		size: 10,
		cube: [[[]]],
		algorithm: "HillClimbings"
	}))
})

ws.addEventListener("message", (e) => {
	console.log(e.data)
})
