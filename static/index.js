const root = document.getElementById("root");
const display = document.createElement("div");
const startButton = document.createElement("button");
const cancelButton = document.createElement("button");
const ws = new WebSocket("ws://localhost:8080/");

ws.addEventListener("message", (e) => {
	display.insertAdjacentHTML("afterbegin", `${e.data}<br>`);
});

cancelButton.innerText = "Cancel";
cancelButton.addEventListener("click", () => {
	ws.send(JSON.stringify({ cancel: true }));
});

const cube = [
	[
		[69, 107, 97, 60, 5],
		[115, 19, 2, 120, 100],
		[18, 65, 61, 105, 89],
		[29, 36, 16, 85, 84],
		[37, 34, 48, 43, 102],
	],
	[
		[11, 111, 53, 70, 3],
		[123, 62, 38, 39, 114],
		[81, 78, 86, 50, 1],
		[72, 15, 98, 96, 46],
		[75, 28, 116, 125, 35],
	],
	[
		[52, 106, 63, 109, 76],
		[10, 73, 12, 27, 40],
		[25, 95, 77, 31, 58],
		[110, 94, 23, 118, 83],
		[59, 82, 91, 21, 14],
	],
	[
		[90, 68, 42, 17, 92],
		[41, 108, 6, 64, 93],
		[51, 7, 44, 13, 22],
		[122, 80, 47, 117, 55],
		[71, 124, 49, 57, 121],
	],
	[
		[4, 79, 54, 24, 9],
		[112, 87, 88, 74, 33],
		[30, 101, 26, 45, 113],
		[99, 56, 20, 103, 66],
		[32, 8, 119, 104, 67],
	],
];

startButton.innerText = "Start";
startButton.addEventListener("click", () => {
	ws.send(
		JSON.stringify({
			cube,
			size: 5,
			algorithm: {
				type: "HillClimbingStochastic",
				iteration: 1_000_000,
			},
		}),
	);
});

root.append(startButton, cancelButton, display);
