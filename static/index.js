const root = document.getElementById("root");
const display = document.createElement("div");
const startButton = document.createElement("button");
const cancelButton = document.createElement("button");
const generateButton = document.createElement("button");
const methodRadio = document.createElement("input");
const slider = document.createElement("input");
const sliderDisplay = document.createElement("div");
const initCube = document.createElement("div");
const resultCube = document.createElement("div");
const graphicPlot = document.createElement("div");
const ws = new WebSocket("ws://localhost:8080/");
const algorithmsName = ["Hill Climbing", "Hill Climbing With Sideaway", "Stochastic Hill Climbing", "Random Restart Hill Climbing", "Simulated Annealing", "Genetic Algoritm"];
const algorithmsValue = ["HillClimbing", "HillClimbingWithSideway", "HillClimbingStochastic", "HillClimbingRandomRestart", "SimulatedAnnealing", "GeneticAlgorithm"];

function setValue(value) {
	slider.value = value;
	sliderDisplay.innerText = `${value}ms`;
	ws.send(
		JSON.stringify({
			timing: parseInt(value),
		}),
	);
}
slider.type = "range";
slider.min = 10;
slider.max = 10000;
slider.addEventListener("input", (e) => {
	setValue(e.target.value);
});

graphicPlot.id = "myPlot";
let val = [];
let iteration = [];
let count = 1;
ws.addEventListener("message", (e) => {
	const jsonData = JSON.parse(e.data);
	if ("result" in jsonData) {
		console.log(jsonData.result);
		fillCells(resultCells, jsonData.result);
		const data = [{
			x: iteration,
			y: val,
			mode:"markers",
			type:"scatter"
		}];
		const layout = {
			xaxis: {range: [0, 100], title: "Iteration"},
			yaxis: {range: [0, 60], title: "Objective Function"},
			title: "Plot"
		};
		console.log(val);
		console.log(iteration);
		Plotly.newPlot('myPlot', data, layout);
	} else {
		val.push(jsonData.point);
		iteration.push(count);
		count += 1;
	}
	display.insertAdjacentHTML("afterbegin", `${e.data}<br>`);
});

cancelButton.innerText = "Cancel";
cancelButton.addEventListener("click", () => {
	ws.send(JSON.stringify({ cancel: true }));
});

// const cube = [
// 	[
// 		[69, 107, 97, 60, 5],
// 		[115, 19, 2, 120, 100],
// 		[18, 65, 61, 105, 89],
// 		[29, 36, 16, 85, 84],
// 		[37, 34, 48, 43, 102],
// 	],
// 	[
// 		[11, 111, 53, 70, 3],
// 		[123, 62, 38, 39, 114],
// 		[81, 78, 86, 50, 1],
// 		[72, 15, 98, 96, 46],
// 		[75, 28, 116, 125, 35],
// 	],
// 	[
// 		[52, 106, 63, 109, 76],
// 		[10, 73, 12, 27, 40],
// 		[25, 95, 77, 31, 58],
// 		[110, 94, 23, 118, 83],
// 		[59, 82, 91, 21, 14],
// 	],
// 	[
// 		[90, 68, 42, 17, 92],
// 		[41, 108, 6, 64, 93],
// 		[51, 7, 44, 13, 22],
// 		[122, 80, 47, 117, 55],
// 		[71, 124, 49, 57, 121],
// 	],
// 	[
// 		[4, 79, 54, 24, 9],
// 		[112, 87, 88, 74, 33],
// 		[30, 101, 26, 45, 113],
// 		[99, 56, 20, 103, 66],
// 		[32, 8, 119, 104, 67],
// 	],
// ];

// Fungsi untuk mengacak elemen array menggunakan algoritma Fisher-Yates
function shuffle(array) {
	for (let i = array.length - 1; i > 0; i--) {
	  const j = Math.floor(Math.random() * (i + 1));
	  [array[i], array[j]] = [array[j], array[i]];
	}
	return array;
}

const arr = Array.from({ length: 125 }, (_, i) => i + 1);  
let cube = [];

startButton.innerText = "Start";
startButton.addEventListener("click", () => {
	setValue(100);
	const selectedMethod = document.querySelector('input[name="algorithm"]:checked')
	if (selectedMethod && cube.length != 0) {
		let constraint = "";
		if (selectedMethod.value === "HillClimbingWithSideway") {
			constraint = "maximum";
		} else if (selectedMethod.value === "HillClimbingStochastic") {
			constraint = "iteration";
		} else if (selectedMethod.value === "HillClimbingRandomRestart") {
			constraint = "count";
		} else if (selectedMethod.value === "GeneticAlgorithm") {
			constraint = "pool"
		}
 		ws.send(
			JSON.stringify({
				cube,
				size: 5,
				transformer: "Digital",
				algorithm: {
					type: selectedMethod.value,
					[constraint]: 100,
				},
			}),
		);
	}
	console.log(cube);  

});

function fillCells(cells, currCube) {
	cells.forEach((cell, idx) => {
        const layerIndex = Math.floor(idx / 25); // Dapatkan indeks layer
        const rowIndex = Math.floor((idx % 25) / 5); // Dapatkan indeks baris
        const colIndex = idx % 5; // Dapatkan indeks kolom
        cell.textContent = currCube[layerIndex][rowIndex][colIndex];
    });
}

generateButton.innerText = "Generate Initial State"
generateButton.addEventListener("click", () => {
	shuffle(arr);
	cube = [];
	let index = 0;
	for (let i = 0; i < 5; i++) {
	const layer = [];
	for (let j = 0; j < 5; j++) {
	  const row = [];
	  for (let k = 0; k < 5; k++) {
		row.push(arr[index]);
		index++;
	  }
	  layer.push(row);
	}
	cube.push(layer);
	}
	fillCells(initCells, cube)
	console.log(cube);
})

algorithmsName.forEach((alg, index) => {
    const radio = document.createElement("input");
    radio.type = "radio";
    radio.name = "algorithm";
    radio.value = algorithmsValue[index];
    radio.id = `algorithm${index}`;

    const label = document.createElement("label");
    label.htmlFor = radio.id;
    label.innerText = alg;

	root.append(radio, label, document.createElement("br"))
});

const initCells = [];
initCube.classList.add("container");
resultCube.classList.add("container");
for (let layerIndex = 0; layerIndex < 5; layerIndex++) {
	const layer = document.createElement("div");
	layer.classList.add("layer");

	for (let i = 0; i < 25; i++) {
        const cell = document.createElement("div");
        cell.classList.add("cell");
        layer.appendChild(cell);
        initCells.push(cell); 
    }

	initCube.appendChild(layer);
}

const resultCells = []
for (let layerIndex = 0; layerIndex < 5; layerIndex++) {
	const layer = document.createElement("div");
	layer.classList.add("layer");
	for (let i = 0; i < 25; i++) {
        const cell = document.createElement("div");
        cell.classList.add("cell");
        layer.appendChild(cell);
        resultCells.push(cell); 
    }

	resultCube.appendChild(layer);
}

root.append(startButton, cancelButton, generateButton, slider, sliderDisplay, initCube, resultCube, graphicPlot, display);
