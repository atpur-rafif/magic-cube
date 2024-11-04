// Sedang capek dan tidak ingin menulis mutasi

const root = document.getElementById("root");
const initCube = document.createElement("div");
const resultCube = document.createElement("div");
const initHeader = document.createElement("h2");
const finalHeader = document.createElement("h2");
const plotHeader = document.createElement("h2");
const graphicPlot = document.createElement("div");
const detail = document.createElement("div");
const duration = document.createElement("p");
const lastValue = document.createElement("p");
const iterationCount = document.createElement("p");
const restart = document.createElement("p");
const iterPerRestart = document.createElement("p");
const plotProbability = document.createElement("div");
const plotGA = document.createElement("div");
const stuckCount = document.createElement("p");
const maxIterate = document.createElement("p");
const population = document.createElement("p");
const scrollContainer = document.createElement("div");
const ws = new WebSocket(`ws${document.location.protocol === "https:" ? "s" : ""}://${document.location.host}`);
let cube = [];

function $(id) {
	return document.getElementById(id);
}

function createInput(config) {
	if (config.type === "number")
		return `
    <div>
      <label>${config.label}</label>
      <input type="${config.type}" value="${config.value}" name="${config.name}" />
    </div>
  `;

	if (config.type === "option")
		return `
    <div>
        <label>${config.label}</label>
        <select id="${config.id}" name="">
          ${config.option.map((v) => `<option value="${v.value}">${v.label}</option>`)}
        </select>
    </div>
  `;
}

function setAlgorithm(algorithm) {
	$("algorithm").value = algorithm;

	let input = "";
	if (algorithm === "HillClimb") {
		input = "";
	} else if (algorithm === "HillClimbWithSideway") {
		input = `
      ${createInput({
				type: "number",
				label: "Maximum Iteration",
				name: "maxIteration",
				value: 100,
			})}
    `;
	} else if (algorithm === "HillClimbRandomRestart") {
		input = `
      ${createInput({
				type: "number",
				label: "Maximum Restart",
				name: "maxRestart",
				value: 30,
			})}
    `;
	} else if (algorithm === "HillClimbStochastic") {
		input = `
      ${createInput({
				type: "number",
				label: "Iteration",
				name: "maxIteration",
				value: 100_000,
			})}
    `;
	} else if (algorithm === "SimulatedAnnealing") {
		input = `
      ${createInput({
				type: "number",
				label: "Initial Temperature",
				name: "initialTemperature",
				value: 1_000_000,
			})}
    `;
	} else if (algorithm === "GeneticAlgorithm") {
		input = `
      ${createInput({
				type: "number",
				label: "Population size",
				name: "populationSize",
				value: 10,
			})}
		${createInput({
			type: "number",
			label: "Max Iteration",
			name: "maxIteration",
			value: 10,
		})}
    `;
	}

	$("algorithm-parameter").innerHTML = input;
}

plotProbability.id = "probability";
plotGA.id = "plotga";
scrollContainer.className = "scrollable";

let val = [];
let iteration = [];
let count = 0;
let currentValue = 0;
let iterationPerRestart = [];
let iterCount = 0;
let resCount = 0;
let probability = [];
let countStuck = 0;
let avg = [];
let maxPoint = [];
ws.addEventListener("message", (e) => {
	console.log(e.data);
	const algorithm = document.getElementById("algorithm").value;

	const jsonData = JSON.parse(e.data);
	if (jsonData.status == "Start") {
		cube = jsonData.data;
		fillCells(initCells, cube);
	}
	iteration = Array.from({ length: count }, (_, i) => i + 1);
	if (jsonData.status == "Finish") {
		detail.replaceChildren();
		cube = jsonData.data.matrix;
		fillCells(resultCells, cube);
		console.log(count);
		createPlot(
			iteration,
			val,
			"Iteration",
			"Objective Function",
			"plot",
			"Objective Function Plot",
			false,
			[],
		);
		duration.textContent = `Duration: ${jsonData.data.duration * 1e-9} ms`;
		lastValue.textContent = `Current Value: ${currentValue}`;
		if (
			algorithm == "HillClimb" ||
			algorithm == "HillClimbStochastic" ||
			algorithm == "HillClimbWithSideway"
		) {
			iterationCount.textContent = `Iteration: ${count}`;
		}
		detail.appendChild(lastValue);
		detail.appendChild(duration);
		detail.appendChild(iterationCount);
		if (algorithm == "HillClimbRandomRestart") {
			restart.textContent = `Restart: ${resCount}`;
			for (let i = 0; i < iterationPerRestart.length; i++) {
				const iterRestart = document.createElement("p")
				iterRestart.textContent = `Restart Iterasi ${i+1}: ${iterationPerRestart[i]} iterasi`
				scrollContainer.appendChild(iterRestart);
			}
			detail.appendChild(scrollContainer);
		}

		if (algorithm == "SimulatedAnnealing") {
			stuckCount.textContent = `Stuck Count: ${countStuck}`;
			detail.appendChild(stuckCount);
			detail.appendChild(plotProbability);
			let countIter = Array.from(
				{ length: probability.length },
				(_, i) => i + 1,
			);
			createPlot(
				countIter,
				probability,
				"Iteration",
				"Probability",
				"probability",
				"Probability Plot",
				false,
				[],
			);
		}

		if (algorithm == "GeneticAlgorithm") {
			const el = document.getElementsByTagName("input");
			for (const element of el) {
				if (element.name == "maxIteration") {
					maxIterate.textContent = `Iteration: ${element.value}`;
				}
				if (element.name == "populationSize") {
					population.textContent = `Population: ${element.value}`;
				}
			}
			detail.appendChild(population);
			detail.appendChild(maxIterate);
			detail.appendChild(plotGA);
			let countIter = Array.from({ length: iterCount }, (_, i) => i + 1);
			createPlot(
				countIter,
				avg,
				"Iteration",
				"Max and Average Point",
				"plotga",
				"Max and Average Plot",
				true,
				maxPoint,
			);
		}
		$("start-button").disabled = false;
	} 
	
	if (jsonData.status == "Update") {
		val.push(jsonData.data.point);
		count = jsonData.data.iteration;
		currentValue = jsonData.data.point;
		createPlot(Array.from({ length: count }, (_, i) => i + 1), val, "Iteration", "Objective Function", "plot", "Objective Function Plot", false, []);
		if (algorithm == "HillClimbRandomRestart") {
			if (iterationPerRestart.length <= jsonData.data.data.restartCount) {
				iterationPerRestart.push(iterCount);
				iterCount = 0;
				resCount = jsonData.data.data.restartCount;
			} else {
				iterCount += 1;
			}
		}
		if (algorithm == "SimulatedAnnealing") {
			probability.push(
				jsonData.data.data.probabilityThreshold > 1
					? 1
					: jsonData.data.data.probabilityThreshold,
			);
			countStuck = jsonData.data.data.stuckCount;
		}
		if (algorithm == "GeneticAlgorithm") {
			avg.push(jsonData.data.data.pointAverage);
			maxPoint.push(jsonData.data.point);
			iterCount = jsonData.data.iteration;
		}
	}
});

function send() {
	$("start-button").disabled = true;
	detail.replaceChildren();
	clearCells(resultCells);
	duration.textContent = "Duration: -";
	lastValue.textContent = "Current Value: -";
	Plotly.purge("plot");
	if (document.getElementById("plotga")) {
		Plotly.purge("plot");
	}
	if (document.getElementById("probability")) {
		Plotly.purge("probability");
	}
	let inputArr = {};	
	let parameter = document.getElementById("algorithm-parameter");
	let inputTag = parameter.getElementsByTagName("input");
	for (let i = 0; i < inputTag.length; i++) {
		inputArr[inputTag[i].name] = parseInt(inputTag[i].value);
	}

	const algorithm = document.getElementById("algorithm").value;
	if (algorithm === "SimulatedAnnealing") {
		inputArr.function = {
			type: "Exponential",
			divisor: 1 + 1e-3,
		};
	}
	ws.send(
		JSON.stringify({
			run: {
				size: 5,
				algorithm: {
					type: algorithm,
					properties: inputArr,
				},
			},
		}),
	);
}

root.innerHTML = `
<div>
  <button id="start-button">Start</button>
  <button id="stop-button">Stop</button>
  ${createInput({
		type: "option",
		label: "Algorithm: ",
		id: "algorithm",
		option: [
			{
				label: "Hill Climbing",
				value: "HillClimb",
			},
			{
				label: "Hill Climbing with Sideway",
				value: "HillClimbWithSideway",
			},
			{
				label: "Hill Climbing Random Restart",
				value: "HillClimbRandomRestart",
			},
			{
				label: "Hill Climbing Stochastic",
				value: "HillClimbStochastic",
			},
			{
				label: "Simulated Annealing",
				value: "SimulatedAnnealing",
			},
			{
				label: "Genetic Algorithm",
				value: "GeneticAlgorithm",
			},
		],
	})}
</div>
<div id="algorithm-parameter" style="display: flex; flex-direction: column; gap: 1rem; padding: 1rem;">
  <div>
    <label>Test</label>
    <input type="number" value=10></input>
  </div>
  
</div>
`;

function createPlot(i, value, x, y, id, judul, ga, value2) {
	const data = [];
	const line1 = {
		x: i,
		y: value,
		mode: "lines",
		type: "scatter",
	};
	data.push(line1);
	if (ga) {
		const line2 = {
			x: i,
			y: value2,
			mode: "lines",
			type: "scatter",
		};
		data.push(line2);
	}
	const layout = {
		xaxis: { range: [0, Math.max(...i)], title: x },
		yaxis: {
			range: [
				0,
				Math.max(...value) > Math.max(...value2)
					? Math.max(...value)
					: Math.max(...value2),
			],
			title: y,
		},
		title: judul,
	};
	Plotly.newPlot(id, data, layout);
}

function clearCells(cells) {
	for (let cell of cells) {
		cell.textContent = "";
	}
}

function fillCells(cells, currCube) {
	cells.forEach((cell, idx) => {
		const layerIndex = Math.floor(idx / 25); // Dapatkan indeks layer
		const rowIndex = Math.floor((idx % 25) / 5); // Dapatkan indeks baris
		const colIndex = idx % 5; // Dapatkan indeks kolom
		cell.textContent = currCube[layerIndex][rowIndex][colIndex];
	});
}

const arr = Array.from({ length: 125 }, (_, i) => i + 1);
const initCells = [];

initHeader.textContent = "Initial State";
finalHeader.textContent = "Final State";

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

const resultCells = [];
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

graphicPlot.id = "plot";
plotHeader.textContent = "Objective Function Plot";
detail.id = "detail";

root.append(
	initHeader,
	initCube,
	finalHeader,
	resultCube,
	detail,
	plotHeader,
	graphicPlot,
);

$("start-button").addEventListener("click", send);
$("stop-button").addEventListener("click", () => {
	ws.send(
		JSON.stringify({
			force: true,
		}),
	);
});

$("algorithm").addEventListener("change", (e) => {
	setAlgorithm(e.target.value);
});
setAlgorithm("HillClimbRandomRestart");
