// Sedang capek dan tidak ingin menulis mutasi

const root = document.getElementById("root");
const ws = new WebSocket("ws://localhost:8080/");

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
				name: "maxIteration",
				value: 30,
			})}
    `;
	} else if (algorithm === "HillClimbStochastic") {
		input = `
      ${createInput({
				type: "number",
				label: "Iteration",
				name: "iteration",
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
    `;
	}

	$("algorithm-parameter").innerHTML = input;
}

ws.addEventListener("message", (e) => {
	console.log(e.data);
});

function send() {
	ws.send(
		JSON.stringify({
			run: {
				size: 5,
				algorithm: {
					type: "HillClimb",
					properties: {},
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

$("start-button").addEventListener("click", send);
$("algorithm").addEventListener("change", (e) => {
	setAlgorithm(e.target.value);
});
setAlgorithm("HillClimbRandomRestart");
