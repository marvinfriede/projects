// import javascript
import { data } from "./reactions.js";
import { Grid } from "ag-grid-community";

import { foreach, isEmpty, splitStringAtLastOccurence } from "./utils.js";

// import styles
import "../css/main.css";
import "ag-grid-community/dist/styles/ag-grid.css";
import "ag-grid-community/dist/styles/ag-theme-alpine.css";

// ---------------------------------------------------
// grid data
// ---------------------------------------------------

const columnDefs = [
	{ headerName: "Name", field: "name" },
	{ headerName: "type", field: "category", width: 100 },
	{ headerName: "educt", field: "educt", width: 120 },
	{ headerName: "reagents", field: "reagents" },
	{ headerName: "product", field: "product", width: 120 },
	{ headerName: "conditions", field: "conditions", sortable: false, width: 120 },
	{ headerName: "tolerated functional groups", field: "tfg", sortable: false },
	{
		headerName: "advantages",
		field: "advantages",
		sortable: false,
	},
	{
		headerName: "disadvantages",
		field: "disadvantages",
		sortable: false,
	},
];

const rowData = data.organic;

// ---------------------------------------------------
// resize grid
// ---------------------------------------------------

const autoSizeAll = (skipHeader = false) => {
	const allColIds = [];
	const allCols = gridOptions.columnApi.getAllColumns();

	foreach(allCols, (col) => allColIds.push(col.colId));
	gridOptions.columnApi.autoSizeColumns(allColIds, skipHeader);
};

const sizeToFit = () => gridOptions.api.sizeColumnsToFit();

// ---------------------------------------------------
// grid functions
// ---------------------------------------------------

const onFirstDataRendered = (params) => {
	params.api.sizeColumnsToFit();
};

const onColumnResized = (params) => {
	params.api.resetRowHeights();
};

const onColumnVisible = (params) => {
	params.api.resetRowHeights();
};

// let the grid know which columns and what data to use
let gridOptions = {
	columnDefs: columnDefs,
	defaultColDef: {
		autoHeight: true,
		filter: "agTextColumnFilter",
		floatingFilter: true,
		resizable: true,
		sortable: true,
		wrapText: true,
	},
	onColumnResized: onColumnResized,
	onColumnVisible: onColumnVisible,
	onFirstDataRendered: onFirstDataRendered,
	rowData: rowData,
};

const initGrid = () => {
	const eGridDiv = document.querySelector("#grid");
	new Grid(eGridDiv, gridOptions);

	const btnAutoSizeAll = document.querySelector("#autoSizeAll");
	btnAutoSizeAll.addEventListener("click", autoSizeAll);

	const btnSizeToFit = document.querySelector("#sizeToFit");
	btnSizeToFit.addEventListener("click", sizeToFit);
};

// -------------------------------------------------
// smooth page transition
// -------------------------------------------------

const getUrl = () => {
	const baseUrl = location.protocol + "//" + location.host;
	const path = splitStringAtLastOccurence(location.pathname, "/", 0);
	return baseUrl + path;
};

const getPage = async (url) => {
	// get other page via fetch and replace content
	try {
		const response = await fetch(url);
		if (!response.ok) {
			const message = `An error has occured: ${response.status}`;
			throw new Error(message);
		}

		const html = await response.text();
		const doc = new DOMParser().parseFromString(html, "text/html");

		const varContent = document.querySelector("main");
		const grid = varContent.firstElementChild;
		const newContent = doc.querySelector("body div");

		grid.remove();
		varContent.insertBefore(newContent, null);
	} catch (err) {
		console.error(err);
	}
};

const loadPageFromHash = async () => {
	// check if hash empty or different from any allowed
	const hash = location.hash;
	const allowedHashs = {
		"#all": "projects/all.html",
		"#study": "projects/study.html",
		"#personal": "projects/personal.html",
		"#misc": "projects/misc.html",
		"#datenEinaxAlter": "projects/datenEinaxAlter.html",
		"#dijkstraParallel": "projects/dijkstraParallel.html",
		"#dijkstraSequential": "projects/dijkstraSequential.html",
		"#dftHelium": "projects/dftHelium.html",
		"#hmo": "projects/hmo.html",
		"#javascriptShortcuts": "projects/javascriptShortcuts.html",
		"#kineticModel": "projects/kineticModel.html",
		"#logisticRegression": "projects/logisticRegression.html",
		"#linearRegression": "projects/linearRegression.html",
		"#mandelbrotSetSequential": "projects/mandelbrotSetSequential.html",
		"#matrixExercise": "projects/matrixExercise.html",
		"#monteCarloSimulation": "projects/monteCarloSimulation.html",
		"#ompPrivateDemo": "projects/ompPrivateDemo.html",
		"#reactionDatabase": "projects/reactionDatabase.html",
		"#trapezoidalRulePi": "projects/trapezoidalRulePi.html",
	};
	if (isEmpty(hash) || !(hash in allowedHashs)) return;

	// check if this hash is not already active
	const a = document.querySelector(hash);
	const span = a.firstElementChild;
	if (span.classList.contains("active")) return;

	// style active and get page
	makeTargetActiveLink(span);
	await getPage(`${getUrl()}/${allowedHashs[hash]}`);

	if (hash == "#reactionDatabase") {
		initGrid();
	}
};

const makeTargetActiveLink = (el) => {
	const links = document.querySelectorAll("nav .link span");
	foreach(links, (link) => link.classList.remove("active"));

	el.classList.add("active");
};

// -------------------------------------------------
// inits
// -------------------------------------------------

const initEventListeners = () => {
	window.addEventListener("hashchange", loadPageFromHash);
};

const init = () => {
	initEventListeners();
	loadPageFromHash();
};

document.addEventListener("DOMContentLoaded", init);

// -------------------------------------------------
// old
// -------------------------------------------------

window.onload = () => {
	if (document.body.dataset.pageName == "projects") {
		initEventListenersProjects();
	}
};

// -------------------------------------------------
// event listeners
// -------------------------------------------------

const initEventListenersProjects = () => {
	const sort = document.querySelector(".sort-btn");
	sort.addEventListener("click", () => sortProjects());

	const drop_opts = document.querySelectorAll(".dropdown-content__option");
	foreach(drop_opts, (el) =>
		el.addEventListener("click", (e) => selectDropdownOption(e))
	);

	const drops = document.querySelectorAll(".dropdown-wrap");
	foreach(drops, (el) => {
		el.addEventListener("mouseenter", (e) => openDropdown(e));
		el.addEventListener("mouseleave", (e) => closeDropdown(e));
	});
};

// -------------------------------------------------
// scroll function
// -------------------------------------------------

// show the button after scrolling down 60px
const scrollFunction = () => {
	const btn_scroll = document.querySelector(".btn-scroll");
	if (document.body.scrollTop > 60) {
		btn_scroll.style.display = "block";
		return;
	}
	btn_scroll.style.display = "none";
};
const scrollToTop = () => (document.body.scrollTop = 0);

// -------------------------------------------------
// sorting
// -------------------------------------------------

const sortProjects = () => {
	const selector = document.querySelector(".sort-select").getAttribute("data-label");
	const projects = document.querySelectorAll(".project-container");

	// execute animated sort functions
	animateSelect(projects, selector);
};
const animateSelect = (projects, selector) => {
	foreach(projects, (project) => {
		project.style.display = "flex";
		if (selector == "all") return;
		if (selector !== project.getAttribute("data-lang")) {
			project.style.display = "none";
		}
	});
};

// -------------------------------------------------
// custom dropdown
// -------------------------------------------------

const openDropdown = (e) => {
	e.target.querySelector(".dropdown-content").style.display = "flex";
	e.target.parentNode.querySelector(".dropdown-caret").classList.add("caret-open");
};
const closeDropdown = (e) => {
	e.target.querySelector(".dropdown-content").style.display = "none";
	e.target.parentNode.querySelector(".dropdown-caret").classList.remove("caret-open");
};
const selectDropdownOption = (e) => {
	const opt = e.target.innerText;
	const optVal = e.target.getAttribute("data-label");
	const drop = traverseDomUp(e, "dropdown");
	const currEl = drop.querySelector(".dropdown-current-element");

	currEl.innerText = opt;
	currEl.setAttribute("data-label", optVal);
	e.target.parentNode.style.display = "none";
};
