// import javascript
import { foreach } from "./utils.js";
import { data } from "./reactions.js";
import { Grid } from "ag-grid-community";

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

// ---------------------------------------------------
// init
// ---------------------------------------------------

const init = () => {
	initGrid();
	initEventListeners();
};

const initEventListeners = () => {
	const btnAutoSizeAll = document.querySelector("#autoSizeAll");
	btnAutoSizeAll.addEventListener("click", autoSizeAll);

	const btnSizeToFit = document.querySelector("#sizeToFit");
	btnSizeToFit.addEventListener("click", sizeToFit);
};

const initGrid = () => {
	const eGridDiv = document.querySelector("#grid");
	new Grid(eGridDiv, gridOptions);
};

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
// global variables
// ---------------------------------------------------

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

document.addEventListener("DOMContentLoaded", init);
