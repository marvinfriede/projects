// ------------------------------------------------------------------
// small helper functions
// ------------------------------------------------------------------

/**
 * Custom foreach function to avoid verbose syntax. Converts to array internally
 * @param {Object[]} list Object to iterate over
 * @param {Function} cb callback function
 * @returns {void}
 */
export const foreach = (list, cb) => Array.prototype.forEach.call(list, cb);

/**
 * Custom foreach key function to avoid verbose syntax.
 * @param {Object[]} list Object to iterate over
 * @param {Function} cb callback function
 * @returns {void}
 */
export const foreachKey = (list, cb) => Object.keys(list).forEach(cb);

/**
 * Finding the index of an element within its parentNode's children
 * @param {HTMLElement} el target element
 * @returns {Number} index of element
 */
export const getNodeIndex = (el) => [...el.parentNode.children].indexOf(el);

/**
 * Traverse parentNodes of event target until <className> is found
 * @param {Object} e event object
 * @param {String} className that is searched for in parent nodes
 * @returns {HTMLElement|boolean} HTMLElement if class found in parents, false if not found
 */
export const traverseDomUp = (e, className) => {
	let el = e.srcElement || e.target;
	if (el.classList.contains(className)) return el;
	while ((el = el.parentNode)) {
		if (el.classList && el.classList.contains(className)) return el;
	}
	return false;
};

/**
 * Split a string at the first occurence of a character. Returns string if search string is not matched.
 * @param {string} haystack string that should be split
 * @param {string} needle character at which to split
 * @param {number} [idx=0] first (0, default) or last (1) part
 * @returns {string|null} desired part of string or null on error
 */
export const splitStringAtFirstOccurence = (haystack, needle, idx = 0) => {
	const pos = haystack.indexOf(needle);
	if (pos == -1) return haystack;
	if (idx == 0) return haystack.substr(0, pos);
	if (idx == 1) return haystack.substr(pos + 1);
	return null;
};

/**
 * Split a string at the last occurence of a character. Returns string if search string is not matched.
 * @param {string} haystack string that should be split
 * @param {string} needle character at which to split
 * @param {number} [idx=1] first (0) or last (1, default) part
 * @returns {string|null} desired part of string or null on error
 */
export const splitStringAtLastOccurence = (haystack, needle, idx = 1) => {
	if (haystack.indexOf(needle) == -1) return haystack;
	if (idx == 0) return haystack.substr(0, haystack.lastIndexOf(needle));
	if (idx == 1) return haystack.substr(haystack.lastIndexOf(needle) + 1);
	return null;
};

/**
 * Split a string between first occurence of character 1 and last occurence of character 2. Returns string if search string is not matched.
 * @param {string} haystack string that should be split
 * @param {string} char1 first character at which to split
 * @param {string} char2 second character at which to split
 * @returns {string} desired substring or null on error
 */
export const splitStringBetweenTwoChars = (haystack, char1, char2) => {
	if (haystack.indexOf(char1) == -1 || haystack.indexOf(char2) == -1) return haystack;
	return haystack.substring(haystack.indexOf(char1) + 1, haystack.lastIndexOf(char2));
};

/**
 * Hide an element by adding "hidden" class, removing "visible" class and setting aria-hidden to true.
 * @param {HTMLElement} el element to show
 * @returns {void}
 */
export const hideElement = (el) => {
	if (!el) return;
	el.classList.add("hidden");
	el.classList.remove("visible");
	el.setAttribute("aria-hidden", true);
	return;
};

/**
 * Show an element by adding "visible" class, removing "hidden" class and setting aria-hidden to false.
 * @param {HTMLElement} el element to show
 * @returns {void}
 */
export const showElement = (el) => {
	if (!el) return;
	el.classList.add("visible");
	el.classList.remove("hidden");
	el.setAttribute("aria-hidden", false);
	return;
};

/**
 * Checks if string is emtpy.
 * @param {String} str
 * @returns {Boolean}
 */
export const isEmpty = (str) => str.length === 0 || !str.trim();


/**
 * Return today's date in a specified format.
 * @param {string} format allowed formats: iso, short, de
 * @returns {string|null} date in specified format or null on error
 */
export const getTodaysDate = (format) => {
	const today = new Date();
	let dd = today.getDate();
	let mm = today.getMonth() + 1;
	const yyyy = today.getFullYear();

	// add zeros
	if (dd < 10) dd = "0" + dd;
	if (mm < 10) mm = "0" + mm;

	switch (format) {
		case "iso":
			return `${yyyy}-${mm}-${dd}`;
		case "short":
			return `${mm}/${dd}/${yyyy}`;
		case "de":
			return `${mm}.${dd}.${yyyy}`;
		default:
			return null;
	}
};

/**
 * Set a timeout that can be "awaited".
 * @param {Number} ms duration in milli seconds
 * @returns {Promise}
 */
export const timeout = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

/**
 * Strict check for integer.
 * @param {any} value variable that is to check
 * @returns {Boolean}
 */
export const isInt = (value) => {
	let x;
	return isNaN(value) ? !1 : ((x = parseFloat(value)), (0 | x) === x);
};

/**
 * Return value of a CSS property as float.
 * @param {HTMLElement} el target element
 * @param {String} property name of property
 * @returns {Number} float
 */
export const getCssValue = (el, property) =>
	parseFloat(window.getComputedStyle(el).getPropertyValue(property));
