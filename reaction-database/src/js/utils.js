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