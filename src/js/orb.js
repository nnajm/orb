/**
 * Root namespace.
 * @namespace orb
 */

/**
 * Utility functions namespace.
 * @namespace utils
 * @memberOf orb
 */

/**
 * Reactjs components namespace.
 * @namespace react
 * @memberOf orb
 */

/**
 * UI namespace.
 * @namespace ui
 * @memberOf orb
 */

orb = {};
orb.utils = {
	/**
	 * Creates a namespcae hierarchy if not exists
	 * @param  {string} identifier - namespace identifier
	 * @return {object}
	 */
	ns: function(identifier){
		var parts = identifier.split('.');
		var parent = window;
		var i = 0;
		while(i<parts.length) {
			parent[parts[i]] = parent[parts[i]] || {};
			parent = parent[parts[i]];
			i++;
		}
		return parent;
	},
	/**
	 * Returns an array of object own properties
	 * @param  {Object} obj
	 * @return {Array}
	 */
	ownProperties: function(obj) {
		var arr = [];
		for(var prop in obj) {
			if(obj.hasOwnProperty(prop)) {
				arr.push(prop);
			}
		}
		return arr;
	},
	/**
	 * Returns whether or not the supplied obj is a javascript array.
	 * @param  {object}  obj
	 * @return {Boolean}
	 */
	isArray: function(obj) {
		return Object.prototype.toString.apply(obj) === '[object Array]';
	},
	/**
	 * Returns a JSON string represenation of an object
	 * @param {object} obj
	 * @return {string}
	 */
	jsonStringify: function(obj, censorKeywords){
		function censor(key, value) {
			return censorKeywords && censorKeywords.indexOf(key) > -1 ? undefined: value;
		}
		return JSON.stringify(obj, censor, 2);
	}
};