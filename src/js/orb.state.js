/**
 * @fileOverview Pivot Grid columns viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

 /* global module */

'use strict';

module.exports = function() {
	var states = {};

	this.set = function(key, state) {
		states[key] = state;
	};

	this.get = function(key) {
		return states[key];
	};
};