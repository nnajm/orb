/* global module, require, react */
/*jshint eqnull: true*/

'use strict';

module.exports.forEach = function(list, func, defStop) {
	var ret;
	if(list != null) {
		for(var i = 0, l = list.length; i < l; i++) {
			ret = func(list[i], i);
			if(ret !== undefined && defStop === true) {
				break;
			}
		}
	}
	return ret;
}

module.exports.getOffset = function(element) {
	if(element != null) {
	    var rect = element.getBoundingClientRect();
	    return { x: rect.left + 0, y: rect.top + 0 };
	}
    return { x: 0, y: 0 };
}

module.exports.getSize = function(element) {
	if(element != null) {
	    var rect = element.getBoundingClientRect();
	    return { width: rect.right - rect.left, height: rect.bottom - rect.top};
	}
    return { x: 0, y: 0 };
}