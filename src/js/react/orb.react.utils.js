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
};

module.exports.getOffset = function(element) {
	if(element != null) {
	    var rect = element.getBoundingClientRect();
	    return { x: rect.left, y: rect.top };
	}
    return { x: 0, y: 0 };
};

module.exports.getParentOffset = function(element) {
	if(element != null) {
	    var rect = element.getBoundingClientRect();
	    var rectParent = element.parentNode != null ? element.parentNode.getBoundingClientRect() : { top: 0, left: 0} ;
	    return { x: rect.left - rectParent.left, y: rect.top - rectParent.top };
	}
    return { x: 0, y: 0 };
};

module.exports.getSize = function(element) {
	if(element != null) {
	    var rect = element.getBoundingClientRect();
	    return { width: rect.right - rect.left, height: rect.bottom - rect.top};
	}
    return { width: 0, height: 0 };
};