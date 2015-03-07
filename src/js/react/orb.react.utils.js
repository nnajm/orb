/* global module, require, react, window, document */
/*jshint eqnull: true*/

'use strict';

module.exports.forEach = function(list, func, defStop) {
	var ret;
	if(list) {
		for(var i = 0, l = list.length; i < l; i++) {
			ret = func(list[i], i);
			if(ret !== undefined && defStop === true) {
				break;
			}
		}
	}
	return ret;
};

module.exports.removeClass = function(element, classname) {
	if(element && classname) {
		while(element.className.indexOf(classname) >= 0) {
			element.className = element.className.replace(classname, '');
		}
	}
};

module.exports.addClass = function(element, classname) {
	if(element && classname) {
		if(element.className.indexOf(classname) < 0) {
			element.className += ' ' + classname;
		}
	}
};

module.exports.getOffset = function(element) {
	if(element) {
	    var rect = element.getBoundingClientRect();
	    return { x: rect.left, y: rect.top };
	}
    return { x: 0, y: 0 };
};

module.exports.getParentOffset = function(element) {
	if(element) {
	    var rect = element.getBoundingClientRect();
	    var rectParent = element.parentNode != null ? element.parentNode.getBoundingClientRect() : { top: 0, left: 0} ;
	    return { x: rect.left - rectParent.left, y: rect.top - rectParent.top };
	}
    return { x: 0, y: 0 };
};

module.exports.getSize = function(element) {
	if(element) {
	    var rect = element.getBoundingClientRect();
	    return { width: rect.right - rect.left, height: rect.bottom - rect.top};
	}
    return { width: 0, height: 0 };
};

module.exports.getStyle = function(element, styleProps, keepString)
{
	var values = [];
	if(element && styleProps) {
		var currStyle, f;
		if (element.currentStyle) {
			currStyle = element.currentStyle;
			f = function(prop) { return currStyle[prop]; };
		} else if (window && window.getComputedStyle) {
			currStyle = window.getComputedStyle(element,null);
			f = function(prop) { return currStyle.getPropertyValue(prop); };
		}

		for(var i = 0; i < styleProps.length; i++) {
			var val = f(styleProps[i]);
			values.push(val && keepString !== true ? Math.ceil(parseFloat(val)) : val);
		}
	}
	return values;
};

module.exports.isVisible = function(element) {
	if(element) {
		return element.style.display !== 'none' && (element.offsetWidth !== 0 || element.offsetHeight !== 0);
	}
	return false;
};

module.exports.updateTableColGroup = function(tableNode, widths) {
	if(tableNode) {
	    var colGroupNode = tableNode.firstChild;
	    if(colGroupNode && colGroupNode.nodeName === 'COLGROUP') {
		    tableNode.style.tableLayout = 'auto';
		    tableNode.style.width = '';

		    colGroupNode.innerHTML = '';
		    for(var i = 0; i < widths.length; i++) {
		      var col = document.createElement('col');
		      col.style.width = widths[i] + 'px';
		      colGroupNode.appendChild(col);
		    }
		    tableNode.style.tableLayout = 'fixed';
		}
	}
  };