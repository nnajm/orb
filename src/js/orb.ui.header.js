
/**
 * @fileOverview Pivot Grid rows viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global orb */
/*jshint eqnull: true*/

// Ensure orb.ui namespace is created
orb.utils.ns('orb.ui');

(function(){

function cellbase(options) {	
	/**
	 * axe type (COLUMNS, ROWS, DATA, ...)
	 * @type {orb.axe.Type}
	 */
	this.axetype = options.axetype;
	/**
	 * cell type (EMPTY, DATA_VALUE, FIELD_BUTTON, INNER, WRAPPER, SUB_TOTAL, GRAND_TOTAL, ...)
	 * @type {orb.ui.HeaderType}
	 */ 
	this.type = options.type;
	/**
	 * header cell template
	 * @type {String}
	 */
	this.template = options.template;
	/**
	 * header cell value
	 * @type {Object}
	 */
	this.value = options.value;
	/**
	 * is header cell expanded
	 * @type {Boolean}
	 */
	this.expanded = true;
	/**
	 * header cell css class(es)
	 * @type {String}
	 */
	this.cssclass = options.cssclass;
	/**
	 * header cell width
	 * @type {Number}
	 */
	this.hspan = options.hspan || function() { return 1; };
	/**
	 * gets header cell's height
	 * @return {Number}
	 */
	this.vspan = options.vspan || function() { return 1; };
	/**
	 * gets wether header cell is visible
	 * @return {Boolean}
	 */
	this.visible = options.isvisible || function() { return true; };
}

/**
 * Creates a new instance of a row header.
 * @class
 * @memberOf orb.ui
 * @param  {orb.ui.rowHeader} parent - parent header.
 * @param  {orb.dimension} dim - related dimension values container.
 * @param  {orb.ui.HeaderType} type - header type (INNER, WRAPPER, SUB_TOTAL, GRAND_TOTAL).
 * @param  {orb.ui.rowHeader} totalHeader - sub total or grand total related header.
 */
orb.ui.header = function(axetype, headerType, dim, parent, subtotalHeader) {

	var self = this;

	var hspan;
	var vspan;
	var value;

    var isRowsAxe = axetype === orb.axe.Type.ROWS;
    headerType = headerType || (dim.depth === 1 ? orb.ui.HeaderType.INNER: orb.ui.HeaderType.WRAPPER);

	switch(headerType) {
		case orb.ui.HeaderType.GRAND_TOTAL:
			value = 'Grand Total';
			hspan = isRowsAxe ? dim.depth - 1 || 1 : 1;
			vspan = isRowsAxe ? 1 : dim.depth - 1 || 1;
			break;
		case orb.ui.HeaderType.SUB_TOTAL:
			value = 'Total ' + dim.value;
			hspan = isRowsAxe ? dim.depth : 1;
			vspan = isRowsAxe ? 1 : dim.depth;
			break;
		default:
			value = dim.value;
			hspan = isRowsAxe ? 1 : null;
			vspan = isRowsAxe ? null : 1;
			break;
	}

	cellbase.call(this, {
			axetype:   axetype, 
			type: headerType,
			template:  isRowsAxe ? 'cell-template-row-header' : 'cell-template-column-header', 
			value:     value,
			cssclass:  orb.ui.HeaderType.getHeaderClass(headerType, axetype),
			hspan:     hspan != null  ? function() { return hspan; } : calcSpan,
			vspan:     vspan != null  ? function() { return vspan; } : calcSpan,		
			isvisible: isParentExpanded
		}
	);

	this.subtotalHeader = subtotalHeader;
	this.parent = parent;
	this.subheaders = [];
	this.dim = dim;
	this.expanded = headerType !== orb.ui.HeaderType.SUB_TOTAL || !dim.field.subtotal.collapsed;

	this.expand = function() {
		self.expanded = true;
	}
	this.collapse = function() {
		self.expanded = false;
	}

	if(parent != null) {
		parent.subheaders.push(this);
	}

	function isParentExpanded() {
		if(self.type === orb.ui.HeaderType.SUB_TOTAL) {
			var hparent = self.parent;
			while(hparent != null) {
				if(hparent.subtotalHeader && !hparent.subtotalHeader.expanded) {
					return false;
				}
				hparent = hparent.parent;
			}
			return true;
		} else {

			var isexpanded = self.dim.isRoot || self.dim.isLeaf || !self.dim.field.subtotal.visible || self.subtotalHeader.expanded;
			if(!isexpanded) {
				return false;
			}

			var par = self.parent;
			while(par != null && (!par.dim.field.subtotal.visible || (par.subtotalHeader != null && par.subtotalHeader.expanded))) {
				par = par.parent;
			}
			return par == null || par.subtotalHeader == null ? isexpanded : par.subtotalHeader.expanded;
		}
	}

	function calcSpan() {
		var tspan = 0;
		var subSpan;
		var addone = false;

		if(self.visible()) {
			if(!self.dim.isLeaf) {
				// subdimvals 'own' properties are the set of values for this dimension
				for(var i = 0; i < self.subheaders.length; i++) {
					var subheader = self.subheaders[i];
					// if its not an array
					if(!subheader.dim.isLeaf) {
						subSpan = isRowsAxe ? subheader.vspan() : subheader.hspan();
						tspan += subSpan;
                        if(i === 0 && (subSpan === 0 || (isRowsAxe && subheader.type === orb.ui.HeaderType.SUB_TOTAL && !subheader.expanded ))) {
                            addone = true;
                        }
					} else {
						tspan += 1;
					}
				}
			} else {
				return 1;
			}
			return tspan +  (addone ? 1 : 0);
		}	
		return tspan;
	}
};

orb.ui.dataCell = function(pgrid, isvisible, rowinfo, colinfo) {

	cellbase.call(this, {
			axetype:    null, 
			type: orb.ui.HeaderType.DATA_VALUE, 
			template:   'cell-template-datavalue', 
			value:      pgrid.getData(rowinfo.dim, colinfo.dim),
			cssclass:   'cell ' + orb.ui.HeaderType.getCellClass(rowinfo.type, colinfo.type),
			isvisible:  isvisible
		}
	);
};

orb.ui.buttonCell = function(field) {

	cellbase.call(this, {
			axetype:   null, 
			type: orb.ui.HeaderType.FIELD_BUTTON, 
			template:  'cell-template-fieldbutton',
			value:     field,
			cssclass:  orb.ui.HeaderType.getHeaderClass(orb.ui.HeaderType.FIELD_BUTTON)
		}
	);
};

orb.ui.emptyCell = function(hspan, vspan) {
	
	cellbase.call(this, {
			axetype:   null, 
			type: orb.ui.HeaderType.EMPTY, 
			template:  'cell-template-empty', 
			value:     null,
			cssclass:  orb.ui.HeaderType.getHeaderClass(orb.ui.HeaderType.EMPTY),
			hspan:     function() { return hspan; },
			vspan:     function() { return vspan; },
		}
	);
};

}());