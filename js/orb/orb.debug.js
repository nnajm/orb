/*! orb v0.1.0, Javascript pivot grid library.
 *  (c) Najmeddine Nouri, 2014-11-23.
 *  Licence: MIT.
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
	 * Returns the first element in the array that satisfies the given predicate
	 * @param  {Array} array     the array to search
	 * @param  {function} predicate Function to apply to each element until it returns true
	 * @return {Object}           The first object in the array that satisfies the predicate or undefined.
	 */
	findInArray: function(array, predicate) {
		if(orb.utils.isArray(array) && predicate) {
			for(var i = 0; i < array.length; i++) {
				var item = array[i];
				if(predicate(item)) {
					return item;
				}
			}
		}
		return undefined;
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
};;'use strict';

/* global orb */
/*jshint eqnull: true*/

(function() {

function getpropertyvalue(property, configs, defaultvalue) {
	for(var i = 0; i < configs.length; i++) {
		if(configs[i][property] != null) {
			return configs[i][property];
		}
	}
	return defaultvalue;
}

function mergefieldconfigs() {

	var configs = [];
	var sorts = [];
	var subtotals = [];
	var filters = [];

	for(var i = 0; i < arguments.length; i++) {
		var nnconfig = arguments[i] || {};
		configs.push(nnconfig);
		sorts.push(nnconfig.sort || {});
		subtotals.push(nnconfig.subtotal || {});
		filters.push(nnconfig.filter || {});
	}
	
	return {		
		name: getpropertyvalue('name', configs, ''),

		caption: getpropertyvalue('caption', configs, ''),
		filter: {
			type: getpropertyvalue('type', filters, 'operator'),
			regexp: getpropertyvalue('regexp', filters, null),
			operator: getpropertyvalue('operator', filters, null),
			value: getpropertyvalue('value', filters, null)
		},
	
		sort: {
			order: getpropertyvalue('order', sorts, null),
			customfunc: getpropertyvalue('customfunc', sorts, null)
		},
		subtotal: { 
			visible: getpropertyvalue('visible', subtotals, true),
			collapsible: getpropertyvalue('collapsible', subtotals, true),
			collapsed: getpropertyvalue('collapsed', subtotals, false)
		},

		aggregatefunc: getpropertyvalue('aggregatefunc', configs, null),	
		formatfunc: getpropertyvalue('formatfunc', configs, null)
	}
}

function createfield(rootconfig, axetype, fieldconfig, defaultfieldconfig) {

	var axeconfig;

	if(defaultfieldconfig) {
		switch(axetype) {
			case orb.axe.Type.ROWS:
				axeconfig = defaultfieldconfig.rowsettings;
				break;
			case orb.axe.Type.COLUMNS:
				axeconfig = defaultfieldconfig.columnsettings;
				break;
			case orb.axe.Type.DATA:
				axeconfig = defaultfieldconfig.datasettings;
				break;
			default:
				axeconfig = null;
				break;
		}
	} else {
		axeconfig = null;
	}

	return mergefieldconfigs(fieldconfig, axeconfig, defaultfieldconfig, rootconfig)
}

function grandtotalconfig(options) {

	options = options || {};
	
	this.rowsvisible = options.rowsvisible !== undefined ? options.rowsvisible : true;
	this.columnsvisible = options.columnsvisible !== undefined ? options.columnsvisible : true;
}

function subtotalconfig(options, setdefaults) {
	
	var defaults = {
		visible: setdefaults === true ? true : undefined,
		collapsible: setdefaults === true ? true : undefined,
		collapsed: setdefaults === true ? false : undefined
	};
	options = options || {};

	this.visible = options.visible !== undefined ? options.visible : defaults.visible;
	this.collapsible = options.collapsible !== undefined ? options.collapsible : defaults.collapsible;
	this.collapsed = options.collapsed !== undefined ? options.collapsed : defaults.collapsed;
}

function sortconfig(options) {
	options = options || {};

	this.order = options.order;
	this.customfunc = options.customfunc;
}

function filterconfig(options) {
	options = options || {};

	this.type = options.type;
	this.regexp = options.regexp;
	this.operator = options.operator;
	this.value = options.value;
}

orb.field = function(options, suboptions) {
	
	options = options || {};

	// field name
	this.name = options.name;
 
	// shared settings
	this.caption = options.caption || this.name;
	this.filter = new filterconfig(options.filter);

	// rows & columns settings
	this.sort = new sortconfig(options.sort);
	this.subtotal = new subtotalconfig(options.subtotal);

	// data settings
	this.aggregatefunc = options.aggregatefunc;
	this.formatfunc = options.formatfunc;

	if(suboptions !== true) {
		(this.rowsettings = new orb.field(options.rowsettings, true)).name = this.name;
		(this.columnsettings = new orb.field(options.columnsettings, true)).name = this.name;
		(this.datasettings = new orb.field(options.datasettings, true)).name = this.name;
	}
}


/**
 * Creates a new instance of pgrid
 * @class
 * @memberOf orb
 * @param  {object} config - configuration object
 */
orb.config = function(config) {

	var self = this;

	this.datasource = config.datasource;
	this.dataheaderslocation = config.dataheaderslocation === 'columns' ? 'columns' : 'rows';
	this.grandtotal =  new grandtotalconfig(config.grandtotal);
	this.subtotal = new subtotalconfig(config.subtotal, true);

	this.allfields = (config.fields || []).map(function(fieldconfig) {
		return new orb.field(fieldconfig);
    });

   	this.rowfields = (config.rows || []).map(function(fieldconfig) {
		return createfield(self, orb.axe.Type.ROWS, fieldconfig, getfield(self.allfields, fieldconfig.name));
    });

   	this.columnfields = (config.columns || []).map(function(fieldconfig) {
		return createfield(self, orb.axe.Type.COLUMNS, fieldconfig, getfield(self.allfields, fieldconfig.name));
    });

   	this.datafields = (config.data || []).map(function(fieldconfig) {
		return createfield(self, orb.axe.Type.DATA, fieldconfig, getfield(self.allfields, fieldconfig.name));
    });

    this.datafieldscount = this.datafields ? (this.datafields.length || 1) : 1;

    function getfield(axefields, fieldname) {
    	var fieldindex = getfieldindex(axefields, fieldname);
    	if(fieldindex > -1) {
    		return axefields[fieldindex];
    	}
    	return null;
    }

    function getfieldindex(axefields, fieldname) {
    	for(var fi = 0; fi < axefields.length; fi++) {
    		if(axefields[fi].name === fieldname) {
    			return fi;
    		}
    	}
    	return -1;
    }

	self.availablefields = function() {
		return self.allfields.filter(function(field) {
			var notequalfield = function(otherfield) {
				return field.name !== otherfield.name;
			};

			return self.datafields.every(notequalfield) &&
					self.rowfields.every(notequalfield) &&
					self.columnfields.every(notequalfield);
		});
	};

	this.moveField = function(fieldname, oldaxetype, newaxetype, position) {
		
		var oldaxe, oldposition;
		var newaxe;
		var field = getfield(self.allfields, fieldname);
		
		if(field) {

			switch(oldaxetype){
				case orb.axe.Type.ROWS: 
					oldaxe = self.rowfields;
					break;
				case orb.axe.Type.COLUMNS:
					oldaxe = self.columnfields;
					break;
				case orb.axe.Type.DATA:
					oldaxe = self.datafields;
					break;
				default:
					break;
			}

			switch(newaxetype){				
				case orb.axe.Type.ROWS: 
					newaxe = self.rowfields;
					break;
				case orb.axe.Type.COLUMNS:
					newaxe = self.columnfields;
					break;
				case orb.axe.Type.DATA:
					newaxe = self.datafields;
					break;
				default:
					break;
			}

			if(oldaxe || newaxe) {

				if(oldaxe) {
					oldposition = getfieldindex(oldaxe, fieldname);
					if(oldaxetype === newaxetype){
						if(oldposition == oldaxe.length - 1 &&
							position == null ||
							oldposition === position - 1) {
							return false;
						}
					} 
					oldaxe.splice(oldposition, 1);
				}

				field = createfield(self, newaxetype, null, field);

				if(newaxe) {
					if(position != null) {
						newaxe.splice(position, 0, field);
					} else {
						newaxe.push(field);
					}
				}

				// update data fields count
				self.datafieldscount = self.datafields ? (self.datafields.length || 1) : 1;

				return true;
			}
		}
	};
}

}());;'use strict';

/* global orb */
/*jshint eqnull: true*/


(function(){


/**
 * Creates a new container for a row/column dimension values.<br/>
 * This object will have all informations related to a dimension: its values, depth, width and subdimensions. 
 * @class
 * @memberOf orb
 * @param  {orb.dimension} parent - parent dimension
 * @param  {array} fields - array describing the fields used for an axe dimenisons
 * @param  {int} fieldindex - index of this dimension field in fields array 
 * @param  {Boolean} isRoot - whether or not this is the root dimension for a given axe (row/column)
 */
orb.dimension = function(id, parent, value, field, depth, isRoot, isLeaf) {
	
	var self = this;

	/**
	 * unique id within parent orb.axe instance.
	 * @type {Number}
	 */
	this.id = id;

	/**
	 * parent subdimension
	 * @type {orb.dimension}
	 */
	this.parent = parent;
	/**
	 * This instance dimension value
	 * @type {object}
	 */
	this.value = value;
	/**
	 * Whether or not this is the root dimension for a given axe (row/column)
	 * @type {Boolean}
	 */
	this.isRoot = isRoot;
	/**
	 * Whether or not this is the leaf (deepest) dimension for a given axe (row/column)
	 * @type {Boolean}
	 */
	this.isLeaf = isLeaf;
	/**
	 * Dimension's data field
	 * @type {Array}
	 */
	this.field = field;
	/**
	 * Dimension's depth (to the deepest sub-dimension)
	 * @type {Number}
	 */
	this.depth = depth;
	/**
	 * Dimension's set of all values
	 * @type {Array}
	 */
	this.values = [];
	/**
	 * Direct descendant subdimensions dictionary
	 * @type {Object}
	 */
	this.subdimvals = {};

	this.rowIndexes = null;

	this.getRowIndexes = function(result) {
		if(self.rowIndexes == null) {
			this.rowIndexes = [];
			for(var i = 0; i < self.values.length; i++) {
				self.subdimvals[self.values[i]].getRowIndexes(this.rowIndexes);
			}
		}
		if(result != null) {
			for(var j = 0; j < self.rowIndexes.length; j++) {
				result.push(self.rowIndexes[j]);
			}
			return result;
		} else {
			return self.rowIndexes;
		}
	}
};

}());;'use strict';

/* global orb */
/*jshint eqnull: true*/

(function(){

/**
 * Creates a new instance of an axe's dimensions list.
 * @class
 * @memberOf orb
 * @param  {array} pgrid - Parent pivot grid
 * @param  {orb.axe.Type} type - Axe type (rows, columns, data)
 */
orb.axe = function(pgrid, type){

	var self = this;
	var dimid = 0;

	if(pgrid != null && pgrid.config != null) {

		/**
		 * Parent pivot grid
		 * @type {orb.pgrid}
		 */
		this.pgrid = pgrid;

		/**
		 * Axe type (rows, columns, data)
		 * @type {orb.axe.Type}
		 */
		this.type = type;

		/**
		 * This axe dimension fields
		 * @type {Array}
		 */
		this.fields = (function() {
			switch(type) {
				case orb.axe.Type.COLUMNS: 
					return self.pgrid.config.columnfields;
				case orb.axe.Type.ROWS: 
					return self.pgrid.config.rowfields;
				case orb.axe.Type.DATA: 
					return self.pgrid.config.datafields;
				default:
					return [];
			}
		}());

		/**
		 * Number of dimensions in this axe
		 * @type {Number}
		 */
		this.dimensionsCount;

		/**
		 * Root dimension
		 * @type {orb.dimension}
		 */
		this.root;

		/** 
		 * Dimensions dictionary indexed by depth
		 * @type {Object} Dictionary of (depth, arrays)
		 */
		this.dimensionsByDepth;

		this.update = function() {
			self.dimensionsCount = self.fields.length;
			self.root = new orb.dimension(++dimid, null, null, null, self.dimensionsCount + 1, true);

			self.dimensionsByDepth = {};
			for(var depth = 1; depth <= self.dimensionsCount; depth++){
				self.dimensionsByDepth[depth] = [];
			}

			// fill data
			fill();	

			// initial sort
			for(var findex = 0; findex < self.fields.length; findex++) {
				var ffield = self.fields[findex];
				if(ffield.sort.order === 'asc' || ffield.sort.order === 'desc') {
					self.sort(ffield, true);
				}
			}
		}

		this.sort = function(field, donottoggle) {
			if(field != null) {
				if(donottoggle !== true) {
					if(field.sort.order !== 'asc') {
						field.sort.order = 'asc';
					} else {
						field.sort.order = 'desc';
					}
				}

				var depth = self.dimensionsCount - getfieldindex(field);
				var parents = depth === self.dimensionsCount ? [self.root] : self.dimensionsByDepth[depth + 1];
				for(var i = 0; i < parents.length; i++) {
					parents[i].values.sort();
					if(field.sort.order === 'desc') {
						parents[i].values.reverse();
					}
				}
			}
		};

		this.update();
	}

	function getfieldindex(field) {
		for(var i = 0; i < self.fields.length; i++) {
			if(self.fields[i].name === field.name) {
				return i;
			}
		}
		return -1;
	}

	/**
	 * Creates all subdimensions using the supplied data
	 */
	function fill() {

		if(self.pgrid.config.datasource != null && self.dimensionsCount > 0) {

			var datasource = self.pgrid.config.datasource;
			if(datasource != null && orb.utils.isArray(datasource) && datasource.length > 0) {
				for(var rowIndex = 0, dataLength = datasource.length; rowIndex < dataLength; rowIndex++) {
					var row = datasource[rowIndex];
					var dim = self.root;
					for(var findex = 0; findex < self.dimensionsCount; findex++) {
						var depth = self.dimensionsCount - findex;
						var subfield = self.fields[findex];
						var subvalue = row[subfield.name];
						var subdimvals = dim.subdimvals;

						if(subdimvals[subvalue] !== undefined){
							dim = subdimvals[subvalue];
						} else {
							dim.values.push(subvalue);
							dim = new orb.dimension(++dimid, dim, subvalue, subfield, depth, false, findex == self.dimensionsCount - 1);
							subdimvals[subvalue] = dim;
							dim.rowIndexes = [];
							self.dimensionsByDepth[depth].push(dim);
						}

						dim.rowIndexes.push(rowIndex);
					}
				}
			}
		}
	};
};

/**
 * Axe types
 * @readonly
 * @enum {Number}
 */
orb.axe.Type = {
	COLUMNS: 1,
	ROWS: 2,
	DATA: 3
}

}());;'use strict';

/* global orb */
/*jshint eqnull: true*/

(function(){

/**
 * Creates a new instance of pgrid
 * @class
 * @memberOf orb
 * @param  {object} config - configuration object
 */
orb.pgrid = function(config) {

	var defaultfield = { name: '#undefined#' };

	var self = this;
	var _iCache;

	this.config = new orb.config(config);

	this.rows = new orb.axe(self, orb.axe.Type.ROWS);
	this.columns = new orb.axe(self, orb.axe.Type.COLUMNS);
	this.dataMatrix = {};

	this.moveField = function(fieldname, oldaxetype, newaxetype, position) {
		if(self.config.moveField(fieldname, oldaxetype, newaxetype, position)) {			
			self.rows.update();
			self.columns.update();
			buildData();		
		}
	};

	this.getData = function(datafield, rowdim, coldim) {

		if(rowdim && coldim) {
			datafield = datafield || (self.config.datafields[0] || defaultfield).name;

			if(self.dataMatrix[rowdim.id] && self.dataMatrix[rowdim.id][coldim.id]) {
				return self.dataMatrix[rowdim.id][coldim.id][datafield] || null;
			}
			return null;
		}
	};

	buildData();

	function calcCellData(rowIndexes, colIndexes, origRowIndexes) {

		var res = {};

		if(self.config.datafieldscount > 0) {

			var intersection;
			
			if(rowIndexes == null) {
				intersection = colIndexes;
			} else if(colIndexes == null) {
				intersection = rowIndexes;
			} else {
				intersection = [];
				for(var ri = 0; ri < rowIndexes.length; ri++) {
					var rowindex = rowIndexes[ri];
					if(rowindex >= 0) {
						var colrowindex = colIndexes.indexOf(rowindex);
						if(colrowindex >= 0) {
							rowIndexes[ri] = 0 - (rowindex + 2);
							intersection.push(rowindex);	
						}
					}
				}
			}

			var datasource = self.config.datasource;

			for(var datafieldIndex = 0; datafieldIndex < self.config.datafieldscount; datafieldIndex++) {
				var datafield = self.config.datafields[datafieldIndex] || defaultfield;
		
				if(datafield.aggregatefunc) {
					res[datafield.name] = datafield.aggregatefunc(datafield.name, intersection, datasource, origRowIndexes, colIndexes);
				} else {
					var intersectionIndexes = intersection != null;
					intersection = intersection || datasource;

					if(intersection.length > 0) {
						var sum = 0;
						for(var ii = 0; ii < intersection.length; ii++) {
							var itemi = intersection[ii];					
							if(intersectionIndexes) {
								if(itemi >= 0) {
									sum += datasource[itemi][datafield.name];
								}
							} else {
								sum += itemi[datafield.name];
							}
						}
						res[datafield.name] = sum;
					}
				}
			}
		}

		return res;
	}

	function calcRowData(rowDim) {

		if(rowDim) {			
			var data = {};
			var rid = 'r' + rowDim.id;

			// set cached row indexes for current row dimension
			if(_iCache[rid] === undefined) {
				_iCache[rid] = rowDim.isRoot ? null : (_iCache[rowDim.parent.id] || rowDim.getRowIndexes());	
			}

			// calc grand-total cell
			data[self.columns.root.id] = calcCellData(rowDim.isRoot ? null : _iCache[rid].slice(0), null);

			if(self.columns.dimensionsCount > 0) {
				var p = 0;
				var parents = [self.columns.root];

				while(p < parents.length) {
					var parent = parents[p];
					var rowindexes = rowDim.isRoot ?
					null :
					(parent.isRoot ?
						_iCache[rid].slice(0) :
						_iCache['c' + parent.id].slice(0));

					for(var i = 0; i < parent.values.length; i++) {
						var subdim = parent.subdimvals[parent.values[i]];
						var cid = 'c' + subdim.id;

						// set cached row indexes for this column leaf dimension
						if(_iCache[cid] === undefined) {
							_iCache[cid] = _iCache[cid] || subdim.getRowIndexes().slice(0);	
						}

						data[subdim.id] = calcCellData(rowindexes, _iCache[cid], rowDim.isRoot ? null : rowDim.getRowIndexes());

						if(!subdim.isLeaf) {
							parents.push(subdim);
							if(rowindexes) {
								_iCache[cid] = [];
								for(var ur = 0; ur < rowindexes.length; ur++) {
									var vr = rowindexes[ur];
									if(vr != -1 && vr < 0) {
										_iCache[cid].push(0 - (vr + 2));
										rowindexes[ur] = -1;
									}
								}
							}
						}
					}
					_iCache['c' + parent.id] = undefined;
					p++;
				}
			}			

			return data;
		}
	}

	function buildData() {
		self.dataMatrix = {};
		_iCache = {};

		// calc grand total row
		self.dataMatrix[self.rows.root.id] = calcRowData(self.rows.root);

		if(self.rows.dimensionsCount > 0) {
			var parents = [self.rows.root];
			var p = 0;
			var parent;
			while(p < parents.length) {
				parent = parents[p];
				// calc children rows
				for(var i = 0; i < parent.values.length; i++) {
					var subdim = parent.subdimvals[parent.values[i]];
					// calc child row
					self.dataMatrix[subdim.id] = calcRowData(subdim);
					// if row is not a leaf, add it to parents array to process its children
					if(!subdim.isLeaf) {
						parents.push(subdim);
					}
				}
				// next parent
				p++;
			}
		}
	}
};

}());;'use strict';

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
orb.ui.header = function(axetype, headerType, dim, parent, datafieldscount, subtotalHeader) {

	var self = this;

	var hspan;
	var vspan;
	var value;

    var isRowsAxe = axetype === orb.axe.Type.ROWS;
    headerType = headerType || (dim.depth === 1 ? orb.ui.HeaderType.INNER: orb.ui.HeaderType.WRAPPER);

	switch(headerType) {
		case orb.ui.HeaderType.GRAND_TOTAL:
			value = 'Grand Total';
			hspan = isRowsAxe ? dim.depth - 1 || 1 : datafieldscount;
			vspan = isRowsAxe ? datafieldscount : dim.depth - 1 || 1;
			break;
		case orb.ui.HeaderType.SUB_TOTAL:
			value = 'Total ' + dim.value;
			hspan = isRowsAxe ? dim.depth : datafieldscount;
			vspan = isRowsAxe ? datafieldscount : dim.depth;
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
						tspan += datafieldscount;
					}
				}
			} else {
				return datafieldscount;
			}
			return tspan +  (addone ? 1 : 0);
		}	
		return tspan;
	}
};

orb.ui.dataHeader = function(datafield, parent) {

	cellbase.call(this, {
			axetype:    null, 
			type: orb.ui.HeaderType.DATA_HEADER, 
			template:   'cell-template-dataheader', 
			value:      datafield,
			cssclass:   orb.ui.HeaderType.getHeaderClass(parent.type),
			isvisible:  parent.visible
		}
	);

	this.parent = parent;
};

orb.ui.dataCell = function(pgrid, isvisible, rowinfo, colinfo) {

	var rowdim = rowinfo.type === orb.ui.HeaderType.DATA_HEADER ? rowinfo.parent.dim : rowinfo.dim;
	var coldim = colinfo.type === orb.ui.HeaderType.DATA_HEADER ? colinfo.parent.dim : colinfo.dim;
	var rowtype = rowinfo.type === orb.ui.HeaderType.DATA_HEADER ? rowinfo.parent.type : rowinfo.type;
	var coltype = colinfo.type === orb.ui.HeaderType.DATA_HEADER ? colinfo.parent.type : colinfo.type;
	var datafield = pgrid.config.datafieldscount > 1 ?
	 (pgrid.config.dataheaderslocation === 'rows' ?
	 	rowinfo.value :
	 	colinfo.value) :
	 pgrid.config.datafields[0];


	cellbase.call(this, {
			axetype:    null, 
			type: orb.ui.HeaderType.DATA_VALUE, 
			template:   'cell-template-datavalue', 
			value:      pgrid.getData(datafield.name, rowdim, coldim),
			cssclass:   'cell ' + orb.ui.HeaderType.getCellClass(rowtype, coltype),
			isvisible:  isvisible
		}
	);

	this.datafield = datafield;
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

}());;'use strict';

/* global orb */
/*jshint eqnull: true*/

// Ensure orb.ui namespace is created
orb.utils.ns('orb.ui');

(function(){

/**
 * Creates a new instance of rows ui properties.
 * @class
 * @memberOf orb.ui
 * @param  {orb.axe} rowsAxe - axe containing all rows dimensions.
 */
orb.ui.rows = function(rowsAxe) {

	var self = this;

	/**
	 * Row dimensions axe
	 * @type {orb.axe}
	 */
	this.axe = rowsAxe;	

	/**
	 * Rows render properties
	 * @type {Array}
	 */
	this.uiInfos = [];

	var _multidatafields;
	var _datafieldscount;

	this.build = function () {

		_datafieldscount = self.axe.pgrid.config.dataheaderslocation === 'rows' ? (self.axe.pgrid.config.datafieldscount || 1) : 1;
		_multidatafields = self.axe.pgrid.config.dataheaderslocation === 'rows' &&  _datafieldscount > 1;

		var uiInfos = [[]];
		if(self.axe != null) {
			// Fill Rows layout infos
			getUiInfo(uiInfos, self.axe.root);

			if(self.axe.pgrid.config.grandtotal.rowsvisible) {
				var lastrow = uiInfos[uiInfos.length - 1];
				var grandtotalHeader = new orb.ui.header(orb.axe.Type.ROWS, orb.ui.HeaderType.GRAND_TOTAL, self.axe.root, null, _datafieldscount);
				if(lastrow.length === 0) {
					lastrow.push(grandtotalHeader);	
				} else {
					uiInfos.push([grandtotalHeader]);
				}

				// add grand-total data headers if more than 1 data field and they will be the leaf headers
				addDataHeaders(uiInfos, grandtotalHeader);
			}

			if(uiInfos[0].length === 0) {
				uiInfos[0].push(new orb.ui.header(orb.axe.Type.ROWS, orb.ui.HeaderType.INNER, self.axe.root, null, _datafieldscount));
			}
			
		}
		self.uiInfos = uiInfos;
	}

	this.build();

	function addDataHeaders(infos, parent) {
		if(_multidatafields) {
			var lastInfosArray = infos[infos.length - 1];
			for(var datafieldindex = 0; datafieldindex < _datafieldscount; datafieldindex++) {
				lastInfosArray.push(new orb.ui.dataHeader(self.axe.pgrid.config.datafields[datafieldindex], parent));
				if(datafieldindex < _datafieldscount - 1) {
					infos.push((lastInfosArray = []));
				}
			}
		}
	}

	/**
	 * Fills the infos array given in argument with the dimension layout infos as row.
	 * @param  {orb.dimension}  dimension - the dimension to get ui info for
	 * @param  {object}  infos - array to fill with ui dimension info
	 */
	function getUiInfo(infos, dimension, totalheader) {
		if(dimension.values.length > 0) {

			var infosMaxIndex = infos.length - 1;
			var lastInfosArray = infos[infosMaxIndex];
			var parent = lastInfosArray.length > 0 ? lastInfosArray[lastInfosArray.length - 1] : null;

			for(var valIndex = 0; valIndex < dimension.values.length; valIndex++) {
				var subvalue = dimension.values[valIndex];
				var subdim = dimension.subdimvals[subvalue];

				var subTotalHeader;
				if(!subdim.isLeaf && subdim.field.subtotal.visible) {
					subTotalHeader = new orb.ui.header(orb.axe.Type.ROWS, orb.ui.HeaderType.SUB_TOTAL, subdim, parent, _datafieldscount);
				} else {
					subTotalHeader = null;
				}
				
				var newHeader = new orb.ui.header(orb.axe.Type.ROWS, null, subdim, parent, _datafieldscount, subTotalHeader);

				if(valIndex > 0) {
					infos.push((lastInfosArray = []));
				}

				lastInfosArray.push(newHeader);

				if(!subdim.isLeaf) {
					getUiInfo(infos, subdim, subTotalHeader);
					if(subdim.field.subtotal.visible) {
						infos.push([subTotalHeader]);

						// add sub-total data headers if more than 1 data field and they will be the leaf headers
						addDataHeaders(infos, subTotalHeader);
					}
				} else {
					// add data headers if more than 1 data field and they will be the leaf headers
					addDataHeaders(infos, newHeader);
				}
			}
		}
	}
};

}());;'use strict';

/* global orb */
/*jshint eqnull: true*/

// Ensure orb.ui namespace is created
orb.utils.ns('orb.ui');

(function(){

/**
 * Creates a new instance of columns ui properties.
 * @class
 * @memberOf orb.ui
 * @param  {orb.axe} columnsAxe - axe containing all columns dimensions.
 */
orb.ui.cols = function(columnsAxe) {

	var self = this;

	/**
	 * Column dimensions axe
	 * @type {orb.axe}
	 */
	this.axe = columnsAxe;	

	/**
	 * Columns render properties
	 * @type {Array}
	 */
	this.uiInfos = null;

	this.leafsHeaders = null;

	var _multidatafields;
	var _datafieldscount;

	this.build = function () {

		_datafieldscount = self.axe.pgrid.config.dataheaderslocation === 'columns' ? self.axe.pgrid.config.datafieldscount : 1;
		_multidatafields = self.axe.pgrid.config.dataheaderslocation === 'columns' &&  _datafieldscount > 1;

		self.uiInfos = [];

		if(self.axe != null) {
			// Fill columns layout infos
			for(var depth = self.axe.root.depth; depth > 1; depth--) {
				self.uiInfos.push([]);
				getUiInfo(depth, self.uiInfos);
			}

			if(self.axe.pgrid.config.grandtotal.columnsvisible) {
				// add grandtotal header
				(self.uiInfos[0] = self.uiInfos[0] || []).push(new orb.ui.header(orb.axe.Type.COLUMNS, orb.ui.HeaderType.GRAND_TOTAL, self.axe.root, null, _datafieldscount));
			}

			if(self.uiInfos.length === 0) {
				self.uiInfos.push([new orb.ui.header(orb.axe.Type.COLUMNS, orb.ui.HeaderType.INNER, self.axe.root, null, _datafieldscount)]);
			}
			
			// generate leafs headers
			generateLeafsHeaders();
		}
		console.log('fff');
	}

	function generateLeafsHeaders() {

		var leafsHeaders = [];

		if(self.uiInfos.length > 0) {
			// last headers row
			var infos = self.uiInfos[self.uiInfos.length - 1];
			var header = infos[0];

			function pushsubtotal(pheader) {
				if(pheader && pheader.dim.field.subtotal.visible) {
					leafsHeaders.push(pheader.subtotalHeader);
				}
			}

			var currparent,
			    prevpar = header.parent;

			for(var i = 0; i < infos.length; i++) {
				header = infos[i];
				currparent = header.parent;
				// if current header parent is different than previous header parent,
				// add previous parent
				if(currparent != prevpar) {
					pushsubtotal(prevpar);
					if(currparent != null) {
						// walk up parent hierarchy and add grand parents if different 
						// than current header grand parents
						var grandpar = currparent.parent;
						var prevgrandpar = prevpar ? prevpar.parent : null;
						while(grandpar != prevgrandpar && prevgrandpar != null) {
							pushsubtotal(prevgrandpar);
							grandpar = grandpar ? grandpar.parent : null;
							prevgrandpar = prevgrandpar ? prevgrandpar.parent : null;
						}
					}
					// update previous parent variable
					prevpar = currparent;
				}
				// push current header
				leafsHeaders.push(infos[i]);

				// if it's the last header, add all of its parents up to the top
				if(i === infos.length - 1) {
					while(prevpar != null) {
						pushsubtotal(prevpar);
						prevpar = prevpar.parent;
					}
				}
			}
			// grandtotal is visible for columns and if there is more than one dimension in this axe
			if(self.axe.pgrid.config.grandtotal.columnsvisible && self.axe.dimensionsCount > 1) {
				// push also grand total header
				leafsHeaders.push(self.uiInfos[0][self.uiInfos[0].length - 1]);
			}
		}

		// add data headers if more than 1 data field and they willbe the leaf headers
		if(_multidatafields) {
			self.leafsHeaders = [];
			for(var leafIndex = 0; leafIndex < leafsHeaders.length; leafIndex++) {
				for(var datafieldindex = 0; datafieldindex < _datafieldscount; datafieldindex++) {
					self.leafsHeaders.push(new orb.ui.dataHeader(self.axe.pgrid.config.datafields[datafieldindex], leafsHeaders[leafIndex]));
				}
			}
			self.uiInfos.push(self.leafsHeaders);
		} else {
			self.leafsHeaders = leafsHeaders;
		}
	}

	this.build();

	/**
	 * Calculates the width of a given column header.<br/>
	 * Column's width represents the number of cells it should span to wrap all sub-dimensions to the deepest.
	 * @param  {orb.dimension} dimension - the column header dimension object
	 * @return {Number}
	 */
	function calcWidth(dimension) {
		var width = 0;

		if(!dimension.isLeaf) {
			// subdimvals 'own' properties are the set of values for this dimension
			for(var i = 0; i < dimension.values.length; i++) {
				var subdim = dimension.subdimvals[dimension.values[i]];
				// if its not an array
				if(!subdim.isLeaf) {
					// call its extractValues (recursive)
					width += calcWidth(subdim) + 1;
				} else {
					width += 1;
				}
			}
			return width;
		} else {
			return 1;
		}
	}

	/**
	 * Fills the infos array given in argument with the dimension layout infos as column.
 	 * @param  {orb.dimension}  dimension - the dimension to get ui info for
	 * @param  {int}  depth - the depth of the dimension that it's subdimensions will be returned
	 * @param  {object}  infos - array to fill with ui dimension info
	 */
	function getUiInfo(depth, uiInfos){

		var infos = uiInfos[uiInfos.length - 1];
		var parents = self.axe.root.depth === depth
			? [null]
			: uiInfos[self.axe.root.depth - depth - 1].filter(function(p) {
				return p.type !== orb.ui.HeaderType.SUB_TOTAL;
			});

		for(var pi = 0; pi < parents.length; pi++) {
			
			var parent = parents[pi];
			var parentDim = parent == null ? self.axe.root : parent.dim;

			for(var di = 0; di < parentDim.values.length; di++) {
			
				var subvalue = parentDim.values[di];
				var subdim = parentDim.subdimvals[subvalue];

				var subtotalHeader;
				if(!subdim.isLeaf && subdim.field.subtotal.visible) {
					subtotalHeader = new orb.ui.header(orb.axe.Type.COLUMNS, orb.ui.HeaderType.SUB_TOTAL, subdim, parent, _datafieldscount);
				} else {
					subtotalHeader = null;
				}

				var header = new orb.ui.header(orb.axe.Type.COLUMNS, null, subdim, parent, _datafieldscount, subtotalHeader);
				infos.push(header);

				if(!subdim.isLeaf && subdim.field.subtotal.visible) {
					infos.push(subtotalHeader);
				}
			}
		}
	}
};

}());;'use strict';

/* global orb */
/*jshint eqnull: true*/

// Ensure orb.ui namespace is created
orb.utils.ns('orb.ui');

(function(){

/**
 * Creates a new instance of pivot grid control
 * @class
 * @memberOf orb.ui
 * @param  {object} pgrid - pivot grid instance
 */
orb.ui.pgridwidget = function(config) {

	var self = this;

	/**
	 * Parent pivot grid
	 * @type {orb.pgrid}
	 */
	this.pgrid = new orb.pgrid(config);

	/**
	 * Control rows headers
	 * @type {orb.ui.rows}
	 */
	this.rows = null;
	/**
	 * Control columns headers
	 * @type {orb.ui.cols}
	 */
	this.columns = null;

	/**
	 * Total number of horizontal row headers.
	 * @type {Array<Array>}
	 */
	this.rowHeadersWidth = null;

	/**
	 * Total number of horizontal column headers.
	 * @type {Array<Array>}
	 */
	this.columnHeadersWidth = null;

	/**
	 * Total number of vertical row headers.
	 * @type {Array<Array>}
	 */
	this.rowHeadersHeight = null;

	/**
	 * Total number of horizontal column headers.
	 * @type {Array<Array>}
	 */
	this.columnHeadersHeight = null;

	/**
	 * Total number of horizontal cells of self pivot grid control.
	 * @type {Array<Array>}
	 */
	this.totalWidth = null;

	/**
	 * Total number of vertical cells of this pivot grid control.
	 * @type {Array<Array>}
	 */
	this.totalWidth = null;

	this.sort = function(axetype, field) {
		if(axetype === orb.axe.Type.ROWS) {
			self.pgrid.rows.sort(field);
		} else if(axetype === orb.axe.Type.COLUMNS) {
			self.pgrid.columns.sort(field);
		} else {
			return;
		}

		buildUi();
	}

	this.moveField = function(field, oldAxeType, newAxeType, position) {
		self.pgrid.moveField(field, oldAxeType, newAxeType, position);
		buildUi();
	}

	this.filters = null;

	this.cells = [];

	this.render = function(element) {
		var pivotTableFactory = React.createFactory(orb.react.PivotTable);
		var pivottable = pivotTableFactory({
			data: self,
			config: config
		});

		React.render(pivottable, element);
	}

	buildUi();

	function buildUi() {

		// build rows and columns
		self.rows = new orb.ui.rows(self.pgrid.rows);
		self.columns = new orb.ui.cols(self.pgrid.columns);

		var rowsInfos = self.rows.uiInfos;
		var rowsInfoslength = rowsInfos.length;

		var columnsInfos = self.columns.uiInfos;
		var columnsInfoslength = columnsInfos.length;

		var columnsAllHeaders = self.columns.leafsHeaders;
		var columnsAllHeaderslength = columnsAllHeaders.length;

		// set control properties		
		self.rowHeadersWidth = (self.pgrid.rows.fields.length || 1) + (self.pgrid.config.dataheaderslocation === 'rows' && self.pgrid.config.datafieldscount > 1 ? 1 : 0);;
		self.columnHeadersWidth = columnsAllHeaderslength;
		self.rowHeadersHeight = rowsInfoslength;
		self.columnHeadersHeight = (self.pgrid.columns.fields.length || 1) + (self.pgrid.config.dataheaderslocation === 'columns' && self.pgrid.config.datafieldscount > 1 ? 1 : 0);
		self.totalWidth = self.rowHeadersWidth + self.columnHeadersWidth;
		self.totalHeight = self.rowHeadersHeight + self.columnHeadersHeight;

		var cells = [];
		var cellsLengthChanged = setArrayLength(cells, columnsInfoslength + rowsInfoslength);

		function setArrayLength(arr, length) {
			if(arr.length !== length) {
				arr.length = length;
				return true;
			}
			return false;
		}
		

		for(var ci = 0; ci < columnsInfoslength; ci++) {

			var uiinfo = columnsInfos[ci];
			var arr = (cells[ci] = cells[ci] || []);
			var prelength = 0;
			if(columnsInfoslength > 1 && ci === 0){
				prelength = 1;
				setArrayLength(arr, prelength + uiinfo.length);
				arr[0] = new orb.ui.emptyCell(self.rowHeadersWidth, self.columnHeadersHeight - 1);
			} else if(ci === columnsInfoslength - 1) {
				prelength = self.rowHeadersWidth;
				setArrayLength(arr, prelength + uiinfo.length);
				if(self.pgrid.rows.fields.length > 0) {
					for(var findex = 0; findex < self.pgrid.config.rowfields.length; findex++) {
						arr[findex] = new orb.ui.buttonCell(self.pgrid.config.rowfields[findex]);
					}
				} else {
					arr[0] = new orb.ui.emptyCell(self.rowHeadersWidth, 1);
				}
			}
			
			for(var ui = 0; ui < uiinfo.length; ui++) {
				arr[prelength + ui] = uiinfo[ui];
			}
		}

  		
		for(var ri = 0; ri < rowsInfoslength; ri++) {
			var ruiinfo = rowsInfos[ri];

			arr = (cells[columnsInfoslength + ri] = cells[columnsInfoslength + ri] || new Array(ruiinfo.length + columnsAllHeaderslength));
			setArrayLength(arr, ruiinfo.length + columnsAllHeaderslength);
			
			for(var uri = 0; uri < ruiinfo.length; uri++) {
				arr[uri] = ruiinfo[uri];
			}
			
			var rinfo = ruiinfo[ruiinfo.length - 1];
			for(var cinfosIndex = 0; cinfosIndex < columnsAllHeaderslength; cinfosIndex++) {
				var cinfo = columnsAllHeaders[cinfosIndex];
				var isvisible = (function(rowvisible, colvisible) {
					return function() {
						return rowvisible() && colvisible();
					}
				}(rinfo.visible, cinfo.visible));
				arr[ruiinfo.length + cinfosIndex] = new orb.ui.dataCell(self.pgrid, isvisible, rinfo, cinfo);
			}
		}
		self.cells = cells;
	}
};

orb.ui.HeaderType = {
    EMPTY: 1,
    DATA_HEADER: 2,
    DATA_VALUE: 3,
    FIELD_BUTTON: 4,
	INNER: 5,
	WRAPPER: 6,
	SUB_TOTAL: 7,
	GRAND_TOTAL: 8,
	getHeaderClass: function(headerType, axetype) {
		var cssclass = '';
		switch(headerType) {
			case orb.ui.HeaderType.EMPTY:
			case orb.ui.HeaderType.FIELD_BUTTON:
				cssclass = 'empty';
				break;
			case orb.ui.HeaderType.INNER:
				cssclass = 'header';
				break;
			case orb.ui.HeaderType.WRAPPER:
				if(axetype === orb.axe.Type.ROWS) {
					cssclass = 'header';
				} else if(axetype === orb.axe.Type.COLUMNS) {
					cssclass = 'header';
				}
				break;
			case orb.ui.HeaderType.SUB_TOTAL:
				cssclass = 'header header-sub-total';
				break;
			case orb.ui.HeaderType.GRAND_TOTAL:
				cssclass = 'header header-grand-total';
				break;
		}

		return cssclass;
	},
	getCellClass: function(rowHeaderType, colHeaderType) {
		var cssclass = '';
		switch(rowHeaderType) {
			case orb.ui.HeaderType.GRAND_TOTAL: 
				cssclass = 'cell-grand-total';
				break;
			case orb.ui.HeaderType.SUB_TOTAL: 
				if(colHeaderType === orb.ui.HeaderType.GRAND_TOTAL) {
					cssclass = 'cell-grand-total';
				} else {
					cssclass = 'cell-sub-total';
				}				
				break;
			default:
				if(colHeaderType === orb.ui.HeaderType.GRAND_TOTAL) {
					cssclass = 'cell-grand-total';
				} else if(colHeaderType === orb.ui.HeaderType.SUB_TOTAL) {
					cssclass = 'cell-sub-total';
				} else {
					cssclass = 'cell';
				}
		}
		return cssclass;
	}
};

}());;// Ensure orb.react namespace is created
orb.utils.ns('orb.react');

(function() {

var extraCol = 1;

orb.react.PivotTable = React.createClass({displayName: 'PivotTable',
  getInitialState: function() {
    orb.react.DragManager.init(this);
    return {};
  },
  sort: function(axetype, field) {
    this.props.data.sort(axetype, field);
    this.setProps(this.props);
  },
  moveButton: function(button, newAxeType, position) {
    this.props.data.moveField(button.props.field.name, button.props.axetype, newAxeType, position);
    this.setProps(this.props);
  },
  expandRow: function(cell) {
    cell.expanded = true;
    this.setProps({});
  },
  collapseRow: function(cell) {
    cell.subtotalHeader.expanded = false;
    this.setProps({});
  },
  render: function() {

    var self = this;

    var ptc = this.props.data;
    var PivotButton = orb.react.PivotButton;
    var PivotRow = orb.react.PivotRow;
    var DropTarget = orb.react.DropTarget;

    var fieldButtons = ptc.pgrid.config.availablefields().map(function(field, index) {
      return React.createElement(PivotButton, {key: field.name, 
                          field: field, 
                          axetype: null, 
                          position: index, 
                          rootComp: self}
             );
    });

    var dataButtons = ptc.pgrid.config.datafields.map(function(field, index) {
      return React.createElement(PivotButton, {key: field.name, 
                          field: field, 
                          axetype: orb.axe.Type.DATA, 
                          position: index, 
                          rootComp: self}
             );
    });

    var columnButtons = ptc.pgrid.config.columnfields.map(function(field, index) {
      return React.createElement(PivotButton, {key: field.name, 
                          field: field, 
                          axetype: orb.axe.Type.COLUMNS, 
                          position: index, 
                          rootComp: self}
             );
    });

    // get 'row buttons' row (also last row containing column headers)
    var rowButtons = orb.utils.findInArray(ptc.cells, function(row) {
      return row[0].template === 'cell-template-fieldbutton';
    });

    // build row buttons
    if(rowButtons !== undefined) {
      rowButtons = rowButtons.filter(function(buttonCell) {
        return buttonCell.template === 'cell-template-fieldbutton';
      }).map(function(buttonCell, index) {
          return React.createElement(PivotButton, {key: buttonCell.value.name, 
                              field: buttonCell.value, 
                              axetype: orb.axe.Type.ROWS, 
                              position: index, 
                              rootComp: self}
                 );
      });
    } else {
      rowButtons = [];
    }

    // build the cell that will contains 'row buttons'
    var rowButtonsCell = React.createElement("td", {className: "empty", colSpan: ptc.rowHeadersWidth + extraCol, rowSpan: "1"}, 
                          React.createElement(DropTarget, {data: rowButtons, axetype: orb.axe.Type.ROWS}
                          )
                         );

    var rows = ptc.cells.map(function(row, index) {
      if(index == ptc.columnHeadersHeight - 1) {
        return React.createElement(PivotRow, {key: index, 
                         row: row, 
                         rowButtonsCount: ptc.rowHeadersWidth, 
                         rowButtonsCell: rowButtonsCell, 
                         rootComp: self}
               );
      } else {
        return React.createElement(PivotRow, {key: index, 
                         row: row, 
                         rootComp: self}
               );
      };
    });

    var tblStyle = this.props.config.width ?  {width: this.props.config.width} : {};

    return (
    React.createElement("div", {className: "orb-container", style: tblStyle}, 
      React.createElement("table", {id: "tbl", className: "orb", style: {width: '100%'}}, 
        React.createElement("tbody", null, 
          React.createElement("tr", null, 
            React.createElement("td", {className: "available-fields field-group", colSpan: extraCol, rowSpan: "1"}, 
              React.createElement("div", {className: "field-group-caption"}, "Fields:")
            ), 
            React.createElement("td", {className: "available-fields", colSpan: ptc.totalWidth, rowSpan: "1"}, 
              React.createElement(DropTarget, {data: fieldButtons, axetype: null}
              )
            )
          ), 
          React.createElement("tr", null, 
            React.createElement("td", {className: "field-group", colSpan: extraCol, rowSpan: "1"}, 
              React.createElement("div", {className: "field-group-caption"}, "Data fields:")
            ), 
            React.createElement("td", {className: "empty", colSpan: ptc.totalWidth, rowSpan: "1"}, 
              React.createElement(DropTarget, {data: dataButtons, axetype: orb.axe.Type.DATA}
              )
            )
          ), 
          React.createElement("tr", null, 
            React.createElement("td", {className: "empty", colSpan: ptc.rowHeadersWidth + extraCol, rowSpan: "1"}), 
            React.createElement("td", {className: "empty", colSpan: ptc.columnHeadersWidth, rowSpan: "1"}, 
              React.createElement(DropTarget, {data: columnButtons, axetype: orb.axe.Type.COLUMNS}
              )
            )
          ), 
          rows
        )
      )
    )
    );
  }
});

orb.react.PivotRow = React.createClass({displayName: 'PivotRow',
  render: function() {
    var self = this;
    var PivotCell = orb.react.PivotCell;
    
    var lastCellIndex = this.props.row.length - 1;
    var cell0 = this.props.row[0];
    var cells;

    var rowstyle = {};

    if(this.props.rowButtonsCell !== undefined) {
      cells = this.props.row.slice(this.props.rowButtonsCount).map(function(cell, index) {
        var isrightmost = index === (lastCellIndex - self.props.rowButtonsCount);
        return React.createElement(PivotCell, {key: index, 
                          cell: cell, 
                          rightmost: isrightmost, 
                          leftmost: false, 
                          rootComp: self.props.rootComp}
               );
      });

      return (
        React.createElement("tr", null, 
          this.props.rowButtonsCell, 
          cells
        )
      );

    } else {

      if(cell0.template == 'cell-template-row-header' && cell0.visible && !cell0.visible()) {
        rowstyle.display = 'none';
      }

      cells = this.props.row.map(function(cell, index) {
        var isrightmost = index === lastCellIndex;
        var isleftmost = index === 0 && (
                           cell.type === orb.ui.HeaderType.EMPTY ||
                           cell.type === orb.ui.HeaderType.SUB_TOTAL || 
                           cell.type === orb.ui.HeaderType.GRAND_TOTAL || 
                           (cell.dim && (cell.dim.isRoot || cell.dim.parent.isRoot))
                         );

        return React.createElement(PivotCell, {key: index, 
                          cell: cell, 
                          rightmost: isrightmost, 
                          leftmost: isleftmost, 
                          rootComp: self.props.rootComp}
               );
      });

      return (
        React.createElement("tr", {style: rowstyle}, 
          cells
        )
      );
    }
  }
});

orb.react.PivotCell = React.createClass({displayName: 'PivotCell',
  expand: function() {
    this.props.rootComp.expandRow(this.props.cell);
  },
  collapse: function() {
    this.props.rootComp.collapseRow(this.props.cell);
  },
  render: function() {
    var cell = this.props.cell;
    var divcontent = [];
    var value;
    var vArrow = '\u25bc';
    var hArrow = '\u25b6';

    switch(cell.template) {
      case 'cell-template-row-header':
      case 'cell-template-column-header':
        if(cell.type === orb.ui.HeaderType.WRAPPER && cell.dim.field.subtotal.visible && cell.dim.field.subtotal.collapsible && cell.subtotalHeader.expanded) {
          divcontent.push(React.createElement("span", {key: "toggle-button", className: "toggle-button", onClick: this.collapse}, vArrow));
        } else if(cell.type === orb.ui.HeaderType.SUB_TOTAL && !cell.expanded){
          divcontent.push(React.createElement("span", {key: "toggle-button", className: "toggle-button", onClick: this.expand}, hArrow));
        }
        value = cell.value;
        break;
      case 'cell-template-dataheader':
        value = cell.value.caption;
        break;
      case 'cell-template-datavalue':
        value = cell.datafield && cell.datafield.formatfunc ? cell.datafield.formatfunc(cell.value) : cell.value;
        break;
      default:
        break;
    }

    divcontent.push(React.createElement("span", {key: "cell-value", style: {whiteSpace: 'nowrap'}}, value));

    var classname = cell.cssclass;
    var isHidden = !cell.visible();
    if(isHidden || this.props.rightmost || this.props.leftmost) {
      
      if(isHidden) {
        classname += ' cell-hidden';
      }

      if(this.props.rightmost && (cell.axetype !== orb.axe.Type.COLUMNS || cell.type === orb.ui.HeaderType.GRAND_TOTAL)) {
        classname += ' cell-rightmost';
      }

      if(this.props.leftmost) {
        classname += ' cell-leftmost';
        console.log('cell-leftmost: ' + cell.value);
      }
    }

    return React.createElement("td", {className: classname, 
               colSpan: cell.hspan() + (this.props.leftmost ? extraCol : 0), 
               rowSpan: cell.vspan()}, 
                React.createElement("div", null, 
                  divcontent
                )
           );
  }
});

})();
/** @jsx React.DOM */

// Ensure orb.react namespace is created
orb.utils.ns('orb.react');

(function() {

function forEach(list, func, defStop) {
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

orb.react.DragManager = (function() {
	
	var _pivotComp = null;
	var _dragElement = null;
	var _dragNode = null;
	var _dropTargets = [];
	var _dropIndicators = [];

	function doElementsOverlap(elem1Rect, elem2Rect) {
		return !(elem1Rect.right < elem2Rect.left || 
                elem1Rect.left > elem2Rect.right || 
                elem1Rect.bottom < elem2Rect.top || 
                elem1Rect.top > elem2Rect.bottom);
	}

	function signalDragOver(target) {
		if(target.onDragOver) {
			target.onDragOver(_dragElement);
			return true;
		}
		return false;
	}

	function signalDragEnd(target) {
		if(target.onDragEnd) {
			target.onDragEnd();
			return true;
		}
		return false;
	}

	function getDropTarget() {
		return forEach(_dropTargets, function(target) {
			if(target.component.state.isover) {
				return target;
			}
		}, true);
	}

	function getDropIndicator() {
		return forEach(_dropIndicators, function(indicator) {
			if(indicator.component.state.isover) {
				return indicator;
			}
		}, true);
	}

	var _initialized = false;

	return {
		init: function(pivotComp) {
			_initialized = true;
			_pivotComp = pivotComp;
		},
		dragElement: function(elem) {
			
			var prevDragElement = _dragElement;
			_dragElement = elem;
			if(_dragElement != prevDragElement) {
				if(elem == null) {

					// Drop Target
					var dropTarget = getDropTarget();
					// Drop Indicator
					var dropIndicator = getDropIndicator();

					if(dropTarget) {
						var position = dropIndicator != null ? dropIndicator.position : null;
						_pivotComp.moveButton(prevDragElement, dropTarget.component.props.axetype, position);
					}

					_dragNode = null;
					forEach(_dropTargets, function(target) {
						signalDragEnd(target);
					});

					forEach(_dropIndicators, function(indicator) {
						signalDragEnd(indicator);
					});

				} else {
					_dragNode = _dragElement.getDOMNode();
				}
			}
		},
		registerTarget: function(target, axetype, dragOverHandler, dargEndHandler) {
			_dropTargets.push({
				component: target,
				axetype: axetype,
				onDragOver: dragOverHandler,
				onDragEnd: dargEndHandler
			});
		},
		unregisterTarget: function(target) {
			var tindex;
			for(var i = 0; i < _dropTargets.length; i++) {
				if(_dropTargets[i].component == target) {
					tindex = i;
					break;
				}
			}
			if(tindex != null) {
				_dropTargets.splice(tindex, 1);
			};
		},
		registerIndicator: function(indicator, axetype, position, dragOverHandler, dargEndHandler) {
			_dropIndicators.push({
				component: indicator,
				axetype: axetype,
				position: position,
				onDragOver: dragOverHandler,
				onDragEnd: dargEndHandler
			});
		},
		unregisterIndicator: function(indicator) {
			var iindex;
			for(var i = 0; i < _dropIndicators.length; i++) {
				if(_dropIndicators[i].component == indicator) {
					iindex = i;
					break;
				}
			}
			if(iindex != null) {
				_dropIndicators.splice(iindex, 1);
			};
		},
		elementMoved: function() {
			if(_dragElement != null) {
				var dragNodeRect = _dragNode.getBoundingClientRect();
				var foundTarget;

				forEach(_dropTargets, function(target) {
					if(!foundTarget) {
						var tnodeRect = target.component.getDOMNode().getBoundingClientRect();
						var isOverlap = doElementsOverlap(dragNodeRect, tnodeRect);
						if(isOverlap && signalDragOver(target)) {
							foundTarget = target;
							return true;
						} else {
							signalDragEnd(target);
						}
					}
				}, true);

				var foundIndicator;

				if(foundTarget) {
					forEach(_dropIndicators, function(indicator, index) {
						if(!foundIndicator) {
							var elementOwnIndicator = indicator.component.props.axetype === _dragElement.props.axetype
												&& indicator.component.props.position === _dragElement.props.position;

							var targetIndicator = indicator.component.props.axetype === foundTarget.component.props.axetype;
							if(targetIndicator && !elementOwnIndicator) {	
								var tnodeRect = indicator.component.getDOMNode().getBoundingClientRect();
								var isOverlap = doElementsOverlap(dragNodeRect, tnodeRect);
								if(isOverlap && signalDragOver(indicator)) {
									foundIndicator = indicator;
									return;
								}
							}
						}

						signalDragEnd(indicator);
					});

					if(!foundIndicator) {
						var axeIndicators = _dropIndicators.filter(function(indicator) {
							return indicator.component.props.axetype === foundTarget.component.props.axetype;
						});
						if(axeIndicators.length > 0) {
							signalDragOver(axeIndicators[axeIndicators.length - 1]);
						}
					}
				} else {
					forEach(_dropIndicators, function(indicator, index) {
						signalDragEnd(indicator);
					});
				}
			}
		}
	};
}());

var dtid = 0;

orb.react.DropTarget = React.createClass({displayName: 'DropTarget',
	getInitialState: function () {
		this.dtid = ++dtid;
		// initial state, all zero.
		orb.react.DragManager.registerTarget(this, this.props.axetype, this.onDragOver, this.onDragEnd);
		return {
			isover: false
		};
	},
	componentWillUnmount : function() {
		orb.react.DragManager.unregisterTarget(this);
	},
	onDragOver: function(component) {
		this.setState({
			isover: true
		})
	},
	onDragEnd: function() {
		this.setState({
			isover: false
		})
	},
	render: function() {	
		var self = this;
		var DropIndicator = orb.react.DropIndicator;
		var buttons = this.props.data.map(function(button, index) {			
			if(index < self.props.data.length - 1) {
				return [
					React.createElement(DropIndicator, {isFirst: index === 0, position: index, axetype: self.props.axetype}),
					button
				];
			} else {
				return [
					React.createElement(DropIndicator, {isFirst: index === 0, position: index, axetype: self.props.axetype}),
					button,
					React.createElement(DropIndicator, {isLast: true, position: null, axetype: self.props.axetype})
				];
			}
		});

		return React.createElement("div", {className: 'drop-target' + (this.state.isover ? ' drag-over' : '')}, 
				buttons
			   );
	}
});

function getOffset(element) {
	if(element != null) {
	    var rect = element.getBoundingClientRect();
	    return { x: rect.left, y: rect.top };
	}
    return { x: 0, y: 0 };
}

function getSize(element) {
	if(element != null) {
	    var rect = element.getBoundingClientRect();
	    return { width: rect.right - rect.left, height: rect.bottom - rect.top};
	}
    return { x: 0, y: 0 };
}

orb.react.DropIndicator = React.createClass({
	displayName: 'DropIndicator',
	getInitialState: function () {
		orb.react.DragManager.registerIndicator(this, this.props.axetype, this.props.position, this.onDragOver, this.onDragEnd);
		return {
			isover: false
		};
	},
	componentWillUnmount : function() {
		orb.react.DragManager.unregisterIndicator(this);
	},
	onDragOver: function(component) {
		this.setState({
			isover: true,
			width: component.getDOMNode().style.width
		})
	},
	onDragEnd: function() {
		this.setState({
			isover: false,
			width: null
		})
	},
	render: function() {
		var classname = 'drop-indicator';

		if(this.props.isFirst) {
			classname += ' drop-indicator-first';
		}

		if(this.props.isLast) {
			classname += ' drop-indicator-last';
		}

		var style = {};
		if(this.state.isover) {
			classname += ' drop-indicator-drag-over';
		}

		return React.createElement("div", {style: style, className: classname});
	}
});

var pbid = 0;

orb.react.PivotButton = React.createClass({
	displayName: 'PivotButton',
	getInitialState: function () {
		this.pbid = ++pbid;

		// initial state, all zero.
		return {
			pos: { x: 0, y: 0 },
			startpos: { x: 0, y: 0 },
			mousedown: false,
			dragging: false
		};
	},
	onMouseDown: function(e) {
		// drag/sort with left mouse button
		if (e.button !== 0) return;

		var thispos = getOffset(this.getDOMNode());
		
		// inform mousedown, save start pos
		this.setState({
			mousedown: true,
			mouseoffset: {
				x: thispos.x - e.pageX,
				y: thispos.y - e.pageY,
			},
			startpos: {
				x: e.pageX,
				y: e.pageY
			}
		});
		// prevent event bubbling (to prevent text selection while dragging for example)
		e.stopPropagation();
		e.preventDefault();
	},
	componentDidUpdate: function () {
		if (!this.state.mousedown) {
			// mouse not down, don't care about mouse up/move events.
			orb.react.DragManager.dragElement(null);
			document.removeEventListener('mousemove', this.onMouseMove)
			document.removeEventListener('mouseup', this.onMouseUp)
		} else if (this.state.mousedown) {
			// mouse down, interested by mouse up/move events.
			orb.react.DragManager.dragElement(this);
			document.addEventListener('mousemove', this.onMouseMove)
			document.addEventListener('mouseup', this.onMouseUp)
		}
	},
	componentWillUnmount : function() {
		document.removeEventListener('mousemove', this.onMouseMove)
		document.removeEventListener('mouseup', this.onMouseUp)
	},
	onMouseUp: function() {
		var wasdragging = this.state.dragging;

		this.setState({
			mousedown: false,
			dragging: false,
			size: null,
			pos: {
				x: 0,
				y: 0
			}
		});

		// if button was not dragged, proceed as a click
		if(!wasdragging) {
			this.props.rootComp.sort(this.props.axetype, this.props.field)
		}
	},
	onMouseMove: function (e) {
		console.log('PivotButton[' + this.pbid + '].onMouseMove');

		// if the mouse is not down while moving, return (no drag)
		if (!this.state.mousedown) return

		var size = null;
		if(!this.state.dragging) {
			size = getSize(this.getDOMNode());
		} else {
			size = this.state.size;
		}

		var newpos = {
			x: e.pageX + this.state.mouseoffset.x,
			y: e.pageY + this.state.mouseoffset.y
		};

		this.setState({
			dragging: true,
			size: size,
			pos: newpos
		});

		orb.react.DragManager.elementMoved();

		e.stopPropagation();
		e.preventDefault();
	},
	render: function() {
		var self = this;
		var divstyle = {
			left: self.state.pos.x + 'px',
			top: self.state.pos.y + 'px',
			position: self.state.dragging ? 'absolute' : ''
		};

		if(self.state.size) {
			divstyle.width = self.state.size.width + 'px';
		}

		var DropIndicator = orb.react.DropIndicator;
		var sortIndicator = self.props.field.sort.order === 'asc' ? 
		' \u25B3' :
		(self.props.field.sort.order === 'desc' ?
			' \u25BD' :
			'' );

		return React.createElement("div", {key: self.props.field.name, 
		            className: "field-button", 
		            onMouseDown: this.onMouseDown, 
		            style: divstyle}, 
		            	self.props.field.caption, 
		            	React.createElement("span", null, sortIndicator)
		        );
	}
});

})();