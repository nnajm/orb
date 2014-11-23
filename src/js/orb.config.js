/**
 * @fileOverview Pivot Grid axe viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

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

}());