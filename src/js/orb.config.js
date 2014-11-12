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
		if(configs[i][property] !== undefined) {
			return configs[i][property];
		}
	}
	return defaultvalue;
}

function mergefieldconfigs() {

	var configs = [];
	var sorts = [];
	var subtotals = [];

	for(var i = 0; i < arguments.length; i++) {
		var nnconfig = arguments[i] || {};
		configs.push(nnconfig);
		sorts.push(nnconfig.sort || {});
		subtotals.push(nnconfig.subtotal || {});
	}
	
	return {
		name: getpropertyvalue('name', configs, ''),
		caption: getpropertyvalue('caption', configs, ''),
		aggregatefunc: getpropertyvalue('aggregatefunc', configs, null),
		filter: getpropertyvalue('filter', configs, null),
		sort: {
			order: getpropertyvalue('order', sorts, null),
			customfunc: getpropertyvalue('customfunc', sorts, null)
		},
		subtotal: { 
			visible: getpropertyvalue('visible', subtotals, true),
			collapsible: getpropertyvalue('collapsible', subtotals, true),
			collapsed: getpropertyvalue('collapsed', subtotals, false)
		}
	}
}

function createfield(axetype, fieldconfig, defaultfieldconfig) {

	var axeconfig;
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

	return mergefieldconfigs(fieldconfig, axeconfig, defaultfieldconfig)
}

orb.field = function(fieldconfig) {
	
	fieldconfig = fieldconfig || {};

	this.name = fieldconfig.name;
	this.caption = fieldconfig.caption;
	this.aggregatefunc = fieldconfig.aggregatefunc;
	this.filter = fieldconfig.filter;

	var fieldsort = fieldconfig.sort || {};

	this.sort = {
		order: fieldsort.order,
		customfunc: fieldsort.customfunc
	};
	
	var fieldsubtotal = fieldconfig.subtotal || {};	

	this.subtotal = { 
		visible: fieldsubtotal.visible !== undefined ? fieldsubtotal.visible : true,
		collapsible: fieldsubtotal.collapsible !== undefined ? fieldsubtotal.collapsible : true,
		collapsed: fieldsubtotal.collapsed !== undefined ? fieldsubtotal.collapsed : false
	}

	this.rowsettings = fieldconfig.rowsettings;
	this.columnsettings = fieldconfig.columnsettings;
	this.datasettings = fieldconfig.datasettings;
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
	this.showgrandtotal = config.showgrandtotal === false ? false: true;
	this.showsubtotals = config.showsubtotals === false ? false: true;

	this.allfields = (config.fields || []).map(function(fieldconfig) {
		return new orb.field(fieldconfig);
    });

   	this.rowfields = (config.rows || []).map(function(fieldconfig) {
		return createfield(orb.axe.Type.ROWS, fieldconfig, getfield(self.allfields, fieldconfig.name));
    });

   	this.columnfields = (config.columns || []).map(function(fieldconfig) {
		return createfield(orb.axe.Type.COLUMNS, fieldconfig, getfield(self.allfields, fieldconfig.name));
    });

   	this.datafields = (config.data || []).map(function(fieldconfig) {
		return createfield(orb.axe.Type.DATA, fieldconfig, getfield(self.allfields, fieldconfig.name));
    });

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

				field = createfield(newaxetype, null, field);

				if(newaxe) {
					if(position != null) {
						newaxe.splice(position, 0, field);
					} else {
						newaxe.push(field);
					}
				}

				return true;
			}
		}
	};
}

}());