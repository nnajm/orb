/**
 * @fileOverview Pivot Grid axe viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

var utils = require('./orb.utils');
var axe = require('./orb.axe');
var aggregation = require('./orb.aggregation');
var filtering = require('./orb.filtering');
var themeManager = require('./orb.themes');

function getpropertyvalue(property, configs, defaultvalue) {
    for (var i = 0; i < configs.length; i++) {
        if (configs[i][property] != null) {
            return configs[i][property];
        }
    }
    return defaultvalue;
}

function mergefieldconfigs() {

    var merged = {
        configs: [],
        sorts: [],
        subtotals: [],
        functions: []
    };

    for (var i = 0; i < arguments.length; i++) {
        var nnconfig = arguments[i] || {};
        merged.configs.push(nnconfig);
        merged.sorts.push(nnconfig.sort || {});
        merged.subtotals.push(nnconfig.subTotal || {});
        merged.functions.push({
            aggregateFuncName: nnconfig.aggregateFuncName,
            aggregateFunc: i === 0 ? nnconfig.aggregateFunc : (nnconfig.aggregateFunc ? nnconfig.aggregateFunc() : null),
            formatFunc: i === 0 ? nnconfig.formatFunc : (nnconfig.formatFunc ? nnconfig.formatFunc() : null),
        });
    }

    return merged;
}

function createfield(rootconfig, axetype, fieldconfig, defaultfieldconfig) {

    var axeconfig;
    var fieldAxeconfig;

    if (defaultfieldconfig) {
        switch (axetype) {
            case axe.Type.ROWS:
                axeconfig = rootconfig.rowSettings;
                fieldAxeconfig = defaultfieldconfig.rowSettings;
                break;
            case axe.Type.COLUMNS:
                axeconfig = rootconfig.columnSettings;
                fieldAxeconfig = defaultfieldconfig.columnSettings;
                break;
            case axe.Type.DATA:
                axeconfig = rootconfig.dataSettings;
                fieldAxeconfig = defaultfieldconfig.dataSettings;
                break;
            default:
                axeconfig = null;
                fieldAxeconfig = null;
                break;
        }
    } else {
        axeconfig = null;
        fieldAxeconfig = null;
    }

    var merged = mergefieldconfigs(fieldconfig, fieldAxeconfig, axeconfig, defaultfieldconfig, rootconfig);

    return new Field({
        name: getpropertyvalue('name', merged.configs, ''),

        caption: getpropertyvalue('caption', merged.configs, ''),

        sort: {
            order: getpropertyvalue('order', merged.sorts, null),
            customfunc: getpropertyvalue('customfunc', merged.sorts, null)
        },
        subTotal: {
            visible: getpropertyvalue('visible', merged.subtotals, true),
            collapsible: getpropertyvalue('collapsible', merged.subtotals, true),
            collapsed: getpropertyvalue('collapsed', merged.subtotals, false) && getpropertyvalue('collapsible', merged.subtotals, true)
        },

        aggregateFuncName: getpropertyvalue('aggregateFuncName', merged.functions, 'sum'),
        aggregateFunc: getpropertyvalue('aggregateFunc', merged.functions, aggregation.sum),
        formatFunc: getpropertyvalue('formatFunc', merged.functions, null)
    }, false);
}

function GrandTotalConfig(options) {

    options = options || {};

    this.rowsvisible = options.rowsvisible !== undefined ? options.rowsvisible : true;
    this.columnsvisible = options.columnsvisible !== undefined ? options.columnsvisible : true;
}

function SubTotalConfig(options, setdefaults) {

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

function SortConfig(options) {
    options = options || {};

    this.order = options.order;
    this.customfunc = options.customfunc;
}

var Field = module.exports.field = function(options, createSubOptions) {

    options = options || {};

    // field name
    this.name = options.name;

    // shared settings
    this.caption = options.caption || this.name;

    // rows & columns settings
    this.sort = new SortConfig(options.sort);
    this.subTotal = new SubTotalConfig(options.subTotal);

    // data settings
    var _aggregatefunc;
    var _formatfunc;

    function defaultFormatFunc(val) {   
        return val != null ? val.toString() : '';
    }

    this.aggregateFunc = function(func) {
        if (func) {
            _aggregatefunc = aggregation.toAggregateFunc(func);
        } else {
            return _aggregatefunc;
        }
    };

    this.formatFunc = function(func) {
        if (func) {
            _formatfunc = func;
        } else {
            return _formatfunc;
        }
    };

    this.aggregateFuncName = options.aggregateFuncName || 
        (options.aggregateFunc ?
            (utils.isString(options.aggregateFunc) ?
                options.aggregateFunc :
                'custom') :
            null);

    this.aggregateFunc(options.aggregateFunc);
    this.formatFunc(options.formatFunc || defaultFormatFunc);

    if (createSubOptions !== false) {
        (this.rowSettings = new Field(options.rowSettings, false)).name = this.name;
        (this.columnSettings = new Field(options.columnSettings, false)).name = this.name;
        (this.dataSettings = new Field(options.dataSettings, false)).name = this.name;
    }
};

/**
 * Creates a new instance of pgrid config
 * @class
 * @memberOf orb
 * @param  {object} config - configuration object
 */
module.exports.config = function(config) {

    var self = this;

    this.dataSource = config.dataSource || [];
    this.canMoveFields = config.canMoveFields !== undefined ? !!config.canMoveFields : true;
    this.dataHeadersLocation = config.dataHeadersLocation === 'columns' ? 'columns' : 'rows';
    this.grandTotal = new GrandTotalConfig(config.grandTotal);
    this.subTotal = new SubTotalConfig(config.subTotal, true);
    this.width = config.width;
    this.height = config.height;
    this.toolbar = config.toolbar;
    this.theme = themeManager;

    themeManager.current(config.theme);

    this.rowSettings = new Field(config.rowSettings, false);
    this.columnSettings = new Field(config.columnSettings, false);
    this.dataSettings = new Field(config.dataSettings, false);

    // datasource field names
    this.dataSourceFieldNames = [];
    // datasource field captions
    this.dataSourceFieldCaptions = [];

    this.captionToName = function(caption) {
        var fcaptionIndex = self.dataSourceFieldCaptions.indexOf(caption);        
        return fcaptionIndex >= 0 ? self.dataSourceFieldNames[fcaptionIndex] : caption;
    };

    this.nameToCaption = function(name) {
        var fnameIndex = self.dataSourceFieldNames.indexOf(name);
        return fnameIndex >= 0 ? self.dataSourceFieldCaptions[fnameIndex] : name;
    };

    this.setTheme = function(newTheme) {
        return self.theme.current() !== self.theme.current(newTheme);
    };

    this.allFields = (config.fields || []).map(function(fieldconfig) {
        var f = new Field(fieldconfig);
        // map fields names to captions
        self.dataSourceFieldNames.push(f.name);
        self.dataSourceFieldCaptions.push(f.caption);
        return f;
    });

    function ensureFieldConfig(obj) {
        if(typeof obj === 'string') {
            return {
                name: self.captionToName(obj)
            };
        }
        return obj;
    }

    this.rowFields = (config.rows || []).map(function(fieldconfig) {
        fieldconfig = ensureFieldConfig(fieldconfig);
        return createfield(self, axe.Type.ROWS, fieldconfig, getfield(self.allFields, fieldconfig.name));
    });

    this.columnFields = (config.columns || []).map(function(fieldconfig) {
        fieldconfig = ensureFieldConfig(fieldconfig);
        return createfield(self, axe.Type.COLUMNS, fieldconfig, getfield(self.allFields, fieldconfig.name));
    });

    this.dataFields = (config.data || []).map(function(fieldconfig) {
        fieldconfig = ensureFieldConfig(fieldconfig);
        return createfield(self, axe.Type.DATA, fieldconfig, getfield(self.allFields, fieldconfig.name));
    });

    this.dataFieldsCount = this.dataFields ? (this.dataFields.length || 1) : 1;

    var runtimeVisibility = {
        subtotals: {
            rows: self.rowSettings.subTotal.visible !== undefined ? self.rowSettings.subTotal.visible : true,
            columns: self.columnSettings.subTotal.visible !== undefined ? self.columnSettings.subTotal.visible : true
        }
    };

    function getfield(axefields, fieldname) {
        var fieldindex = getfieldindex(axefields, fieldname);
        if (fieldindex > -1) {
            return axefields[fieldindex];
        }
        return null;
    }

    function getfieldindex(axefields, fieldname) {
        for (var fi = 0; fi < axefields.length; fi++) {
            if (axefields[fi].name === fieldname) {
                return fi;
            }
        }
        return -1;
    }

    this.getField = function(fieldname) {
        return getfield(self.allFields, fieldname);
    };

    this.getRowField = function(fieldname) {
        return getfield(self.rowFields, fieldname);
    };

    this.getColumnField = function(fieldname) {
        return getfield(self.columnFields, fieldname);
    };

    this.getDataField = function(fieldname) {
        return getfield(self.dataFields, fieldname);
    };

    this.availablefields = function() {
        return self.allFields.filter(function(field) {
            var notequalfield = function(otherfield) {
                return field.name !== otherfield.name;
            };

            return self.dataFields.every(notequalfield) &&
                self.rowFields.every(notequalfield) &&
                self.columnFields.every(notequalfield);
        });
    };

    this.getDataSourceFieldCaptions = function() {
        var row0;
        if(self.dataSource && (row0 = self.dataSource[0])) {
            var fieldNames = utils.ownProperties(row0);
            var headers = [];
            for(var i = 0; i < fieldNames.length; i++) {
                headers.push(self.nameToCaption(fieldNames[i]));
            }
            return headers;
        }
        return null;
    };

    this.getPreFilters = function() {
        var prefilters = {};
        if(config.preFilters) {
            utils.ownProperties(config.preFilters).forEach(function(filteredField) {
                var prefilterConfig = config.preFilters[filteredField];
                if(utils.isArray(prefilterConfig)) {
                    prefilters[self.captionToName(filteredField)] = new filtering.expressionFilter(null, null, prefilterConfig, false);
                } else {
                    var opname = utils.ownProperties(prefilterConfig)[0];
                    if(opname) {
                        prefilters[self.captionToName(filteredField)] = new filtering.expressionFilter(opname, prefilterConfig[opname]);
                    }
                }
            });
        }

        return prefilters;
    };


    this.moveField = function(fieldname, oldaxetype, newaxetype, position) {

        var oldaxe, oldposition;
        var newaxe;
        var fieldConfig;
        var defaultFieldConfig = getfield(self.allFields, fieldname);

        if (defaultFieldConfig) {

            switch (oldaxetype) {
                case axe.Type.ROWS:
                    oldaxe = self.rowFields;
                    break;
                case axe.Type.COLUMNS:
                    oldaxe = self.columnFields;
                    break;
                case axe.Type.DATA:
                    oldaxe = self.dataFields;
                    break;
                default:
                    break;
            }

            switch (newaxetype) {
                case axe.Type.ROWS:
                    newaxe = self.rowFields;
                    fieldConfig = self.getRowField(fieldname);
                    break;
                case axe.Type.COLUMNS:
                    newaxe = self.columnFields;
                    fieldConfig = self.getColumnField(fieldname);
                    break;
                case axe.Type.DATA:
                    newaxe = self.dataFields;
                    fieldConfig = self.getDataField(fieldname);
                    break;
                default:
                    break;
            }

            if (oldaxe || newaxe) {

                var newAxeSubtotalsState = self.areSubtotalsVisible(newaxetype);

                if (oldaxe) {
                    oldposition = getfieldindex(oldaxe, fieldname);
                    if (oldaxetype === newaxetype) {
                        if (oldposition == oldaxe.length - 1 &&
                            position == null ||
                            oldposition === position - 1) {
                            return false;
                        }
                    }
                    oldaxe.splice(oldposition, 1);
                }

                var field = createfield(
                    self, 
                    newaxetype, 
                    fieldConfig, 
                    defaultFieldConfig);

                if(!newAxeSubtotalsState && field.subTotal.visible !== false) {
                    field.subTotal.visible = null;
                }

                if (newaxe) {
                    if (position != null) {
                        newaxe.splice(position, 0, field);
                    } else {
                        newaxe.push(field);
                    }
                }

                // update data fields count
                self.dataFieldsCount = self.dataFields ? (self.dataFields.length || 1) : 1;

                return true;
            }
        }
    };

    this.toggleSubtotals = function(axetype) {

        var i;
        var axeFields;
        var newState = !self.areSubtotalsVisible(axetype);

        if(axetype === axe.Type.ROWS) {
            runtimeVisibility.subtotals.rows = newState;
            axeFields = self.rowFields;
        } else if(axetype === axe.Type.COLUMNS) {
            runtimeVisibility.subtotals.columns = newState;
            axeFields = self.columnFields;
        } else {
            return false;
        }
        
        newState = newState === false ? null : true;
        for(i = 0; i < axeFields.length; i++) {
            if(axeFields[i].subTotal.visible !== false) {
                axeFields[i].subTotal.visible = newState;
            }
        }
        return true;
    };

    this.areSubtotalsVisible = function(axetype) {
        if(axetype === axe.Type.ROWS) {
            return runtimeVisibility.subtotals.rows;
        } else if(axetype === axe.Type.COLUMNS) {
            return runtimeVisibility.subtotals.columns;
        } else {
            return null;
        }
    };


    this.toggleGrandtotal = function(axetype) {
        var newState = !self.isGrandtotalVisible(axetype);

        if(axetype === axe.Type.ROWS) {
            self.grandTotal.rowsvisible = newState;
        } else if(axetype === axe.Type.COLUMNS) {
            self.grandTotal.columnsvisible = newState;
        } else {
            return false;
        }
        return true;
    };

    this.isGrandtotalVisible = function(axetype) {
        if(axetype === axe.Type.ROWS) {
            return self.grandTotal.rowsvisible;
        } else if(axetype === axe.Type.COLUMNS) {
            return self.grandTotal.columnsvisible;
        } else {
            return false;
        }
    };
};