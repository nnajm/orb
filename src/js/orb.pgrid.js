/**
 * @fileOverview Pivot Grid viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

var axe = require('./orb.axe');
var configuration = require('./orb.config').config;
var filtering = require('./orb.filtering');
var query = require('./orb.query');
var utils = require('./orb.utils');

/**
 * Creates a new instance of pgrid
 * @class
 * @memberOf orb
 * @param  {object} config - configuration object
 */
module.exports = function(config) {

    var defaultfield = {
        name: '#undefined#'
    };

    var self = this;
    var _iCache;
    

    this.config = new configuration(config);
    this.filters = self.config.getPreFilters();
    this.filteredDataSource = self.config.dataSource;

    this.rows = new axe(self, axe.Type.ROWS);
    this.columns = new axe(self, axe.Type.COLUMNS);
    this.dataMatrix = {};

    function refresh(refreshFilters) {
        if(refreshFilters !== false) {
            refreshFilteredDataSource();
        }
        self.rows.update();
        self.columns.update();
        computeValues();
    }

    function refreshFilteredDataSource() {
        var filterFields = utils.ownProperties(self.filters);
        if(filterFields.length > 0) {
            self.filteredDataSource = [];

            for(var i = 0; i < self.config.dataSource.length; i++) {
                var row = self.config.dataSource[i];
                var exclude = false;
                for(var fi = 0; fi < filterFields.length; fi++) {
                    var fieldname = filterFields[fi];
                    var fieldFilter = self.filters[fieldname];

                    if(fieldFilter && !fieldFilter.test(row[fieldname])) {
                        exclude = true;
                        break;
                    }
                }
                if(!exclude) {
                    self.filteredDataSource.push(row);
                }
            }
        } else {
            self.filteredDataSource = self.config.dataSource;
        }
    }

    this.moveField = function(fieldname, oldaxetype, newaxetype, position) {
        if (self.config.moveField(fieldname, oldaxetype, newaxetype, position)) {
            refresh(false);
            return true;
        }
        return false;
    };

    this.applyFilter = function(fieldname, operator, term, staticValue, excludeStatic) {
        self.filters[fieldname] = new filtering.expressionFilter(operator, term, staticValue, excludeStatic);
        refresh();
    };

    this.refreshData = function(data) {
        self.config.dataSource = data;
        refresh();
    };

    this.getFieldValues = function(field, filterFunc) {
        var values1 = [];
        var values = [];
        var containsBlank = false;
        for(var i = 0; i < self.config.dataSource.length; i++) {
            var row = self.config.dataSource[i];
            var val = row[field];
            if(filterFunc !== undefined) {
                if(filterFunc === true || (typeof filterFunc === 'function' && filterFunc(val))) {
                    values1.push(val);
                }
            } else {
                if(val) {
                    values1.push(val);
                } else {
                    containsBlank = true;
                }
            }
        }
        if(values1.length > 1) {
            if(utils.isNumber(values1[0]) || utils.isDate(values1[0])) {
                values1.sort(function(a, b) { return a ? (b ? a - b : 1) : (b ? -1 : 0); });
            } else {
                values1.sort();
            }

            for(var vi = 0; vi < values1.length; vi++) {
                if(vi === 0 || values1[vi] !== values[values.length - 1]) {
                    values.push(values1[vi]);
                }
            }            
        } else {
            values = values1;
        }
        values.containsBlank = containsBlank;
        return values;
    };

    this.getFieldFilter = function(field) {
        return self.filters[field];
    };

    this.isFieldFiltered = function(field) {
        var filter = self.getFieldFilter(field);
        return filter != null && !filter.isAlwaysTrue();
    };

    this.getData = function(field, rowdim, coldim, aggregateFunc) {
        var value;
        if (rowdim && coldim) {

            var datafieldName = field || (self.config.dataFields[0] || defaultfield).name;
            var datafield = self.config.getDataField(datafieldName);
            
            if(!datafield || (aggregateFunc && datafield.aggregateFunc != aggregateFunc)) {
                value = self.calcAggregation(
                    rowdim.isRoot ? null : rowdim.getRowIndexes().slice(0), 
                    coldim.isRoot ? null : coldim.getRowIndexes().slice(0), 
                    [datafieldName], 
                    aggregateFunc)[datafieldName];
            } else {
                if (self.dataMatrix[rowdim.id] && self.dataMatrix[rowdim.id][coldim.id]) {
                    value = self.dataMatrix[rowdim.id][coldim.id][datafieldName];
                } else {
                    value = null;
                }
            }
        }

        return value === undefined ? null : value;
    };

    this.calcAggregation = function(rowIndexes, colIndexes, fieldNames, aggregateFunc) {
        return computeValue(rowIndexes, colIndexes, rowIndexes, fieldNames, aggregateFunc);
    };

    this.query = query(self);

    refresh();

    function computeValue(rowIndexes, colIndexes, origRowIndexes, fieldNames, aggregateFunc) {

        var res = {};

        if (self.config.dataFieldsCount > 0) {

            var intersection;

            if (rowIndexes == null) {
                intersection = colIndexes;
            } else if (colIndexes == null) {
                intersection = rowIndexes;
            } else {
                intersection = [];
                for (var ri = 0; ri < rowIndexes.length; ri++) {
                    var rowindex = rowIndexes[ri];
                    if (rowindex >= 0) {
                        var colrowindex = colIndexes.indexOf(rowindex);
                        if (colrowindex >= 0) {
                            rowIndexes[ri] = 0 - (rowindex + 2);
                            intersection.push(rowindex);
                        }
                    }
                }
            }

            var emptyIntersection = intersection && intersection.length === 0;
            var datasource = self.filteredDataSource;
            var datafield;
            var datafields = [];

            if(fieldNames) {
                for (var fieldnameIndex = 0; fieldnameIndex < fieldNames.length; fieldnameIndex++) {
                    datafield = self.config.getDataField(fieldNames[fieldnameIndex]);
                    if(!aggregateFunc) {
                        if(!datafield) {
                            datafield = self.config.getField(fieldNames[fieldnameIndex]);
                            if(datafield) {
                                aggregateFunc = datafield.dataSettings ? datafield.dataSettings.aggregateFunc() : datafield.aggregateFunc();
                            }
                        } else {
                            aggregateFunc = datafield.aggregateFunc();
                        }
                    }

                    if (datafield &&  aggregateFunc) {
                        datafields.push({ field: datafield, aggregateFunc: aggregateFunc});
                    }
                }                
            } else {
                for (var datafieldIndex = 0; datafieldIndex < self.config.dataFieldsCount; datafieldIndex++) {
                    datafield = self.config.dataFields[datafieldIndex] || defaultfield;
                    if (aggregateFunc || datafield.aggregateFunc) {
                        datafields.push({ field: datafield, aggregateFunc: aggregateFunc || datafield.aggregateFunc()});
                    }
                }
            }

            for(var dfi = 0; dfi < datafields.length; dfi++) {
                datafield = datafields[dfi];
                // no data
                if(emptyIntersection) {
                    res[datafield.field.name] = null;
                } else {
                    res[datafield.field.name] = datafield.aggregateFunc(datafield.field.name, intersection || 'all', self.filteredDataSource, origRowIndexes || rowIndexes, colIndexes);
                }
            }
        }

        return res;
    }

    function computeRowValues(rowDim) {

        if (rowDim) {
            var data = {};
            var rid = 'r' + rowDim.id;

            // set cached row indexes for current row dimension
            if (_iCache[rid] === undefined) {
                _iCache[rid] = rowDim.isRoot ? null : (_iCache[rowDim.parent.id] || rowDim.getRowIndexes());
            }

            // calc grand-total cell
            data[self.columns.root.id] = computeValue(rowDim.isRoot ? null : _iCache[rid].slice(0), null);

            if (self.columns.dimensionsCount > 0) {
                var p = 0;
                var parents = [self.columns.root];

                while (p < parents.length) {
                    var parent = parents[p];
                    var rowindexes = rowDim.isRoot ?
                        null :
                        (parent.isRoot ?
                            _iCache[rid].slice(0) :
                            _iCache['c' + parent.id].slice(0));

                    for (var i = 0; i < parent.values.length; i++) {
                        var subdim = parent.subdimvals[parent.values[i]];
                        var cid = 'c' + subdim.id;

                        // set cached row indexes for this column leaf dimension
                        if (_iCache[cid] === undefined) {
                            _iCache[cid] = _iCache[cid] || subdim.getRowIndexes().slice(0);
                        }

                        data[subdim.id] = computeValue(rowindexes, _iCache[cid], rowDim.isRoot ? null : rowDim.getRowIndexes());

                        if (!subdim.isLeaf) {
                            parents.push(subdim);
                            if (rowindexes) {
                                _iCache[cid] = [];
                                for (var ur = 0; ur < rowindexes.length; ur++) {
                                    var vr = rowindexes[ur];
                                    if (vr != -1 && vr < 0) {
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

    function computeValues() {
        self.dataMatrix = {};
        _iCache = {};

        // calc grand total row
        self.dataMatrix[self.rows.root.id] = computeRowValues(self.rows.root);

        if (self.rows.dimensionsCount > 0) {
            var parents = [self.rows.root];
            var p = 0;
            var parent;
            while (p < parents.length) {
                parent = parents[p];
                // calc children rows
                for (var i = 0; i < parent.values.length; i++) {
                    var subdim = parent.subdimvals[parent.values[i]];
                    // calc child row
                    self.dataMatrix[subdim.id] = computeRowValues(subdim);
                    // if row is not a leaf, add it to parents array to process its children
                    if (!subdim.isLeaf) {
                        parents.push(subdim);
                    }
                }
                // next parent
                p++;
            }
        }
    }
};
