/**
 * @fileOverview Pivot Grid viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

var axe = require('./orb.axe');
var configuration = require('./orb.config').config;

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

    this.rows = new axe(self, axe.Type.ROWS);
    this.columns = new axe(self, axe.Type.COLUMNS);
    this.dataMatrix = {};

    this.moveField = function(fieldname, oldaxetype, newaxetype, position) {
        if (self.config.moveField(fieldname, oldaxetype, newaxetype, position)) {
            self.rows.update();
            self.columns.update();
            buildData();
        }
    };

    this.getData = function(datafield, rowdim, coldim) {

        if (rowdim && coldim) {
            datafield = datafield || (self.config.dataFields[0] || defaultfield).name;

            if (self.dataMatrix[rowdim.id] && self.dataMatrix[rowdim.id][coldim.id]) {
                return self.dataMatrix[rowdim.id][coldim.id][datafield] || null;
            }
            return null;
        }
    };

    this.measures = function() {
        var ret = {};
        var filters = {};

        var pushFilter = function(axetype, fieldname, fielddepth) {            
            return function(val) {
                var f = {
                    name: fieldname,
                    val: val,
                    depth: fielddepth
                };
                (filters[axetype] = filters[axetype] || []).push(f);
                return ret;
            }
        }

        function filterDimensions(axetype) {
            if(filters[axetype]) {
                var rarr = filters[axetype].sort(function(f1, f2) {
                    return f2.depth - f1.depth;
                });

                var rfi = 0;
                var dims = null;
                while(rfi < rarr.length) {
                    dims = self[axetype === axe.Type.ROWS ? 'rows': 'columns'].dimensionsByDepth[rarr[rfi].depth].filter(function(d) {
                        return d.value === rarr[rfi].val
                        && (rfi === 0 || dims.some(function(dd) {
                            var parent = d.parent;
                            var dp = d.depth + 1;
                            while(dp < dd.depth) {
                                parent = parent.parent;
                                dp++;
                            }
                            return parent === dd;
                        }));
                    });

                    rfi++;
                }
                return dims;
            }
            return null;
        }

        var utils = require('./orb.utils');

        var getMeasure = function(datafieldname) {   
            return function() {
                var rowdims = filterDimensions(axe.Type.ROWS) || [self.rows.root];
                var coldims = filterDimensions(axe.Type.COLUMNS) || [self.columns.root];
                var res = [];
                for(var rdi = 0; rdi < rowdims.length; rdi++) {
                    for(var cdi = 0; cdi < coldims.length; cdi++) {
                        var resv = {};
                        var rowdim = rowdims[rdi];
                        var coldim = coldims[cdi];
                        resv[rowdim.isRoot ? 'total' : rowdim.field.name] = rowdim.value;
                        resv[coldim.isRoot ? 'total' : coldim.field.name] = coldim.value

                        if(arguments.length == 0) {
                            resv[datafieldname || 'data'] = self.getData(datafieldname, rowdim, coldim);
                        } else {
                            var datares = {};
                            for(var ai = 0; ai < arguments.length; ai++) {
                                datares[arguments[ai]] = self.getData(arguments[ai], rowdim, coldim);
                            }
                            resv.data = datares;
                        }
                        res.push(resv);
                    }                
                }
                return res;
            }
        }

        var rowfields = self.config.rowFields;
        var colfields = self.config.columnFields;
        var datafields = self.config.dataFields;

        for(var i = 0; i < rowfields.length; i++) {
            ret[rowfields[i].name] = pushFilter(axe.Type.ROWS, rowfields[i].name, rowfields.length - i);
        }

        for(var j = 0; j < colfields.length; j++) {
            ret[colfields[j].name] = pushFilter(axe.Type.COLUMNS, colfields[j].name, colfields.length - j);
        }

        for(var k = 0; k < datafields.length; k++) {
            ret[datafields[k].name] = getMeasure(datafields[k].name);
        }

        ret.data = getMeasure();

        return ret;

    };

    buildData();

    function calcCellData(rowIndexes, colIndexes, origRowIndexes) {

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

            var datasource = self.config.dataSource;

            for (var datafieldIndex = 0; datafieldIndex < self.config.dataFieldsCount; datafieldIndex++) {
                var datafield = self.config.dataFields[datafieldIndex] || defaultfield;
                if (datafield.aggregateFunc) {
                    res[datafield.name] = datafield.aggregateFunc()(datafield.name, intersection || 'all', datasource, origRowIndexes, colIndexes);
                }
            }
        }

        return res;
    }

    function calcRowData(rowDim) {

        if (rowDim) {
            var data = {};
            var rid = 'r' + rowDim.id;

            // set cached row indexes for current row dimension
            if (_iCache[rid] === undefined) {
                _iCache[rid] = rowDim.isRoot ? null : (_iCache[rowDim.parent.id] || rowDim.getRowIndexes());
            }

            // calc grand-total cell
            data[self.columns.root.id] = calcCellData(rowDim.isRoot ? null : _iCache[rid].slice(0), null);

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

                        data[subdim.id] = calcCellData(rowindexes, _iCache[cid], rowDim.isRoot ? null : rowDim.getRowIndexes());

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

    function buildData() {
        self.dataMatrix = {};
        _iCache = {};

        // calc grand total row
        self.dataMatrix[self.rows.root.id] = calcRowData(self.rows.root);

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
                    self.dataMatrix[subdim.id] = calcRowData(subdim);
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
