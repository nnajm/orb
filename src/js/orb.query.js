/**
 * @fileOverview Pivot Grid viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

var utils = require('./orb.utils');
var axe = require('./orb.axe');
var aggregation = require('./orb.aggregation');
var orb = require('./orb');

module.exports = function(source, fieldsConfig) {

    if(source instanceof orb.pgrid) {

        var pgrid = source;

        return function(parameters) {

            var query = {};
            var filters = {};

            var datafields = pgrid.config.dataFields;

            setAxeFilters(axe.Type.ROWS, pgrid.config.rowFields);
            setAxeFilters(axe.Type.COLUMNS, pgrid.config.columnFields);

            for (var k = 0; k < datafields.length; k++) {
                var df = datafields[k];
                var dfname = df.name;
                var dfcaption = df.caption || dfname;

                query[dfname] = query[dfcaption] = getMeasure(dfname);
            }

            query.data = getMeasure(undefined, true);

            if (parameters) {
                for (var param in parameters) {
                    if (parameters.hasOwnProperty(param)) {
                        query[param](parameters[param]);
                    }
                }
            }

            return query;

            function setAxeFilters(axetype, fields) {
                for (var i = 0; i < fields.length; i++) {
                    pushFilter(axetype, fields[i], fields.length - i);
                }
            }

            function pushFilter(axetype, field, depth) {
                return (query[field.name] = query[field.caption || field.name] = function(val) {
                    var f = {
                        name: field.name,
                        val: val,
                        depth: depth
                    };
                    (filters[axetype] = filters[axetype] || []).push(f);
                    return query;
                });
            }

            function applyFilter(axetype) {
                if (filters[axetype]) {
                    var rarr = filters[axetype].sort(function(f1, f2) {
                        return f2.depth - f1.depth;
                    });

                    var rfi = 0;
                    var dims = null;
                    while (rfi < rarr.length) {
                        dims = pgrid[axetype === axe.Type.ROWS ? 'rows' : 'columns'].dimensionsByDepth[rarr[rfi].depth].filter(function(d) {
                            return d.value === rarr[rfi].val && (rfi === 0 || dims.some(function(dd) {
                                var parent = d.parent;
                                var dp = d.depth + 1;
                                while (dp < dd.depth) {
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


            function getMeasure(datafieldname, multi) {
                datafieldname = pgrid.config.captionToName(datafieldname);

                return function(options) {
                    var rowdims = applyFilter(axe.Type.ROWS) || [pgrid.rows.root];
                    var coldims = applyFilter(axe.Type.COLUMNS) || [pgrid.columns.root];

                    var aggregateFunc;
                    var ai;
                    var multiFieldNames;
                    var fieldNames = [];
                    if (multi === true) {
                        if (options && typeof options === 'object') {
                            aggregateFunc = options.aggregateFunc;
                            multiFieldNames = options.fields;
                        } else {
                            aggregateFunc = undefined;
                            multiFieldNames = arguments;
                        }

                        for (ai = 0; ai < multiFieldNames.length; ai++) {
                            fieldNames.push(pgrid.config.captionToName(multiFieldNames[ai]));
                        }
                    } else {
                        aggregateFunc = options;
                        fieldNames.push(datafieldname);
                    }

                    if (aggregateFunc) {
                        if (typeof aggregateFunc === 'string' && aggregation[aggregateFunc]) {
                            aggregateFunc = aggregation[aggregateFunc];
                        } else if (typeof func !== 'function') {
                            aggregateFunc = aggregation.sum;
                        }
                    }

                    var agg;

                    if (rowdims.length === 1 && coldims.length === 1) {
                        agg = {};
                        for (ai = 0; ai < fieldNames.length; ai++) {
                            agg[fieldNames[ai]] = pgrid.getData(fieldNames[ai], rowdims[0], coldims[0], aggregateFunc);
                        }
                    } else {
                        var rowIndexes = [];
                        var colIndexes = [];

                        for (var rdi = 0; rdi < rowdims.length; rdi++) {
                            rowIndexes = rowIndexes.concat(rowdims[rdi].getRowIndexes());
                        }
                        for (var cdi = 0; cdi < coldims.length; cdi++) {
                            colIndexes = colIndexes.concat(coldims[cdi].getRowIndexes());
                        }

                        agg = pgrid.calcAggregation(rowIndexes, colIndexes, fieldNames, aggregateFunc);
                    }

                    if (multi === true) {
                        var res = {};
                        for (ai = 0; ai < multiFieldNames.length; ai++) {
                            res[multiFieldNames[ai]] = agg[pgrid.config.captionToName(multiFieldNames[ai])];
                        }
                        return res;
                    } else {
                        return agg[datafieldname];
                    }
                }
            }
        };
    } else if(utils.isArray(source)) {

        var array = source;

        return (function() {

            var query = {};
            var filters = [];
            var captionToName = {};

            if(fieldsConfig) {
                var fieldNames = utils.ownProperties(fieldsConfig);
                for (var fi = 0; fi < fieldNames.length; fi++) {
                    var fname = fieldNames[fi];
                    var f = fieldsConfig[fname];
                    var fcaption = f.caption || fname;

                    captionToName[fcaption] = fname;

                    if(f.toAggregate) {
                        query[fname] = query[fcaption] = getMeasure(fname, false, f.aggregateFunc);
                    } else {
                        query[fname] = query[fcaption] = (function(fieldname) {
                            return function(val) {
                                return query.slice(fieldname, val);                                
                            };
                        }(fname));
                    }
                }
            }

            query.slice = function(field, val) {
                var f = {
                    name: field,
                    val: val
                };
                filters.push(f);
                return query;
            };

            query.data = getMeasure(undefined, true);

            return query;

            function applyFilters() {
                var rowIndexes = [];

                for (var i = 0; i < array.length; i++) {
                    var row = array[i];
                    var include = true;
                    for (var j = 0; j < filters.length; j++) {
                        var filter = filters[j];
                        if (row[filter.name] !== filter.val) {
                            include = false;
                            break;
                        }
                    }
                    if (include) {
                        rowIndexes.push(i);
                    }
                }

                return rowIndexes;
            }

            function toAggregateFunc(aggregateFunc) {
                if (aggregateFunc) {
                    if (typeof aggregateFunc === 'string' && aggregation[aggregateFunc]) {
                        return aggregation[aggregateFunc];
                    } else if (typeof func !== 'function') {
                        return aggregation.sum;
                    }
                } else {
                    return aggregation.sum;
                }
            }


            function getMeasure(datafieldname, multi, aggregateFunc) {

                datafieldname = captionToName[datafieldname] || datafieldname;

                return function(options) {
                    var rowIndexes = applyFilters();

                    var ai;
                    var multiFieldNames;
                    var fieldNames = [];

                    if (multi === true) {
                        if (options && typeof options === 'object') {
                            aggregateFunc = options.aggregateFunc;
                            multiFieldNames = options.fields;
                        } else {
                            aggregateFunc = undefined;
                            multiFieldNames = arguments;
                        }

                        for (ai = 0; ai < multiFieldNames.length; ai++) {
                            fieldNames.push(captionToName[multiFieldNames[ai]] || multiFieldNames[ai]);
                        }
                    } else {
                        aggregateFunc = options || aggregateFunc;
                        fieldNames.push(datafieldname);
                    }

                    var agg = {};

                    for (ai = 0; ai < fieldNames.length; ai++) {
                        var datafield = fieldNames[ai];
                        var aggFunc = toAggregateFunc(multi === true ? 
                            aggregateFunc || (fieldsConfig && fieldsConfig[datafield] ? fieldsConfig[datafield].aggregateFunc : undefined) :
                            aggregateFunc);

                        agg[datafield] = aggFunc(datafield, rowIndexes || 'all', array, rowIndexes, null);
                    }

                    if (multi === true) {
                        var res = {};
                        for (ai = 0; ai < multiFieldNames.length; ai++) {
                            res[multiFieldNames[ai]] = agg[captionToName[multiFieldNames[ai]] || multiFieldNames[ai]];
                        }
                        return res;
                    } else {
                        return agg[datafieldname];
                    }
                };
            }
        }());

    } else {
        return;
    }
};
