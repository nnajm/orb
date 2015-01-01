/**
 * @fileOverview Pivot Grid viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

///***///var utils = require('./orb.utils');
var axe = require('./orb.axe');
var aggregation = require('./orb.aggregation');

module.exports = function(pgrid) {

    function setAxeFilters(axetype, fields, query, filters) {
        for (var i = 0; i < fields.length; i++) {
            pushFilter(axetype, fields[i], fields.length - i, query, filters);
        }
    }

    function pushFilter(axetype, field, depth, query, filters) {
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

    return function(parameters) {

        var query = {};
        var filters = {};

        var datafields = pgrid.config.dataFields;

        setAxeFilters(axe.Type.ROWS, pgrid.config.rowFields, query, filters);
        setAxeFilters(axe.Type.COLUMNS, pgrid.config.columnFields, query, filters);

        for (var k = 0; k < datafields.length; k++) {
            var df = datafields[k];
            var dfname = df.name;
            var dfcaption = df.caption || dfname;

            query[dfname] = query[dfcaption] = getMeasure(dfname);
        }

        query.data = getMeasure(undefined, true);

        if(parameters) {
            for(var param in parameters) {
                if(parameters.hasOwnProperty(param)) {
                    query[param](parameters[param]);
                }
            }
        }

        return query;

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
                    if(options && typeof options === 'object') {
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

                if(aggregateFunc) {
                    if (typeof aggregateFunc === 'string' && aggregation[aggregateFunc]) {
                        aggregateFunc = aggregation[aggregateFunc];
                    } else if (typeof func !== 'function') {
                        aggregateFunc = aggregation.sum;
                    }
                }

                var agg;

                if(rowdims.length === 1 && coldims.length === 1) {
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

                    agg = pgrid.calcAggregation(rowIndexes, colIndexes, rowIndexes, fieldNames, aggregateFunc);
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
};
