/**
 * @fileOverview Pivot Grid viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

///***///var utils = require('./orb.utils');
var axe = require('./orb.axe');

module.exports = function(pgrid) {

    return function(parameters) {

        var query = {};
        var filters = {};

        var rowfields = pgrid.config.rowFields;
        var colfields = pgrid.config.columnFields;
        var datafields = pgrid.config.dataFields;

        for (var i = 0; i < rowfields.length; i++) {
            query[rowfields[i].name] = pushFilter(
                axe.Type.ROWS,
                rowfields[i].name,
                rowfields.length - i
            );
        }

        for (var j = 0; j < colfields.length; j++) {
            query[colfields[j].name] = pushFilter(
                axe.Type.COLUMNS,
                colfields[j].name,
                colfields.length - j
            );
        }

        for (var k = 0; k < datafields.length; k++) {
            query[datafields[k].name] = getMeasure(datafields[k].name);
            query[datafields[k].name].flat = getMeasure(datafields[k].name, true);
        }

        query.data = getMeasure();
        query.data.flat = getMeasure(undefined, true);

        if(parameters) {
            for(var param in parameters) {
                if(parameters.hasOwnProperty(param)) {
                    query[param](parameters[param]);
                }
            }
        }

        return query;

        function pushFilter(axetype, fieldname, fielddepth) {
            return function(val) {
                var f = {
                    name: fieldname,
                    val: val,
                    depth: fielddepth
                };
                (filters[axetype] = filters[axetype] || []).push(f);
                return query;
            }
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

        function getMeasure(datafieldname, flat) {
            return function() {
                var rowdims = applyFilter(axe.Type.ROWS) || [pgrid.rows.root];
                var coldims = applyFilter(axe.Type.COLUMNS) || [pgrid.columns.root];
                var res = [];
                for (var rdi = 0; rdi < rowdims.length; rdi++) {
                    for (var cdi = 0; cdi < coldims.length; cdi++) {
                        var rowdim = rowdims[rdi];
                        var coldim = coldims[cdi];
                        var resv;

                        if(flat !== true) {
                            resv = {};
                            if(!rowdim.isRoot) { resv[rowdim.field.name] = rowdim.value; }
                            if(!coldim.isRoot) { resv[coldim.field.name] = coldim.value; }

                            if (arguments.length == 0) {
                                resv[datafieldname || 'data'] = pgrid.getData(datafieldname, rowdim, coldim);
                            } else {
                                var datares = {};
                                for (var ai = 0; ai < arguments.length; ai++) {
                                    datares[arguments[ai]] = pgrid.getData(arguments[ai], rowdim, coldim);
                                }
                                resv.data = datares;
                            }
                        } else {
                            resv = [];
                            if (arguments.length == 0) {
                                resv.push(pgrid.getData(datafieldname, rowdim, coldim));
                            } else {
                                for (var ai = 0; ai < arguments.length; ai++) {
                                    resv.push(pgrid.getData(arguments[ai], rowdim, coldim));
                                }
                            }
                        }
                        res.push(resv);
                    }
                }
                return res;
            }
        }
    };
};
