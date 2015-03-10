/**
 * @fileOverview orb.query
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

var utils = require('./orb.utils');
var axe = require('./orb.axe');
var aggregation = require('./orb.aggregation');

var queryBase = function(source, query, filters) {

    var self = this;

    this.source = source;
    this.query = query;
    this.filters = filters;

    this.extractResult = function(aggs, options, outerArgs) {
        if (outerArgs.multi === true) {
            var res = {};
            for (var ai = 0; ai < options.multiFieldNames.length; ai++) {
                res[options.multiFieldNames[ai]] = aggs[self.getCaptionName(options.multiFieldNames[ai])];
            }
            return res;
        } else {
            return aggs[outerArgs.datafieldname];
        }
    };

    this.measureFunc = function(datafieldname, multi, aggregateFunc, fieldsConfig) {

        var outerArgs = {
            datafieldname: self.getCaptionName(datafieldname),
            multi: multi,
            aggregateFunc: aggregateFunc
        };

        return function(options) {
            options = self.cleanOptions(options, arguments, outerArgs);
            var aggs = self.compute(options, fieldsConfig, multi);
            return self.extractResult(aggs, options, outerArgs);
        };
    };

    this.setDefaultAggFunctions = function(param) {
        /*********************
         * val() function    *
         *********************/
        // if there is a registered field with a name or caption 'val', use 'val_'
        var valname = self.query.val ? 'val_' : 'val';
        self.query[valname] = self.measureFunc(undefined, true, undefined, param);

        /*********************
         * sum(), avg(), ... *
         *********************/
        var aggFunctions = utils.ownProperties(aggregation);
        for (var funcIndex = 0; funcIndex < aggFunctions.length; funcIndex++) {
            var funcName = aggFunctions[funcIndex];
            if (funcName !== 'toAggregateFunc') {
                self.query[funcName] = self.measureFunc(
                    undefined,
                    true,
                    aggregation[funcName],
                    param
                );
            }
        }
    };

};

var pgridQuery = function(pgrid) {

    queryBase.call(this, pgrid, {}, {});

    var self = this;

    this.getCaptionName = function(caption) {
        return self.source.config.captionToName(caption);
    };

    this.cleanOptions = function(options, innerArgs, outerArgs) {
        var opts = {
            fieldNames: []
        };

        if (outerArgs.multi === true) {
            if (options && typeof options === 'object') {
                opts.aggregateFunc = options.aggregateFunc;
                opts.multiFieldNames = options.fields;
            } else {
                opts.aggregateFunc = outerArgs.aggregateFunc;
                opts.multiFieldNames = innerArgs;
            }

            for (var ai = 0; ai < opts.multiFieldNames.length; ai++) {
                opts.fieldNames.push(self.getCaptionName(opts.multiFieldNames[ai]));
            }
        } else {
            opts.aggregateFunc = options;
            opts.fieldNames.push(outerArgs.datafieldname);
        }

        if (opts.aggregateFunc) {
            opts.aggregateFunc = aggregation.toAggregateFunc(opts.aggregateFunc);
        }

        return opts;
    };

    this.setup = function(parameters) {
        var rowFields = self.source.config.rowFields;
        var colFields = self.source.config.columnFields;
        var datafields = self.source.config.dataFields;
        var fIndex;

        // row fields setup
        for (fIndex = 0; fIndex < rowFields.length; fIndex++) {
            self.slice(rowFields[fIndex], axe.Type.ROWS, rowFields.length - fIndex);
        }

        // column fields setup
        for (fIndex = 0; fIndex < colFields.length; fIndex++) {
            self.slice(colFields[fIndex], axe.Type.COLUMNS, colFields.length - fIndex);
        }

        // data fields setup
        for (fIndex = 0; fIndex < datafields.length; fIndex++) {
            var df = datafields[fIndex];
            var dfname = df.name;
            var dfcaption = df.caption || dfname;

            self.query[dfname] = self.query[dfcaption] = self.measureFunc(dfname);
        }

        if (parameters) {
            for (var param in parameters) {
                if (parameters.hasOwnProperty(param)) {
                    self.query[param](parameters[param]);
                }
            }
        }

        self.setDefaultAggFunctions();

        return self.query;
    };

    this.slice = function(field, axetype, depth) {
        self.query[field.name] = self.query[field.caption || field.name] = function(val) {
            var f = {
                name: field.name,
                val: val,
                depth: depth
            };
            (self.filters[axetype] = self.filters[axetype] || []).push(f);
            return self.query;
        };
    };

    function filterDimensions(upperDims, filter) {
        return function(dim) {
            return dim.value === filter.val &&
                (!upperDims || upperDims.some(
                    function(upperDim) {
                        var parent = dim.parent;
                        if (parent) {
                            while (parent.depth < upperDim.depth) {
                                parent = parent.parent;
                            }
                        }
                        return parent === upperDim;
                    }));
        };
    }

    this.applyFilters = function(axetype) {
        if (self.filters[axetype]) {
            var sortedFilters = self.filters[axetype].sort(function(f1, f2) {
                return f2.depth - f1.depth;
            });

            var currAxe = self.source[axetype === axe.Type.ROWS ? 'rows' : 'columns'];
            var filterIndex = 0;
            var filtered = null;
            while (filterIndex < sortedFilters.length) {
                var filter = sortedFilters[filterIndex];
                filtered = currAxe.dimensionsByDepth[filter.depth]
                    .filter(filterDimensions(filtered, filter));
                filterIndex++;
            }
            return filtered;
        }
        return null;
    };

    this.compute = function(options) {
        var rowdims = self.applyFilters(axe.Type.ROWS) || [self.source.rows.root];
        var coldims = self.applyFilters(axe.Type.COLUMNS) || [self.source.columns.root];

        var aggs;

        if (rowdims.length === 1 && coldims.length === 1) {
            aggs = {};
            for (var ai = 0; ai < options.fieldNames.length; ai++) {
                aggs[options.fieldNames[ai]] = self.source.getData(options.fieldNames[ai], rowdims[0], coldims[0], options.aggregateFunc);
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

            aggs = self.source.calcAggregation(rowIndexes, colIndexes, options.fieldNames, options.aggregateFunc);
        }

        return aggs;
    };
};

var arrayQuery = function(array) {

    queryBase.call(this, array, {}, []);

    var self = this;
    var captionToName = {};

    this.setCaptionName = function(caption, name) {
        captionToName[caption || name] = name;
    };

    this.getCaptionName = function(caption) {
        return captionToName[caption] || caption;
    };

    this.cleanOptions = function(options, innerArgs, outerArgs) {
        var opts = {
            fieldNames: []
        };

        if (outerArgs.multi === true) {
            if (options && typeof options === 'object') {
                opts.aggregateFunc = options.aggregateFunc;
                opts.multiFieldNames = options.fields;
            } else {
                opts.aggregateFunc = outerArgs.aggregateFunc;
                opts.multiFieldNames = innerArgs;
            }

            for (var ai = 0; ai < opts.multiFieldNames.length; ai++) {
                opts.fieldNames.push(self.getCaptionName(opts.multiFieldNames[ai]));
            }
        } else {
            opts.aggregateFunc = options || outerArgs.aggregateFunc;
            opts.fieldNames.push(outerArgs.datafieldname);
        }

        return opts;
    };

    this.setup = function(fieldsConfig) {

        self.query.slice = function(field, val) {
            var f = {
                name: field,
                val: val
            };
            self.filters.push(f);
            return self.query;
        };

        if (fieldsConfig) {

            var fieldNames = utils.ownProperties(fieldsConfig);

            for (var fi = 0; fi < fieldNames.length; fi++) {
                var fname = fieldNames[fi];
                var f = fieldsConfig[fname];
                var fcaption = f.caption || f.name;
                f.name = fname;

                self.setCaptionName(fcaption, fname);

                if (f.toAggregate) {
                    self.query[fname] = self.query[fcaption] = self.measureFunc(fname, false, f.aggregateFunc);
                } else {
                    self.slice(f);
                }
            }
        }

        self.setDefaultAggFunctions(fieldsConfig);

        return self.query;
    };

    this.slice = function(field) {
        self.query[field.name] = self.query[field.caption || field.name] = function(val) {
            return self.query.slice(field.name, val);
        };
    };

    this.applyFilters = function() {
        var rowIndexes = [];

        for (var i = 0; i < self.source.length; i++) {
            var row = self.source[i];
            var include = true;
            for (var j = 0; j < self.filters.length; j++) {
                var filter = self.filters[j];
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
    };

    this.compute = function(options, fieldsConfig, multi) {
        var rowIndexes = self.applyFilters();

        var aggs = {};

        for (var ai = 0; ai < options.fieldNames.length; ai++) {
            var datafield = options.fieldNames[ai];
            var aggFunc = aggregation.toAggregateFunc(
                multi === true ?
                options.aggregateFunc || (fieldsConfig && fieldsConfig[datafield] ?
                    fieldsConfig[datafield].aggregateFunc :
                    undefined) :
                options.aggregateFunc);

            aggs[datafield] = aggFunc(datafield, rowIndexes || 'all', self.source, rowIndexes, null);
        }

        return aggs;
    };
};

module.exports = function(source, fieldsConfig) {
    if (utils.isArray(source)) {
        return new arrayQuery(source).setup(fieldsConfig);
    } else {
        // assume it's a pgrid
        return function(parameters) {
            return new pgridQuery(source).setup(parameters);
        };
    }
};
