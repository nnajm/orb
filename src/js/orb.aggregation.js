/**
 * @fileOverview Pivot Grid default aggregation functions
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module */
/*jshint eqnull: true*/

var Aggregations = module.exports = {
    toAggregateFunc: function(func) {
        if (func) {
            if (typeof func === 'string' && Aggregations[func]) {
                return Aggregations[func];
            } else if (typeof func === 'function') {
                return func;
            } else {
                return Aggregations.sum;
            }
        } else {
            return Aggregations.sum;
        }
    },
    count: function(datafield, intersection, datasource) {
        return intersection === 'all' ? datasource.length : intersection.length;
    },
    sum: function(datafield, intersection, datasource) {
        var sum = 0;
        forEachIntersection(datafield, intersection, datasource, function(val) {
            sum += val;
        });
        return sum;
    },
    min: function(datafield, intersection, datasource) {
        var min = null;
        forEachIntersection(datafield, intersection, datasource, function(val) {
            if (min == null || val < min) {
                min = val;
            }
        });
        return min;
    },
    max: function(datafield, intersection, datasource) {
        var max = null;
        forEachIntersection(datafield, intersection, datasource, function(val) {
            if (max == null || val > max) {
                max = val;
            }
        });
        return max;
    },
    avg: function(datafield, intersection, datasource) {
        var avg = 0;
        var len = (intersection === 'all' ? datasource : intersection).length;
        if (len > 0) {
            forEachIntersection(datafield, intersection, datasource, function(val) {
                avg += val;
            });
            avg /= len;
        }
        return avg;
    },
    prod: function(datafield, intersection, datasource) {
        var prod;
        var len = (intersection === 'all' ? datasource : intersection).length;
        if (len > 0) {
            prod = 1;
            forEachIntersection(datafield, intersection, datasource, function(val) {
                prod *= val;
            });
        }
        return prod;
    },
    stdev: function(datafield, intersection, datasource) {
        return Math.sqrt(calcVariance(datafield, intersection, datasource, false));
    },
    stdevp: function(datafield, intersection, datasource) {
        return Math.sqrt(calcVariance(datafield, intersection, datasource, true));
    },
    var: function(datafield, intersection, datasource) {
        return calcVariance(datafield, intersection, datasource, false);
    },
    varp: function(datafield, intersection, datasource) {
        return calcVariance(datafield, intersection, datasource, true);
    }
};

function calcVariance(datafield, intersection, datasource, population) {
    var variance = 0;
    var avg = 0;
    var len = (intersection === 'all' ? datasource : intersection).length;
    if (len > 0) {
        if (population || len > 1) {
            forEachIntersection(datafield, intersection, datasource, function(val) {
                avg += val;
            });
            avg /= len;
            forEachIntersection(datafield, intersection, datasource, function(val) {
                variance += (val - avg) * (val - avg);
            });
            variance = variance / (population ? len : len - 1);
        } else {
            variance = NaN;
        }
    }
    return variance;
}

function forEachIntersection(datafield, intersection, datasource, callback) {
    var all = intersection === 'all';
    intersection = all ? datasource : intersection;
    if (intersection.length > 0) {
        for (var i = 0; i < intersection.length; i++) {
            callback((all ? intersection[i] : datasource[intersection[i]])[datafield]);
        }
    }
}
