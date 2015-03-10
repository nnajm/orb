/**
 * @fileOverview Pivot Grid axe viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

var utils = require('./orb.utils');

var filtering = module.exports = {
    ALL: '#All#',
    NONE: '#None#',
    BLANK: '#Blank#"'
};

filtering.expressionFilter = function(operator, term, staticValue, excludeStatic) {
    var self = this;

    this.operator = ops.get(operator);
    this.regexpMode = false;
    this.term = term || null;
    if(this.term && this.operator && this.operator.regexpSupported) {
        if(utils.isRegExp(this.term)) {
            this.regexpMode = true;
            if(!this.term.ignoreCase) {
                this.term = new RegExp(this.term.source, 'i');
            }
        }
    }

    this.staticValue = staticValue;
    this.excludeStatic = excludeStatic;

    this.test = function(value) {
        if(utils.isArray(self.staticValue)) {
            var found = self.staticValue.indexOf(value) >= 0;
            return (self.excludeStatic && !found) || (!self.excludeStatic && found);            
        } else if(self.term) {
            return self.operator.func(value, self.term);
        } else if(self.staticValue === true || self.staticValue === filtering.ALL) {
            return true;
        } else if(self.staticValue === false || self.staticValue === filtering.NONE) {
            return false;
        } else {
            return true;
        }
    };

    this.isAlwaysTrue = function() {
        return !(self.term || utils.isArray(self.staticValue) || self.staticValue === filtering.NONE || self.staticValue === false);
    };
};

var ops = filtering.Operators = {
    get: function(opname) {
        switch(opname) {
            case ops.MATCH.name: return ops.MATCH;
            case ops.NOTMATCH.name: return ops.NOTMATCH;
            case ops.EQ.name: return ops.EQ;
            case ops.NEQ.name: return ops.NEQ;
            case ops.GT.name: return ops.GT;
            case ops.GTE.name: return ops.GTE;
            case ops.LT.name: return ops.LT;
            case ops.LTE.name: return ops.LTE;
            default: return ops.NONE;
        }
    },
    NONE: null,
    MATCH: {
        name: 'Matches',
        func: function(value, term) {
            if(value) {
                return value.toString().search(utils.isRegExp(term) ? term : new RegExp(term, 'i')) >= 0;
            } else {
                return !(!!term);
            }
        },
        regexpSupported: true
    },
    NOTMATCH: {
        name: 'Does Not Match',
        func: function(value, term) {
            if(value) {
                return value.toString().search(utils.isRegExp(term) ? term : new RegExp(term, 'i')) < 0;
            } else {
                return !!term;
            }
        },
        regexpSupported: true
    },
    EQ: {
        name: '=',
        func: function(value, term) {
            return value == term;
        },
        regexpSupported: false
    },
    NEQ: {
        name: '<>',
        func: function(value, term) {
            return value != term;
        },
        regexpSupported: false
    },
    GT: {
        name: '>',
        func: function(value, term) {
            return value > term;
        },
        regexpSupported: false
    },
    GTE: {
        name: '>=',
        func: function(value, term) {
            return value >= term;
        },
        regexpSupported: false
    },
    LT: {
        name: '<',
        func: function(value, term) {
            return value < term;
        },
        regexpSupported: false
    },
    LTE: {
        name: '<=',
        func: function(value, term) {
            return value <= term;
        },
        regexpSupported: false
    }
};
