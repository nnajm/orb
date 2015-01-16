module.exports = {
    ALL: '#All#',
    NONE: '#None#',
    BLANK: '#Blank#"',
};

var ops = module.exports.Operators = {
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
        }
    },
    MATCH: {
        name: 'Match',
        func: function(value, term) {
            if(value) {
                return value.toString().search(term) >= 0;
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
                return value.toString().search(term) < 0;
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
