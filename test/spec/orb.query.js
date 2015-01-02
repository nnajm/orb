require('./helpers/orbHelpers');

var testdata = require('./testdata');
var expected_amount = testdata.expected.amount;
var expected_quantity = testdata.expected.quantity;
var params = testdata.params;

var orbpath = '../../src/js/';
var utils = require(orbpath + 'orb.utils');
var orb = require(orbpath + 'orb');
var pgrid = new orb.pgrid(testdata.pgridConfig);

function expectToBeNumericCloseTo(actual, expected) {
    expect(actual).toBeNumeric();
    expect(actual).toBeCloseTo(expected);
}

describe("test orb.query(array)", function() {

    describe("<Grand Total>", function() {

        var _q = orb.query(testdata.dataSource);

        it(".val(6)", function() {
            var result = _q.val(6);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result[6], expected_amount.sum.gt);
        });

        it(".stdev(6)", function() {
            var result = _q.stdev(6);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result[6], expected_amount.stdev.gt);
        });

        it(".val({ aggregateFunc: 'avg', fields: [6]})", function() {
            var result = _q.val(params.queryAvgWithNames1);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result[6], expected_amount.avg.gt);
        });

        it(".val(5, 6)", function() {
            var result = _q.val(5, 6);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result[5], expected_quantity.sum.gt);
            expectToBeNumericCloseTo(result[6], expected_amount.sum.gt);
        });

        it(".stdev(5, 6)", function() {
            var result = _q.stdev(5, 6);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result[5], expected_quantity.stdev.gt);
            expectToBeNumericCloseTo(result[6], expected_amount.stdev.gt);
        });

        it(".val({ aggregateFunc: 'avg', fields: [5, 6]})", function() {
            var result = _q.val(params.queryAvgWithNames2);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result[5], expected_quantity.avg.gt);
            expectToBeNumericCloseTo(result[6], expected_amount.avg.gt);
        });
    });

    describe(".slice(2, 'Adventure Works').slice(3, 'Economy')", function() {

        var _q = orb.query(testdata.dataSource)
            .slice(2, 'Adventure Works')
            .slice(3, 'Economy');

        it(".val(6)", function() {
            var result = _q.val(6);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result[6], expected_amount.sum.sliced);
        });

        it(".stdev(6)", function() {
            var result = _q.stdev(6);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result[6], expected_amount.stdev.sliced);
        });

        it(".val({ aggregateFunc: 'avg', fields: [6]})", function() {
            var result = _q.val(params.queryAvgWithNames1);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result[6], expected_amount.avg.sliced);
        });

        it(".val(5, 6)", function() {
            var result = _q.val(5, 6);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result[5], expected_quantity.sum.sliced);
            expectToBeNumericCloseTo(result[6], expected_amount.sum.sliced);
        });

        it(".stdev(5, 6)", function() {
            var result = _q.stdev(5, 6);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result[5], expected_quantity.stdev.sliced);
            expectToBeNumericCloseTo(result[6], expected_amount.stdev.sliced);
        });

        it(".val({ aggregateFunc: 'avg', fields: [5, 6]})", function() {
            var result = _q.val(params.queryAvgWithNames2);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result[5], expected_quantity.avg.sliced);
            expectToBeNumericCloseTo(result[6], expected_amount.avg.sliced);
        });
    });
});

describe("test orb.query(array, fieldsConfig)", function() {

    describe("<Grand Total>", function() {

        var _q = orb.query(testdata.dataSource, params.queryFieldsConfig);

        it(".Amount()", function() {
            expectToBeNumericCloseTo(_q.Amount(), expected_amount.avg.gt);
        });

        it(".val('Amount')", function() {
            var result = _q.val('Amount');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.avg.gt);
        });

        it(".Amount('stdev')", function() {
            expectToBeNumericCloseTo(_q.Amount('stdev'), expected_amount.stdev.gt);
        });

        it(".stdev('Amount')", function() {
            var result = _q.stdev('Amount');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.gt);
        });

        it(".val({ aggregateFunc: 'stdev', fields: ['Amount']})", function() {
            var result = _q.val(params.queryStdevWithCaptions1);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.gt);
        });

        it(".val('Amount', 'Q')", function() {
            var result = _q.val('Amount', 'Q');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Q, expected_quantity.sum.gt);
            expectToBeNumericCloseTo(result.Amount, expected_amount.avg.gt);
        });

        it(".stdev('Amount', 'Q')", function() {
            var result = _q.stdev('Amount', 'Q');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.gt);
            expectToBeNumericCloseTo(result.Q, expected_quantity.stdev.gt);
        });

        it(".val({ aggregateFunc: 'stdev', fields: ['Amount', 'Q']})", function() {
            var result = _q.val(params.queryStdevWithCaptions2);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Q, expected_quantity.stdev.gt);
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.gt);
        });
    });

    describe(".Manufacturer('Adventure Works').Class('Economy')", function() {

        var _q = orb.query(testdata.dataSource, params.queryFieldsConfig)
            .Manufacturer('Adventure Works')
            .Class('Economy');

        it(".Amount()", function() {
            expectToBeNumericCloseTo(_q.Amount(), expected_amount.avg.sliced);
        });

        it(".val('Amount')", function() {
            var result = _q.val('Amount');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.avg.sliced);
        });

        it(".Amount('stdev')", function() {
            expectToBeNumericCloseTo(_q.Amount('stdev'), expected_amount.stdev.sliced);
        });

        it(".stdev('Amount')", function() {
            var result = _q.stdev('Amount');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.sliced);
        });

        it(".val({ aggregateFunc: 'stdev', fields: ['Amount']})", function() {
            var result = _q.val(params.queryStdevWithCaptions1);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.sliced);
        });

        it(".val('Amount', 'Q')", function() {
            var result = _q.val('Amount', 'Q');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Q, expected_quantity.sum.sliced);
            expectToBeNumericCloseTo(result.Amount, expected_amount.avg.sliced);
        });

        it(".stdev('Amount', 'Q')", function() {
            var result = _q.stdev('Amount', 'Q');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Q, expected_quantity.stdev.sliced);
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.sliced);
        });

        it(".val({ aggregateFunc: 'stdev', fields: ['Amount', 'Q']})", function() {
            var result = _q.val(params.queryStdevWithCaptions2);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Q, expected_quantity.stdev.sliced);
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.sliced);
        });
    });
});

describe("test pgrid.query()", function() {

    describe("<Grand Total>", function() {

        var _q = pgrid.query();

        it(".Amount()", function() {
            expectToBeNumericCloseTo(_q.Amount(), expected_amount.avg.gt);
        });

        it(".Amount('stdev')", function() {
            expectToBeNumericCloseTo(_q.Amount('stdev'), expected_amount.stdev.gt);
        });

        it(".stdev('Amount')", function() {
            var result = _q.stdev('Amount');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result['Amount'], expected_amount.stdev.gt);
        });

        it(".val('Amount')", function() {
            var result = _q.val('Amount');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.avg.gt);
        });

        it(".val({ aggregateFunc: 'stdev', fields: ['Amount'] })", function() {
            var result = _q.val(params.queryStdevWithCaptions1);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.gt);
        });

        it(".val('Amount', 'Q')", function() {
            var result = _q.val('Amount', 'Q');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.avg.gt);
            expectToBeNumericCloseTo(result.Q, expected_quantity.sum.gt);
        });

        it(".stdev('Amount', 'Q')", function() {
            var result = _q.stdev('Amount', 'Q');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.gt);
            expectToBeNumericCloseTo(result.Q, expected_quantity.stdev.gt);
        });

        it(".val({ aggregateFunc: 'stdev', fields: ['Amount', 'Q'] })", function() {
            var result = _q.val(params.queryStdevWithCaptions2);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.gt);
            expectToBeNumericCloseTo(result.Q, expected_quantity.stdev.gt);
        });
    });

    describe(".Manufacturer('Adventure Works').Class('Economy')", function() {

        var _q = pgrid.query()
            .Manufacturer('Adventure Works')
            .Class('Economy');

        it(".Amount()", function() {
            expectToBeNumericCloseTo(_q.Amount(), expected_amount.avg.sliced);
        });

        it(".val('Amount')", function() {
            var result = _q.val('Amount');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.avg.sliced);
        });

        it(".Amount('stdev')", function() {
            expectToBeNumericCloseTo(_q.Amount('stdev'), expected_amount.stdev.sliced);
        });

        it(".stdev('Amount')", function() {
            var result = _q.stdev('Amount');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.sliced);
        });

        it(".val({ aggregateFunc: 'stdev', fields: ['Amount'] })", function() {
            var result = _q.val(params.queryStdevWithCaptions1);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.sliced);
        });

        it(".val('Amount', 'Q')", function() {
            var result = _q.val('Amount', 'Q');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.avg.sliced);
            expectToBeNumericCloseTo(result.Q, expected_quantity.sum.sliced);
        });

        it(".stdev('Amount', 'Q')", function() {
            var result = _q.stdev('Amount', 'Q');
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.sliced);
            expectToBeNumericCloseTo(result.Q, expected_quantity.stdev.sliced);
        });

        it(".val({ aggregateFunc: 'stdev', fields: ['Amount', 'Q'] })", function() {
            var result = _q.val(params.queryStdevWithCaptions2);
            expect(result).not.toBeNull();
            expectToBeNumericCloseTo(result.Amount, expected_amount.stdev.sliced);
            expectToBeNumericCloseTo(result.Q, expected_quantity.stdev.sliced);
        });
    });
});
