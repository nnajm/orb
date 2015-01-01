describe("test orb.query()", function() {

  var orbpath = '../../src/js/';

  var utils = require(orbpath + 'orb.utils');
  var orb = require(orbpath + 'orb');

  require('./helpers/orbHelpers');

  var config = require('./config');
  var pgrid = new orb.pgrid(config);

  describe("/* Grand Total */", function() {

    var amountAvgGt = 3497.3149;
    var quantityAvgGt = 861;

    var amountStdevGt = 4345.37;
    var quantityStdevGt = 12.78;

    var _q = pgrid.query();

    it(".Amount()            = " + amountAvgGt, function() {

        var amount = _q.Amount();

        expect(amount).toBeNumeric();
        expect(amount).toBeCloseTo(amountAvgGt);
    });

    it(".data('Amount', 'Q') = { Amount:" + amountAvgGt + ", Q:" + quantityAvgGt + " }", function() {

        var data = _q.data('Amount', 'Q');

        expect(data).not.toBeNull();
        
        expect(data.Amount).toBeNumeric();
        expect(data.Amount).toBeCloseTo(amountAvgGt);

        expect(data.Q).toBeNumeric();
        expect(data.Q).toBeCloseTo(quantityAvgGt);
    });

    it(".Amount('stdev')     = " + amountStdevGt, function() {

        var amount = _q.Amount('stdev');

        expect(amount).toBeNumeric();
        expect(amount).toBeCloseTo(amountStdevGt);
    });

    it(".data({ aggregateFunc: 'stdev', fields: ['Amount', 'Q'] }) = { Amount:" + amountStdevGt + ", Q:" + quantityStdevGt + " }", function() {

        var data = _q.data({ aggregateFunc: 'stdev', fields: ['Amount', 'Q'] });

        expect(data).not.toBeNull();
        
        expect(data.Amount).toBeNumeric();
        expect(data.Amount).toBeCloseTo(amountStdevGt);

        expect(data.Q).toBeNumeric();
        expect(data.Q).toBeCloseTo(quantityStdevGt);
    });

  });

  describe(".Manufacturer('Adventure Works').Class('Economy')", function() {

    var amountAvg1 = 1185.1736;
    var quantityAvg1 = 44;
    var amountStdev1 = 1377.58;
    var quantityStdev1 = 3.9;

    var _q = pgrid.query()
                  .Manufacturer('Adventure Works')
                  .Class('Economy');

    it(".Amount()            = " + amountAvg1, function() {

      var amount = _q.Amount();

      expect(amount).toBeNumeric();
      expect(amount).toBeCloseTo(amountAvg1);
    });

    it(".data('Amount', 'Q') = { Amount:" + amountAvg1 + ", Q:" + quantityAvg1 + " }", function() {

      var data = _q.data('Amount', 'Q');

      expect(data).not.toBeNull();
      
      expect(data.Amount).toBeNumeric();
      expect(data.Amount).toBeCloseTo(amountAvg1);

      expect(data.Q).toBeNumeric();
      expect(data.Q).toBeCloseTo(quantityAvg1);
    });

    it(".Amount('stdev')     = " + amountStdev1, function() {

        var amount = _q.Amount('stdev');

        expect(amount).toBeNumeric();
        expect(amount).toBeCloseTo(amountStdev1);
    });

    it(".data({ aggregateFunc: 'stdev', fields: ['Amount', 'Q'] }) = { Amount:" + amountStdev1 + ", Q:" + quantityStdev1 + " }", function() {

        var data = _q.data({ aggregateFunc: 'stdev', fields: ['Amount', 'Q'] });

        expect(data).not.toBeNull();
        
        expect(data.Amount).toBeNumeric();
        expect(data.Amount).toBeCloseTo(amountStdev1);
        
        expect(data.Q).toBeNumeric();
        expect(data.Q).toBeCloseTo(quantityStdev1);
    });

  });

});
