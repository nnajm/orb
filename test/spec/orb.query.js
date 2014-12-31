describe("test orb.query()", function() {

  var orbpath = '../../src/js/';

  var utils = require(orbpath + 'orb.utils');
  var orb = require(orbpath + 'orb');

  var config = require('./config');
  var pgrid = new orb.pgrid(config);

  var amountGt = 3497.3149;
  var quantityGt = 861;

  it(".Amount() should return " + amountGt + " /* Grand Total */", function() {

      var amount = pgrid.query().Amount();

      expect(amount).not.toBeNaN();
      expect(typeof amount).toBe('number');

      expect(amount).toBeCloseTo(amountGt);
  });

  it(".data('Amount', 'Q') should return { Amount:" + amountGt + ", Q:" + quantityGt + "} /* Grand Totals */", function() {

      var data = pgrid.query().data('Amount', 'Q');

      expect(data).not.toBeNull();
      
      expect(data.Amount).not.toBeUndefined();
      expect(data.Amount).not.toBeNaN();
      expect(typeof data.Amount).toBe('number');

      expect(data.Q).not.toBeUndefined();
      expect(data.Q).not.toBeNaN();
      expect(typeof data.Q).toBe('number');

      expect(data.Amount).toBeCloseTo(amountGt);
      expect(data.Q).toBeCloseTo(quantityGt);
  });

  describe(".Manufacturer('Adventure Works').Class('Economy')", function() {

    var amount1 = 1185.1736;
    var quantity1 = 44;
    var _q = pgrid.query()
                  .Manufacturer('Adventure Works')
                  .Class('Economy');

    it(".Amount() should return " + amount1, function() {

      var amount = _q.Amount();

      expect(amount).not.toBeNaN();
      expect(typeof amount).toBe('number');

      expect(amount).toBeCloseTo(amount1);
    });

    it(".data('Amount', 'Q') should return { Amount:" + amount1 + ", Q:" + quantity1 + "}", function() {

      var data = _q.data('Amount', 'Q');

      expect(data).not.toBeNull();
      
      expect(data.Amount).not.toBeUndefined();
      expect(data.Amount).not.toBeNaN();
      expect(typeof data.Amount).toBe('number');

      expect(data.Q).not.toBeUndefined();
      expect(data.Q).not.toBeNaN();
      expect(typeof data.Q).toBe('number');

      expect(data.Amount).toBeCloseTo(amount1);
      expect(data.Q).toBeCloseTo(quantity1);
    });

  });

});
