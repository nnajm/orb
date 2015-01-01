beforeEach(function () {
	jasmine.addMatchers({
		toBeNumeric: function () {
			return {
				compare: function (actual, expected) {
					return {
						pass: actual !== undefined && !isNaN(actual) && typeof(actual) === 'number'
					}
				}
			};
		}
	})
});