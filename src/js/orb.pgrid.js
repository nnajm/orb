
/**
 * @fileOverview Pivot Grid viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global orb */
/*jshint eqnull: true*/

(function(){

/**
 * Creates a new instance of pgrid
 * @class
 * @memberOf orb
 * @param  {object} config - configuration object
 */
orb.pgrid = function(config) {

	var defaultfield = { name: '#undefined#' };

	var self = this;
	var _iCache;

	this.config = new orb.config(config);

	this.rows = new orb.axe(self, orb.axe.Type.ROWS);
	this.columns = new orb.axe(self, orb.axe.Type.COLUMNS);
	this.dataMatrix = {};

	this.moveField = function(fieldname, oldaxetype, newaxetype, position) {
		if(self.config.moveField(fieldname, oldaxetype, newaxetype, position)) {			
			self.rows.update();
			self.columns.update();
			buildData();		
		}
	};

	this.getData = function(datafield, rowdim, coldim) {

		if(rowdim && coldim) {
			datafield = datafield || (self.config.dataFields[0] || defaultfield).name;

			if(self.dataMatrix[rowdim.id] && self.dataMatrix[rowdim.id][coldim.id]) {
				return self.dataMatrix[rowdim.id][coldim.id][datafield] || null;
			}
			return null;
		}
	};

	buildData();

	function calcCellData(rowIndexes, colIndexes, origRowIndexes) {

		var res = {};

		if(self.config.dataFieldsCount > 0) {

			var intersection;
			
			if(rowIndexes == null) {
				intersection = colIndexes;
			} else if(colIndexes == null) {
				intersection = rowIndexes;
			} else {
				intersection = [];
				for(var ri = 0; ri < rowIndexes.length; ri++) {
					var rowindex = rowIndexes[ri];
					if(rowindex >= 0) {
						var colrowindex = colIndexes.indexOf(rowindex);
						if(colrowindex >= 0) {
							rowIndexes[ri] = 0 - (rowindex + 2);
							intersection.push(rowindex);	
						}
					}
				}
			}

			var datasource = self.config.dataSource;

			for(var datafieldIndex = 0; datafieldIndex < self.config.dataFieldsCount; datafieldIndex++) {
				var datafield = self.config.dataFields[datafieldIndex] || defaultfield;
				if(datafield.aggregateFunc) {
					res[datafield.name] = datafield.aggregateFunc()(datafield.name, intersection || 'all', datasource, origRowIndexes, colIndexes);
				}
			}
		}

		return res;
	}

	function calcRowData(rowDim) {

		if(rowDim) {			
			var data = {};
			var rid = 'r' + rowDim.id;

			// set cached row indexes for current row dimension
			if(_iCache[rid] === undefined) {
				_iCache[rid] = rowDim.isRoot ? null : (_iCache[rowDim.parent.id] || rowDim.getRowIndexes());	
			}

			// calc grand-total cell
			data[self.columns.root.id] = calcCellData(rowDim.isRoot ? null : _iCache[rid].slice(0), null);

			if(self.columns.dimensionsCount > 0) {
				var p = 0;
				var parents = [self.columns.root];

				while(p < parents.length) {
					var parent = parents[p];
					var rowindexes = rowDim.isRoot ?
					null :
					(parent.isRoot ?
						_iCache[rid].slice(0) :
						_iCache['c' + parent.id].slice(0));

					for(var i = 0; i < parent.values.length; i++) {
						var subdim = parent.subdimvals[parent.values[i]];
						var cid = 'c' + subdim.id;

						// set cached row indexes for this column leaf dimension
						if(_iCache[cid] === undefined) {
							_iCache[cid] = _iCache[cid] || subdim.getRowIndexes().slice(0);	
						}

						data[subdim.id] = calcCellData(rowindexes, _iCache[cid], rowDim.isRoot ? null : rowDim.getRowIndexes());

						if(!subdim.isLeaf) {
							parents.push(subdim);
							if(rowindexes) {
								_iCache[cid] = [];
								for(var ur = 0; ur < rowindexes.length; ur++) {
									var vr = rowindexes[ur];
									if(vr != -1 && vr < 0) {
										_iCache[cid].push(0 - (vr + 2));
										rowindexes[ur] = -1;
									}
								}
							}
						}
					}
					_iCache['c' + parent.id] = undefined;
					p++;
				}
			}			

			return data;
		}
	}

	function buildData() {
		self.dataMatrix = {};
		_iCache = {};

		// calc grand total row
		self.dataMatrix[self.rows.root.id] = calcRowData(self.rows.root);

		if(self.rows.dimensionsCount > 0) {
			var parents = [self.rows.root];
			var p = 0;
			var parent;
			while(p < parents.length) {
				parent = parents[p];
				// calc children rows
				for(var i = 0; i < parent.values.length; i++) {
					var subdim = parent.subdimvals[parent.values[i]];
					// calc child row
					self.dataMatrix[subdim.id] = calcRowData(subdim);
					// if row is not a leaf, add it to parents array to process its children
					if(!subdim.isLeaf) {
						parents.push(subdim);
					}
				}
				// next parent
				p++;
			}
		}
	}
};

function forEachIntersection(datafield, intersection, datasource, callback) {
	var all = intersection === 'all';
	intersection = all ? datasource : intersection;
	if(intersection.length > 0) {			
		for(var i = 0; i < intersection.length; i++) {	
			callback((all ? intersection[i] : datasource[intersection[i]])[datafield]);
		}
	}
}

function calcVariance(datafield, intersection, datasource, population) {
	var variance = 0;
	var avg = 0;
	var len = (intersection === 'all' ? datasource : intersection).length;
	if(len > 0) {
		if(population || len > 1) {
			forEachIntersection(datafield, intersection, datasource, function(val) {
				avg += val;
			});
			avg /= len;
			forEachIntersection(datafield, intersection, datasource, function(val) {
				variance += (val - avg)*(val - avg);
			});
			variance = variance/(population ? len : len - 1);
		} else {
			variance = NaN;
		}
	}
	return variance;
}

orb.pgrid.aggregation = {
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
			if(min == null || val < min) {
				min = val;
			}
		});		
		return min;
	},
	max: function(datafield, intersection, datasource) {
		var max = null;
		forEachIntersection(datafield, intersection, datasource, function(val) {
			if(max == null || val > max) {
				max = val;
			}
		});
		return max;
	},
	avg: function(datafield, intersection, datasource) {
		var avg = 0;
		var len = (intersection === 'all' ? datasource : intersection).length;
		if(len > 0) {
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
		if(len > 0) {
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

}());
