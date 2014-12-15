
/**
 * @fileOverview Pivot Grid axe viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global orb */
/*jshint eqnull: true*/

(function(){

/**
 * Creates a new instance of an axe's dimensions list.
 * @class
 * @memberOf orb
 * @param  {array} pgrid - Parent pivot grid
 * @param  {orb.axe.Type} type - Axe type (rows, columns, data)
 */
orb.axe = function(pgrid, type){

	var self = this;
	var dimid = 0;

	if(pgrid != null && pgrid.config != null) {

		/**
		 * Parent pivot grid
		 * @type {orb.pgrid}
		 */
		this.pgrid = pgrid;

		/**
		 * Axe type (rows, columns, data)
		 * @type {orb.axe.Type}
		 */
		this.type = type;

		/**
		 * This axe dimension fields
		 * @type {Array}
		 */
		this.fields = (function() {
			switch(type) {
				case orb.axe.Type.COLUMNS: 
					return self.pgrid.config.columnFields;
				case orb.axe.Type.ROWS: 
					return self.pgrid.config.rowFields;
				case orb.axe.Type.DATA: 
					return self.pgrid.config.dataFields;
				default:
					return [];
			}
		}());

		/**
		 * Number of dimensions in this axe
		 * @type {Number}
		 */
		this.dimensionsCount;

		/**
		 * Root dimension
		 * @type {orb.dimension}
		 */
		this.root;

		/** 
		 * Dimensions dictionary indexed by depth
		 * @type {Object} Dictionary of (depth, arrays)
		 */
		this.dimensionsByDepth;

		this.update = function() {
			self.dimensionsCount = self.fields.length;
			self.root = new orb.dimension(++dimid, null, null, null, self.dimensionsCount + 1, true);

			self.dimensionsByDepth = {};
			for(var depth = 1; depth <= self.dimensionsCount; depth++){
				self.dimensionsByDepth[depth] = [];
			}

			// fill data
			fill();	

			// initial sort
			for(var findex = 0; findex < self.fields.length; findex++) {
				var ffield = self.fields[findex];
				if(ffield.sort.order === 'asc' || ffield.sort.order === 'desc') {
					self.sort(ffield, true);
				}
			}
		}

		this.sort = function(field, donottoggle) {
			if(field != null) {
				if(donottoggle !== true) {
					if(field.sort.order !== 'asc') {
						field.sort.order = 'asc';
					} else {
						field.sort.order = 'desc';
					}
				}

				var depth = self.dimensionsCount - getfieldindex(field);
				var parents = depth === self.dimensionsCount ? [self.root] : self.dimensionsByDepth[depth + 1];
				for(var i = 0; i < parents.length; i++) {
					parents[i].values.sort();
					if(field.sort.order === 'desc') {
						parents[i].values.reverse();
					}
				}
			}
		};

		this.update();
	}

	function getfieldindex(field) {
		for(var i = 0; i < self.fields.length; i++) {
			if(self.fields[i].name === field.name) {
				return i;
			}
		}
		return -1;
	}

	/**
	 * Creates all subdimensions using the supplied data
	 */
	function fill() {

		if(self.pgrid.config.dataSource != null && self.dimensionsCount > 0) {

			var datasource = self.pgrid.config.dataSource;
			if(datasource != null && orb.utils.isArray(datasource) && datasource.length > 0) {
				for(var rowIndex = 0, dataLength = datasource.length; rowIndex < dataLength; rowIndex++) {
					var row = datasource[rowIndex];
					var dim = self.root;
					for(var findex = 0; findex < self.dimensionsCount; findex++) {
						var depth = self.dimensionsCount - findex;
						var subfield = self.fields[findex];
						var subvalue = row[subfield.name];
						var subdimvals = dim.subdimvals;

						if(subdimvals[subvalue] !== undefined){
							dim = subdimvals[subvalue];
						} else {
							dim.values.push(subvalue);
							dim = new orb.dimension(++dimid, dim, subvalue, subfield, depth, false, findex == self.dimensionsCount - 1);
							subdimvals[subvalue] = dim;
							dim.rowIndexes = [];
							self.dimensionsByDepth[depth].push(dim);
						}

						dim.rowIndexes.push(rowIndex);
					}
				}
			}
		}
	};
};

/**
 * Axe types
 * @readonly
 * @enum {Number}
 */
orb.axe.Type = {
	COLUMNS: 1,
	ROWS: 2,
	DATA: 3
}

}());