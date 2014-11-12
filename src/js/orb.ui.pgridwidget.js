
/**
 * @fileOverview Pivot Grid axe viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global orb */
/*jshint eqnull: true*/

// Ensure orb.ui namespace is created
orb.utils.ns('orb.ui');

(function(){

/**
 * Creates a new instance of pivot grid control
 * @class
 * @memberOf orb.ui
 * @param  {object} pgrid - pivot grid instance
 */
orb.ui.pgridwidget = function(pgrid) {

	var self = this;

	if(pgrid != null) {
		/**
		 * Parent pivot grid
		 * @type {orb.pgrid}
		 */
		this.pgrid = pgrid;

		/**
		 * Control rows headers
		 * @type {orb.ui.rows}
		 */
		this.rows = null;
		/**
		 * Control columns headers
		 * @type {orb.ui.cols}
		 */
		this.columns = null;

		/**
		 * Total number of horizontal row headers.
		 * @type {Array<Array>}
		 */
		this.rowHeadersWidth = null;

		/**
		 * Total number of horizontal column headers.
		 * @type {Array<Array>}
		 */
		this.columnHeadersWidth = null;

		/**
		 * Total number of vertical row headers.
		 * @type {Array<Array>}
		 */
		this.rowHeadersHeight = null;

		/**
		 * Total number of horizontal column headers.
		 * @type {Array<Array>}
		 */
		this.columnHeadersHeight = null;

		/**
		 * Total number of horizontal cells of self pivot grid control.
		 * @type {Array<Array>}
		 */
		this.totalWidth = null;

		/**
		 * Total number of vertical cells of this pivot grid control.
		 * @type {Array<Array>}
		 */
		this.totalWidth = null;

		this.sort = function(axetype, field) {
			if(axetype === orb.axe.Type.ROWS) {
				this.pgrid.rows.sort(field);
			} else if(axetype === orb.axe.Type.COLUMNS) {
				this.pgrid.columns.sort(field);
			} else {
				return;
			}

			buildUi();
		}

		this.moveField = function(field, oldAxeType, newAxeType, position) {
			this.pgrid.moveField(field, oldAxeType, newAxeType, position);
			buildUi();
		}

		this.filters = null;

		this.cells = [];

		buildUi();
	}

	function buildUi() {

		// build rows and columns
		self.rows = new orb.ui.rows(pgrid.rows);
		self.columns = new orb.ui.cols(pgrid.columns);

		var rowsInfos = self.rows.uiInfos;
		var rowsInfoslength = rowsInfos.length;

		var columnsInfos = self.columns.uiInfos;
		var columnsInfoslength = columnsInfos.length;

		var columnsAllHeaders = self.columns.getAllHeaders();
		var columnsAllHeaderslength = columnsAllHeaders.length;

		// set control properties		
		self.rowHeadersWidth = pgrid.rows.fields.length || 1;
		self.columnHeadersWidth = columnsAllHeaderslength;
		self.rowHeadersHeight = rowsInfoslength;
		self.columnHeadersHeight = pgrid.columns.fields.length || 1;
		self.totalWidth = self.rowHeadersWidth + self.columnHeadersWidth;
		self.totalHeight = self.rowHeadersHeight + self.columnHeadersHeight;

		var cells = [];
		var cellsLengthChanged = setArrayLength(cells, columnsInfoslength + rowsInfoslength);

		function setArrayLength(arr, length) {
			if(arr.length !== length) {
				arr.length = length;
				return true;
			}
			return false;
		}
		

		for(var ci = 0; ci < columnsInfoslength; ci++) {

			var uiinfo = columnsInfos[ci];
			var arr = (cells[ci] = cells[ci] || []);
			var prelength = 0;
			if(columnsInfoslength > 1 && ci === 0){
				prelength = 1;
				setArrayLength(arr, prelength + uiinfo.length);
				arr[0] = new orb.ui.emptyCell(self.rowHeadersWidth, self.columnHeadersHeight - 1);
			} else if(ci === columnsInfoslength - 1) {
				prelength = self.rowHeadersWidth;
				setArrayLength(arr, prelength + uiinfo.length);
				if(self.pgrid.rows.fields.length > 0) {
					for(var findex = 0; findex < self.pgrid.config.rowfields.length; findex++) {
						arr[findex] = new orb.ui.buttonCell(self.pgrid.config.rowfields[findex]);
					}
				} else {
					arr[0] = new orb.ui.emptyCell(self.rowHeadersWidth, 1);
				}
			}
			
			for(var ui = 0; ui < uiinfo.length; ui++) {
				arr[prelength + ui] = uiinfo[ui];
			}
		}

  		
		for(var ri = 0; ri < rowsInfoslength; ri++) {
			var ruiinfo = rowsInfos[ri];

			arr = (cells[columnsInfoslength + ri] = cells[columnsInfoslength + ri] || new Array(ruiinfo.length + columnsAllHeaderslength));
			setArrayLength(arr, ruiinfo.length + columnsAllHeaderslength);
			
			for(var uri = 0; uri < ruiinfo.length; uri++) {
				arr[uri] = ruiinfo[uri];
			}
			
			var rinfo = ruiinfo[ruiinfo.length - 1];
			for(var cinfosIndex = 0; cinfosIndex < columnsAllHeaderslength; cinfosIndex++) {
				var cinfo = columnsAllHeaders[cinfosIndex];
				var isvisible = (function(rowvisible, colvisible) {
					return function() {
						return rowvisible() && colvisible();
					}
				}(rinfo.visible, cinfo.visible));
				arr[ruiinfo.length + cinfosIndex] = new orb.ui.dataCell(self.pgrid, isvisible, rinfo, cinfo);
			}
		}
		self.cells = cells;
	}
};

orb.ui.HeaderType = {
    EMPTY: 1,
    DATA_VALUE: 2,
    FIELD_BUTTON: 3,
	INNER: 4,
	WRAPPER: 5,
	SUB_TOTAL: 6,
	GRAND_TOTAL: 7,
	getHeaderClass: function(headerType, axetype) {
		var cssclass = '';
		switch(headerType) {
			case orb.ui.HeaderType.EMPTY:
			case orb.ui.HeaderType.FIELD_BUTTON:
				cssclass = 'empty';
				break;
			case orb.ui.HeaderType.INNER:
				if(axetype === orb.axe.Type.ROWS) {
					cssclass = 'header';
				} else if(axetype === orb.axe.Type.COLUMNS) {
					cssclass = 'header';
				}
				break;
			case orb.ui.HeaderType.WRAPPER:
				if(axetype === orb.axe.Type.ROWS) {
					cssclass = 'header';
				} else if(axetype === orb.axe.Type.COLUMNS) {
					cssclass = 'header';
				}
				break;
			case orb.ui.HeaderType.SUB_TOTAL:
				cssclass = 'header header-sub-total';
				break;
			case orb.ui.HeaderType.GRAND_TOTAL:
				cssclass = 'header header-grand-total';
				break;
		}

		return cssclass;
	},
	getCellClass: function(rowHeaderType, colHeaderType) {
		var cssclass = '';
		switch(rowHeaderType) {
			case orb.ui.HeaderType.GRAND_TOTAL: 
				cssclass = 'cell-grand-total';
				break;
			case orb.ui.HeaderType.SUB_TOTAL: 
				if(colHeaderType === orb.ui.HeaderType.GRAND_TOTAL) {
					cssclass = 'cell-grand-total';
				} else {
					cssclass = 'cell-sub-total';
				}				
				break;
			default:
				if(colHeaderType === orb.ui.HeaderType.GRAND_TOTAL) {
					cssclass = 'cell-grand-total';
				} else if(colHeaderType === orb.ui.HeaderType.SUB_TOTAL) {
					cssclass = 'cell-sub-total';
				} else {
					cssclass = 'cell';
				}
		}
		return cssclass;
	}
};

}());