/**
 * @fileOverview Pivot Grid axe viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require, React */
/*jshint eqnull: true*/

var axe = require('./orb.axe');
var pgrid = require('./orb.pgrid');
var uiheaders = require('./orb.ui.header');
var uirows = require('./orb.ui.rows');
var uicols = require('./orb.ui.cols');
//var React = require('react');
var OrbReactComps = require('./react/orb.react.compiled');
/**
 * Creates a new instance of pivot grid control
 * @class
 * @memberOf orb.ui
 * @param  {object} pgrid - pivot grid instance
 */
module.exports = function(config) {

    var self = this;

    /**
     * Parent pivot grid
     * @type {orb.pgrid}
     */
    this.pgrid = new pgrid(config);

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
        if (axetype === axe.Type.ROWS) {
            self.pgrid.rows.sort(field);
        } else if (axetype === axe.Type.COLUMNS) {
            self.pgrid.columns.sort(field);
        } else {
            return;
        }

        buildUi();
    };

    this.moveField = function(field, oldAxeType, newAxeType, position) {
        self.pgrid.moveField(field, oldAxeType, newAxeType, position);
        buildUi();
    };

    this.filters = null;

    this.cells = [];

    this.render = function(element) {
        var pivotTableFactory = React.createFactory(OrbReactComps.PivotTable);
        var pivottable = pivotTableFactory({
            data: self,
            config: config
        });

        React.render(pivottable, element);
    };

    buildUi();

    function buildUi() {

        // build rows and columns
        self.rows = new uirows(self.pgrid.rows);
        self.columns = new uicols(self.pgrid.columns);

        var rowsInfos = self.rows.uiInfos;
        var rowsInfoslength = rowsInfos.length;

        var columnsInfos = self.columns.uiInfos;
        var columnsInfoslength = columnsInfos.length;

        var columnsAllHeaders = self.columns.leafsHeaders;
        var columnsAllHeaderslength = columnsAllHeaders.length;

        // set control properties		
        self.rowHeadersWidth = (self.pgrid.rows.fields.length || 1) + (self.pgrid.config.dataHeadersLocation === 'rows' && self.pgrid.config.dataFieldsCount > 1 ? 1 : 0);
        self.columnHeadersWidth = columnsAllHeaderslength;
        self.rowHeadersHeight = rowsInfoslength;
        self.columnHeadersHeight = (self.pgrid.columns.fields.length || 1) + (self.pgrid.config.dataHeadersLocation === 'columns' && self.pgrid.config.dataFieldsCount > 1 ? 1 : 0);
        self.totalWidth = self.rowHeadersWidth + self.columnHeadersWidth;
        self.totalHeight = self.rowHeadersHeight + self.columnHeadersHeight;

        var cells = [];
        setArrayLength(cells, columnsInfoslength + rowsInfoslength);

        function setArrayLength(arr, length) {
            if (arr.length !== length) {
                arr.length = length;
                return true;
            }
            return false;
        }

        var arr;


        for (var ci = 0; ci < columnsInfoslength; ci++) {

            var uiinfo = columnsInfos[ci];
            var prelength = 0;
            arr = (cells[ci] = cells[ci] || []);
            if (columnsInfoslength > 1 && ci === 0) {
                prelength = 1;
                setArrayLength(arr, prelength + uiinfo.length);
                arr[0] = new uiheaders.emptyCell(self.rowHeadersWidth, self.columnHeadersHeight - 1);
            } else if (ci === columnsInfoslength - 1) {
                prelength = self.rowHeadersWidth;
                setArrayLength(arr, prelength + uiinfo.length);
                if (self.pgrid.rows.fields.length > 0) {
                    for (var findex = 0; findex < self.pgrid.config.rowFields.length; findex++) {
                        arr[findex] = new uiheaders.buttonCell(self.pgrid.config.rowFields[findex]);
                    }
                } else {
                    arr[0] = new uiheaders.emptyCell(self.rowHeadersWidth, 1);
                }
            }

            for (var ui = 0; ui < uiinfo.length; ui++) {
                arr[prelength + ui] = uiinfo[ui];
            }
        }

        function createVisibleFunc(rowvisible, colvisible) {
            return function() {
                return rowvisible() && colvisible();
            };
        }


        for (var ri = 0; ri < rowsInfoslength; ri++) {
            var ruiinfo = rowsInfos[ri];

            arr = (cells[columnsInfoslength + ri] = cells[columnsInfoslength + ri] || new Array(ruiinfo.length + columnsAllHeaderslength));
            setArrayLength(arr, ruiinfo.length + columnsAllHeaderslength);

            for (var uri = 0; uri < ruiinfo.length; uri++) {
                arr[uri] = ruiinfo[uri];
            }

            var rinfo = ruiinfo[ruiinfo.length - 1];
            for (var cinfosIndex = 0; cinfosIndex < columnsAllHeaderslength; cinfosIndex++) {
                var cinfo = columnsAllHeaders[cinfosIndex];
                var isvisible = createVisibleFunc(rinfo.visible, cinfo.visible);
                arr[ruiinfo.length + cinfosIndex] = new uiheaders.dataCell(self.pgrid, isvisible, rinfo, cinfo);
            }
        }
        self.cells = cells;
    }
};
