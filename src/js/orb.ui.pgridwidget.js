/**
 * @fileOverview Pivot Grid axe viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require, React, window */
/*jshint eqnull: true*/

var ReactDOM = typeof window === 'undefined' ? require('react-dom') : window.ReactDOM,
    axe = require('./orb.axe'),
    pgrid = require('./orb.pgrid'),
    uiheaders = require('./orb.ui.header'),
    uirows = require('./orb.ui.rows'),
    uicols = require('./orb.ui.cols'),

    Dialog = require('./react/orb.react.Dialog.jsx'),
    PivotChart = require('./react/orb.react.PivotChart.jsx'),
    PivotTable = require('./react/orb.react.PivotTable.jsx'),
    Grid = require('./react/orb.react.Grid.jsx');

/**
 * Creates a new instance of pivot grid control
 * @class
 * @memberOf orb.ui
 * @param  {object} pgrid - pivot grid instance
 */
module.exports = function(config) {

    var self = this;
    var renderElement;
    var pivotComponent;
    var dialog = Dialog.create();

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
     * Control data rows
     * @type {orb.ui.CellBase}
     */
    this.dataRows = [];

    this.layout = {
        rowHeaders: {
            /**
             * Total number of horizontal row headers.
             * @type {Number}
             */
            width: null,
            /**
             * Total number of vertical row headers.
             * @type {Number}
             */
            height: null
        },
        columnHeaders: {
            /**
             * Total number of horizontal column headers.
             * @type {Number}
             */
            width: null,
            /**
             * Total number of vertical column headers.
             * @type {Number}
             */
            height: null
        },
        pivotTable: {
            /**
             * Total number of horizontal cells of the whole pivot grid control.
             * @type {Number}
             */
            width: null,
            /**
             * Total number of vertical cells of the whole pivot grid control.
             * @type {Number}
             */
            height: null
        }
    };
    
    this.expandRow = function(cell) {
        cell.expand();
        this.render();
    };
  
    this.collapseRow = function(cell) {
        cell.subtotalHeader.collapse();
        this.render();
    };

    this.sort = function(axetype, field) {
        self.pgrid.sort(axetype, field);
    };

    this.refreshData = function(data) {
        self.pgrid.refreshData(data);
    };

    this.applyFilter = function(fieldname, operator, term, staticValue, excludeStatic) {
        self.pgrid.applyFilter(fieldname, operator, term, staticValue, excludeStatic);
    };

    this.moveField = function(field, oldAxeType, newAxeType, position) {
        self.pgrid.moveField(field, oldAxeType, newAxeType, position);
    };

    this.toggleFieldExpansion = function (axetype, field, newState) {
        var axeToExpand =
            axetype === axe.Type.ROWS
            ? self.rows
            : (axetype === axe.Type.COLUMNS
            ? self.columns
            : null);

        if (axeToExpand && axeToExpand.toggleFieldExpansion(field, newState)) {
            self.render();
        }        
    };

    this.toggleSubtotals = function(axetype) {
        self.pgrid.toggleSubtotals(axetype);
    };

    this.areSubtotalsVisible = function(axetype) {
        return self.pgrid.areSubtotalsVisible(axetype);
    };

    this.toggleGrandtotal = function(axetype) {
        self.pgrid.toggleGrandtotal(axetype);
    };

    this.isGrandtotalVisible = function(axetype) {
        return self.pgrid.isGrandtotalVisible(axetype);
    };

    this.changeTheme = function(newTheme) {
        pivotComponent.changeTheme(newTheme);
    };

    this.render = function(element) {
        renderElement = element || renderElement;
        if(renderElement) {            
            var pivotTableFactory = React.createFactory(
                self.pgrid.config.chartMode.enabled ?
                    PivotChart :
                    PivotTable);
            var pivottable = pivotTableFactory({
                pgridwidget: self
            });

            pivotComponent = ReactDOM.render(pivottable, renderElement);
        }
    };

    this.drilldown = function(dataCell, pivotId) {
        if(dataCell) {
            var colIndexes = dataCell.columnDimension.getRowIndexes();
            var data = dataCell.rowDimension.getRowIndexes().filter(function(index) {
                return colIndexes.indexOf(index) >= 0;
            }).map(function(index) {
                return self.pgrid.filteredDataSource[index];
            });

            var title;
            if(dataCell.rowType === uiheaders.HeaderType.GRAND_TOTAL && dataCell.colType === uiheaders.HeaderType.GRAND_TOTAL) {
                title = 'Grand total';
            } else {
                if(dataCell.rowType === uiheaders.HeaderType.GRAND_TOTAL) {
                    title = dataCell.columnDimension.value + '/Grand total ';
                } else if(dataCell.colType === uiheaders.HeaderType.GRAND_TOTAL) {
                    title = dataCell.rowDimension.value + '/Grand total ';
                } else {
                    title = dataCell.rowDimension.value + '/' + dataCell.columnDimension.value;
                }
            }

            dialog.show({
                title: title,
                comp: {
                    type: Grid,
                    props: {                    
                        headers: self.pgrid.config.getDataSourceFieldCaptions(),
                        data: data,
                        theme: self.pgrid.config.theme
                    }
                },
                theme: self.pgrid.config.theme,
                style: pivotComponent.fontStyle
            });
        }
    };
    
    function init() {
        self.pgrid.subscribe(pgrid.EVENT_UPDATED, buildUiAndRender);
        self.pgrid.subscribe(pgrid.EVENT_SORT_CHANGED, buildUiAndRender);
        self.pgrid.subscribe(pgrid.EVENT_CONFIG_CHANGED, buildUiAndRender);
        
        buildUi();  
    }

    function buildUi() {

        // build row and column headers
        self.rows = new uirows(self.pgrid.rows);
        self.columns = new uicols(self.pgrid.columns);

        var rowsHeaders = self.rows.headers;
        var columnsLeafHeaders = self.columns.leafsHeaders;

        // set control layout infos		
        self.layout = {
            rowHeaders: {
                width: (self.pgrid.rows.fields.length || 1) +
                       (self.pgrid.config.dataHeadersLocation === 'rows' && self.pgrid.config.dataFieldsCount > 1 ? 1 : 0),
                height: rowsHeaders.length
            },
            columnHeaders: {
                width: self.columns.leafsHeaders.length,
                height: (self.pgrid.columns.fields.length || 1) +
                        (self.pgrid.config.dataHeadersLocation === 'columns' && self.pgrid.config.dataFieldsCount > 1 ? 1 : 0)
            }
        };

        self.layout.pivotTable = {
            width: self.layout.rowHeaders.width + self.layout.columnHeaders.width,
            height: self.layout.rowHeaders.height + self.layout.columnHeaders.height
        };

        var dataRows = [];
        var arr;

        function createVisibleFunc(rowvisible, colvisible) {
            return function() {
                return rowvisible() && colvisible();
            };
        }
        if(rowsHeaders.length > 0) {
            for (var ri = 0; ri < rowsHeaders.length; ri++) {
                var rowHeadersRow = rowsHeaders[ri];
                var rowLeafHeader = rowHeadersRow[rowHeadersRow.length - 1];

                arr = [];
                for (var colHeaderIndex = 0; colHeaderIndex < columnsLeafHeaders.length; colHeaderIndex++) {
                    var columnLeafHeader = columnsLeafHeaders[colHeaderIndex];
                    var isvisible = createVisibleFunc(rowLeafHeader.visible, columnLeafHeader.visible);
                    arr[colHeaderIndex] = new uiheaders.dataCell(self.pgrid, isvisible, rowLeafHeader, columnLeafHeader);
                }
                dataRows.push(arr);
            }
        }
        self.dataRows = dataRows;
    }
    
    function buildUiAndRender() {
        buildUi();
        self.render();
    }
    
    init();
};
