/** @jsx React.DOM */

/* global module, require, React, window */
/*jshint node: true*/

'use strict';

var react = typeof window === 'undefined' ? require('react') : window.React;
var utils = require('../orb.utils');
var axe = require('../orb.axe');
var uiheaders = require('../orb.ui.header');
var filtering = require('../orb.filtering');
var reactUtils = require('./orb.react.utils');

var extraCol = 0;
var comps = module.exports;

/** @jsx React.DOM */

/* global module, require, React */

'use strict';

var pivotId = 1;
var themeChangeCallbacks = {};

module.exports.PivotTable = react.createClass({
    id: pivotId++,
    pgrid: null,
    pgridwidget: null,
    getInitialState: function() {
        comps.DragManager.init(this);

        themeChangeCallbacks[this.id] = [];
        this.registerThemeChanged(this.updateClasses);

        this.pgridwidget = this.props.pgridwidget;
        this.pgrid = this.pgridwidget.pgrid;
        return {};
    },
    sort: function(axetype, field) {
        this.pgridwidget.sort(axetype, field);
        this.setProps({});
    },
    moveButton: function(button, newAxeType, position) {
        if (this.pgridwidget.moveField(button.props.field.name, button.props.axetype, newAxeType, position)) {
            this.setProps({});
        }
    },
    toggleFieldExpansion: function(axetype, field, newState) {
        if (this.pgridwidget.toggleFieldExpansion(axetype, field, newState)) {
            this.setProps({});
        }
    },
    toggleSubtotals: function(axetype) {
        if (this.pgridwidget.toggleSubtotals(axetype)) {
            this.setProps({});
        }
    },
    toggleGrandtotal: function(axetype) {
        if (this.pgridwidget.toggleGrandtotal(axetype)) {
            this.setProps({});
        }
    },
    expandRow: function(cell) {
        cell.expand();
        this.setProps({});
    },
    collapseRow: function(cell) {
        cell.subtotalHeader.collapse();
        this.setProps({});
    },
    applyFilter: function(fieldname, operator, term, staticValue, excludeStatic) {
        this.pgridwidget.applyFilter(fieldname, operator, term, staticValue, excludeStatic);
        this.setProps({});
    },
    registerThemeChanged: function(compCallback) {
        if (compCallback) {
            themeChangeCallbacks[this.id].push(compCallback);
        }
    },
    unregisterThemeChanged: function(compCallback) {
        var i;
        if (compCallback && (i = themeChangeCallbacks[this.id].indexOf(compCallback)) >= 0) {
            themeChangeCallbacks[this.id].splice(i, 1);
        }
    },
    changeTheme: function(newTheme) {
        if (this.pgridwidget.pgrid.config.setTheme(newTheme)) {
            // notify self/sub-components of the theme change
            for (var i = 0; i < themeChangeCallbacks[this.id].length; i++) {
                themeChangeCallbacks[this.id][i]();
            }
        }
    },
    updateClasses: function() {
        var thisnode = this.getDOMNode();
        var classes = this.pgridwidget.pgrid.config.theme.getPivotClasses();
        thisnode.className = classes.container;
        thisnode.children[1].className = classes.table;
    },
    componentDidUpdate: function() {
        this.synchronizeCompsWidths();
    },
    componentDidMount: function() {
        var dataCellsContainerNode = this.refs.dataCellsContainer.getDOMNode();
        var dataCellsTableNode = this.refs.dataCellsTable.getDOMNode();
        var colHeadersContainerNode = this.refs.colHeadersContainer.getDOMNode();
        var rowHeadersContainerNode = this.refs.rowHeadersContainer.getDOMNode();

        this.refs.horizontalScrollBar.setScrollClient(dataCellsContainerNode, function(scrollPercent) {
            var scrollAmount = Math.ceil(
                scrollPercent * (
                    reactUtils.getSize(dataCellsTableNode).width -
                    reactUtils.getSize(dataCellsContainerNode).width
                )
            );
            colHeadersContainerNode.scrollLeft = scrollAmount;
            dataCellsContainerNode.scrollLeft = scrollAmount;
        });

        this.refs.verticalScrollBar.setScrollClient(dataCellsContainerNode, function(scrollPercent) {
            var scrollAmount = Math.ceil(
                scrollPercent * (
                    reactUtils.getSize(dataCellsTableNode).height -
                    reactUtils.getSize(dataCellsContainerNode).height
                )
            );
            rowHeadersContainerNode.scrollTop = scrollAmount;
            dataCellsContainerNode.scrollTop = scrollAmount;
        });

        this.synchronizeCompsWidths();
    },
    onWheel: function(e) {
        var elem;
        var scrollbar;
        var amount;

        if (e.currentTarget == (elem = this.refs.colHeadersContainer.getDOMNode())) {
            scrollbar = this.refs.horizontalScrollBar;
            amount = e.deltaX || e.deltaY;
        } else if ((e.currentTarget == (elem = this.refs.rowHeadersContainer.getDOMNode())) ||
            (e.currentTarget == (elem = this.refs.dataCellsContainer.getDOMNode()))) {
            scrollbar = this.refs.verticalScrollBar;
            amount = e.deltaY;
        }

        if (scrollbar && scrollbar.scroll(amount, e.deltaMode)) {
            e.stopPropagation();
            e.preventDefault();
        }
    },
    synchronizeCompsWidths: function() {
        var self = this;

        var pivotWrapperTable = self.refs.pivotWrapperTable.getDOMNode();

        var nodes = (function() {
            var nds = {};
            ['pivotContainer', 'dataCellsContainer', 'dataCellsTable', 'upperbuttonsRow', 'columnbuttonsRow',
                /*'colHeadersTable',*/
                'colHeadersContainer', /*'rowHeadersTable',*/ 'rowHeadersContainer', 'rowButtonsContainer',
                'toolbar', 'horizontalScrollBar', 'verticalScrollBar'
            ].forEach(function(refname) {
                if (self.refs[refname]) {
                    nds[refname] = {
                        node: self.refs[refname].getDOMNode()
                    };
                    nds[refname].size = reactUtils.getSize(nds[refname].node);
                }
            });
            return nds;
        }());

        // colHeadersTable
        nodes.colHeadersTable = {
            node: nodes.colHeadersContainer.node.children[0]
        };
        nodes.colHeadersTable.size = reactUtils.getSize(nodes.colHeadersTable.node);

        // rowHeadersTable
        nodes.rowHeadersTable = {
            node: nodes.rowHeadersContainer.node.children[0]
        };
        nodes.rowHeadersTable.size = reactUtils.getSize(nodes.rowHeadersTable.node);

        // get row buttons container width
        //nodes.rowButtonsContainer.node.style.width = '';
        var rowButtonsContainerWidth = reactUtils.getSize(nodes.rowButtonsContainer.node.children[0]).width;

        // get array of dataCellsTable column widths
        getAllColumnsWidth(nodes.dataCellsTable);
        // get array of colHeadersTable column widths
        getAllColumnsWidth(nodes.colHeadersTable);
        // get array of rowHeadersTable column widths
        getAllColumnsWidth(nodes.rowHeadersTable);

        // get the array of max widths between dataCellsTable and colHeadersTable
        var dataCellsTableMaxWidthArray = [];
        var dataCellsTableMaxWidth = 0;

        for (var i = 0; i < nodes.dataCellsTable.widthArray.length; i++) {
            var mxwidth = Math.max(nodes.dataCellsTable.widthArray[i], nodes.colHeadersTable.widthArray[i]);
            dataCellsTableMaxWidthArray.push(mxwidth);
            dataCellsTableMaxWidth += mxwidth;
        }

        var rowHeadersTableWidth = Math.max(nodes.rowHeadersTable.size.width, rowButtonsContainerWidth, 67);
        var rowDiff = rowHeadersTableWidth - nodes.rowHeadersTable.size.width;
        if (rowDiff > 0) {
            nodes.rowHeadersTable.size.width += rowDiff;
            nodes.rowHeadersTable.widthArray[nodes.rowHeadersTable.widthArray.length - 1] += rowDiff;
        }

        //nodes.rowButtonsContainer.node.style.width = (rowHeadersTableWidth + 1) + 'px';
        //nodes.rowButtonsContainer.node.style.paddingRight = (rowHeadersTableWidth + 1 - rowButtonsContainerWidth + 17) + 'px';

        // Set dataCellsTable cells widths according to the computed dataCellsTableMaxWidthArray
        reactUtils.updateTableColGroup(nodes.dataCellsTable.node, dataCellsTableMaxWidthArray);

        // Set colHeadersTable cells widths according to the computed dataCellsTableMaxWidthArray
        reactUtils.updateTableColGroup(nodes.colHeadersTable.node, dataCellsTableMaxWidthArray);

        // Set rowHeadersTable cells widths
        reactUtils.updateTableColGroup(nodes.rowHeadersTable.node, nodes.rowHeadersTable.widthArray);

        nodes.dataCellsTable.node.style.width = dataCellsTableMaxWidth + 'px';
        nodes.colHeadersTable.node.style.width = dataCellsTableMaxWidth + 'px';
        nodes.rowHeadersTable.node.style.width = rowHeadersTableWidth + 'px';

        var dataCellsContainerWidth = Math.min(
            dataCellsTableMaxWidth + 1,
            nodes.pivotContainer.size.width - rowHeadersTableWidth - nodes.verticalScrollBar.size.width);

        // Adjust data cells container width
        nodes.dataCellsContainer.node.style.width = dataCellsContainerWidth + 'px';
        nodes.colHeadersContainer.node.style.width = dataCellsContainerWidth + 'px';

        var pivotContainerHeight = this.pgridwidget.pgrid.config.height;

        if (pivotContainerHeight) {
            // Adjust data cells container height
            var dataCellsTableHeight = Math.ceil(Math.min(
                pivotContainerHeight -
                (nodes.toolbar ? nodes.toolbar.size.height + 17 : 0) -
                nodes.upperbuttonsRow.size.height -
                nodes.columnbuttonsRow.size.height -
                nodes.colHeadersTable.size.height -
                nodes.horizontalScrollBar.size.height,
                nodes.dataCellsTable.size.height));

            nodes.dataCellsContainer.node.style.height = dataCellsTableHeight + 'px';
            nodes.rowHeadersContainer.node.style.height = dataCellsTableHeight + 'px';
        }

        reactUtils.updateTableColGroup(
            pivotWrapperTable, [
                rowHeadersTableWidth,
                dataCellsContainerWidth,
                nodes.verticalScrollBar.size.width,
                Math.max(
                    nodes.pivotContainer.size.width - (
                        rowHeadersTableWidth +
                        dataCellsContainerWidth +
                        nodes.verticalScrollBar.size.width),
                    0)
            ]);

        this.refs.horizontalScrollBar.refresh();
        this.refs.verticalScrollBar.refresh();
    },
    render: function() {

        var self = this;

        var config = this.pgridwidget.pgrid.config;
        var Toolbar = comps.Toolbar;
        var PivotTableUpperButtons = comps.PivotTableUpperButtons;
        var PivotTableColumnButtons = comps.PivotTableColumnButtons;
        var PivotTableRowButtons = comps.PivotTableRowButtons;
        var PivotTableRowHeaders = comps.PivotTableRowHeaders;
        var PivotTableColumnHeaders = comps.PivotTableColumnHeaders;
        var PivotTableDataCells = comps.PivotTableDataCells;
        var HorizontalScrollBar = comps.HorizontalScrollBar;
        var VerticalScrollBar = comps.VerticalScrollBar;

        var classes = config.theme.getPivotClasses();

        var tblStyle = {};
        if (config.width) {
            tblStyle.width = config.width;
        }
        if (config.height) {
            tblStyle.height = config.height;
        }

        return (
            React.createElement("div", {
                    className: classes.container,
                    style: tblStyle,
                    ref: "pivotContainer"
                },
                config.toolbar && config.toolbar.visible ? React.createElement("div", {
                        ref: "toolbar",
                        className: "orb-toolbar"
                    },
                    React.createElement(Toolbar, {
                        pivotTableComp: self
                    })
                ) : null,
                React.createElement("table", {
                        id: 'tbl-' + self.id,
                        ref: "pivotWrapperTable",
                        className: classes.table,
                        style: {
                            tableLayout: 'fixed'
                        }
                    },
                    React.createElement("colgroup", null,
                        React.createElement("col", {
                            ref: "column1"
                        }),
                        React.createElement("col", {
                            ref: "column2"
                        }),
                        React.createElement("col", {
                            ref: "column3"
                        }),
                        React.createElement("col", {
                            ref: "column4"
                        })
                    ),
                    React.createElement("tbody", null,
                        React.createElement("tr", {
                                ref: "upperbuttonsRow"
                            },
                            React.createElement("td", {
                                    colSpan: "4"
                                },
                                React.createElement(PivotTableUpperButtons, {
                                    pivotTableComp: self
                                })
                            )
                        ),
                        React.createElement("tr", {
                                ref: "columnbuttonsRow"
                            },
                            React.createElement("td", null),
                            React.createElement("td", {
                                    style: {
                                        padding: '11px 4px !important'
                                    }
                                },
                                React.createElement(PivotTableColumnButtons, {
                                    pivotTableComp: self
                                })
                            ),
                            React.createElement("td", {
                                colSpan: "2"
                            })
                        ),
                        React.createElement("tr", null,
                            React.createElement("td", {
                                    style: {
                                        position: 'relative'
                                    }
                                },
                                React.createElement(PivotTableRowButtons, {
                                    pivotTableComp: self,
                                    ref: "rowButtonsContainer"
                                })
                            ),
                            React.createElement("td", null,
                                React.createElement(PivotTableColumnHeaders, {
                                    pivotTableComp: self,
                                    ref: "colHeadersContainer"
                                })
                            ),
                            React.createElement("td", {
                                colSpan: "2"
                            })
                        ),
                        React.createElement("tr", null,
                            React.createElement("td", null,
                                React.createElement(PivotTableRowHeaders, {
                                    pivotTableComp: self,
                                    ref: "rowHeadersContainer"
                                })
                            ),
                            React.createElement("td", null,
                                React.createElement("div", {
                                        className: "inner-table-container data-cntr",
                                        ref: "dataCellsContainer",
                                        onWheel: this.onWheel
                                    },
                                    React.createElement(PivotTableDataCells, {
                                        pivotTableComp: self,
                                        ref: "dataCellsTable"
                                    })
                                )
                            ),
                            React.createElement("td", null,
                                React.createElement(VerticalScrollBar, {
                                    pivotTableComp: self,
                                    ref: "verticalScrollBar"
                                })
                            ),
                            React.createElement("td", null)
                        ),
                        React.createElement("tr", null,
                            React.createElement("td", null),
                            React.createElement("td", null,
                                React.createElement(HorizontalScrollBar, {
                                    pivotTableComp: self,
                                    ref: "horizontalScrollBar"
                                })
                            ),
                            React.createElement("td", {
                                colSpan: "2"
                            })
                        )
                    )
                ),
                React.createElement("div", {
                    className: "orb-overlay orb-overlay-hidden",
                    id: 'drilldialog' + self.id
                })
            )
        );
    }
});

/**
 * Gets the width of all columns (maximum width of all column cells) of a html table element
 * @param  {Object}  tblObject - object having a table element in its 'node' property
 * @returns {Array} An array of numeric values representing the width of each column.
 *                  Its length is equal to the greatest number of cells of all rows
 *                  (in case of cells having colSpan/rowSpan greater than 1.)
 */
function getAllColumnsWidth(tblObject) {
    if (tblObject && tblObject.node) {

        var tbl = tblObject.node;
        var widthArray = [];

        for (var rowIndex = 0; rowIndex < tbl.rows.length; rowIndex++) {
            // current row
            var currRow = tbl.rows[rowIndex];
            // reset widthArray index
            var arrayIndex = 0;
            var currWidth = null;

            // get the width of each cell within current row
            for (var cellIndex = 0; cellIndex < currRow.cells.length; cellIndex++) {
                // current cell
                var currCell = currRow.cells[cellIndex];

                if (currCell.__orb._visible) {
                    // cell width
                    //var cellwidth = Math.ceil(reactUtils.getSize(currCell.children[0]).width/currCell.colSpan);
                    var cellwidth = Math.ceil((currCell.__orb._textWidth / currCell.__orb._colSpan) + currCell.__orb._paddingLeft + currCell.__orb._paddingRight + currCell.__orb._borderLeftWidth + currCell.__orb._borderRightWidth);
                    // whether current cell spans vertically to the last row
                    var rowsSpan = currCell.__orb._rowSpan > 1 && currCell.__orb._rowSpan >= tbl.rows.length - rowIndex;

                    // if current cell spans over more than one column, add its width (its) 'colSpan' number of times
                    for (var cspan = 0; cspan < currCell.__orb._colSpan; cspan++) {
                        // If cell span over more than 1 row: insert its width into widthArray at arrayIndex
                        // Else: either expand widthArray if necessary or replace the width if its smaller than current cell width

                        currWidth = widthArray[arrayIndex];
                        // skip inhibited widths (width that belongs to an upper cell than spans vertically to current row)
                        while (currWidth && currWidth.inhibit > 0) {
                            currWidth.inhibit--;
                            arrayIndex++;
                            currWidth = widthArray[arrayIndex];
                        }

                        if (widthArray.length - 1 < arrayIndex) {
                            widthArray.push({
                                width: cellwidth
                            });
                        } else if (cellwidth > widthArray[arrayIndex].width) {
                            widthArray[arrayIndex].width = cellwidth;
                        }

                        widthArray[arrayIndex].inhibit = currCell.__orb._rowSpan - 1;

                        // increment widthArray index
                        arrayIndex++;
                    }
                }
            }

            // decrement inhibited state of all widths unsed in widthArray (not reached by current row cells)
            currWidth = widthArray[arrayIndex];
            while (currWidth) {
                if (currWidth.inhibit > 0) {
                    currWidth.inhibit--;
                }
                arrayIndex++;
                currWidth = widthArray[arrayIndex];
            }
        }

        // set widthArray to the tblObject
        tblObject.size.width = 0;
        tblObject.widthArray = widthArray.map(function(item, index) {
            tblObject.size.width += item.width;
            return item.width;
        });
    }
}

/**
 * Sets the width of all cells of a html table element
 * @param  {Object}  tblObject - object having a table element in its 'node' property
 * @param  {Array}  newWidthArray - an array of numeric values representing the width of each individual cell.
 *                                  Its length is equal to the greatest number of cells of all rows
 *                                  (in case of cells having colSpan/rowSpan greater than 1.)
 */
function setTableWidths(tblObject, newWidthArray) {
    if (tblObject && tblObject.node) {

        // reset table width
        (tblObject.size = (tblObject.size || {})).width = 0;

        var tbl = tblObject.node;

        // for each row, set its cells width
        for (var rowIndex = 0; rowIndex < tbl.rows.length; rowIndex++) {

            // current row
            var currRow = tbl.rows[rowIndex];
            // index in newWidthArray
            var arrayIndex = 0;
            var currWidth = null;

            // set width of each cell
            for (var cellIndex = 0; cellIndex < currRow.cells.length; cellIndex++) {

                // current cell
                var currCell = currRow.cells[cellIndex];
                if (currCell.__orb._visible) {
                    // cell width
                    var newCellWidth = 0;
                    // whether current cell spans vertically more than 1 row
                    var rowsSpan = currCell.__orb._rowSpan > 1 && rowIndex < tbl.rows.length - 1;

                    // current cell width is the sum of (its) "colspan" items in newWidthArray starting at 'arrayIndex'
                    // 'arrayIndex' should be incremented by an amount equal to current cell 'colspan' but should also skip 'inhibited' cells
                    for (var cspan = 0; cspan < currCell.__orb._colSpan; cspan++) {
                        currWidth = newWidthArray[arrayIndex];
                        // skip inhibited widths (width that belongs to an upper cell than spans vertically to current row)
                        while (currWidth && currWidth.inhibit > 0) {
                            currWidth.inhibit--;
                            arrayIndex++;
                            currWidth = newWidthArray[arrayIndex];
                        }

                        if (currWidth) {
                            // add width of cells participating in the span
                            newCellWidth += currWidth.width;
                            // if current cell spans vertically more than 1 row, mark its width as inhibited for all cells participating in this span
                            if (rowsSpan) {
                                currWidth.inhibit = currCell.__orb._rowSpan - 1;
                            }

                            // advance newWidthArray index
                            arrayIndex++;
                        }
                    }

                    currCell.children[0].style.width = newCellWidth + 'px';

                    // set table width (only in first iteration)
                    if (rowIndex === 0) {
                        var outerCellWidth = 0;
                        if (currCell.__orb) {
                            outerCellWidth = currCell.__orb._colSpan * (Math.ceil(currCell.__orb._paddingLeft + currCell.__orb._paddingRight + currCell.__orb._borderLeftWidth + currCell.__orb._borderRightWidth));
                        }
                        tblObject.size.width += newCellWidth + outerCellWidth;
                    }
                }
            }

            // decrement inhibited state of all widths unsed in newWidthArray (not reached by current row cells)
            currWidth = newWidthArray[arrayIndex];
            while (currWidth) {
                if (currWidth.inhibit > 0) {
                    currWidth.inhibit--;
                }
                arrayIndex++;
                currWidth = newWidthArray[arrayIndex];
            }
        }
    }
}

function clearTableWidths(tbl) {
        if (tbl) {
            for (var rowIndex = 0; rowIndex < tbl.rows.length; rowIndex++) {
                var row = tbl.rows[rowIndex];
                for (var cellIndex = 0; cellIndex < row.cells.length; cellIndex++) {
                    row.cells[cellIndex].children[0].style.width = '';
                }
            }
            tbl.style.width = '';
        }
    }
    /** @jsx React.DOM */

/* global module, require, React */

'use strict';


module.exports.PivotRow = react.createClass({
    render: function() {
        var self = this;
        var PivotCell = comps.PivotCell;

        var lastCellIndex = this.props.row.length - 1;
        var cell0 = this.props.row[0];
        var leftmostCellFound = false;
        var layoutInfos = self.props.layoutInfos;
        var cells;

        var rowstyle = {};

        var istopmost = false;

        cells = this.props.row.map(function(cell, index) {

            var isleftmost = false;

            // If current cells are column/data headers and left most cell is not found yet
            // and last row left most cell does not span vertically over the current one and current one is visible 
            // then mark IT as the left most cell
            if (cell.visible() && layoutInfos) {
                if (cell.dim) {
                    if ((cell.dim.isRoot && layoutInfos.topMostCells[cell.dim.depth - 1] === undefined) || (!cell.dim.isRoot && layoutInfos.topMostCells[cell.dim.depth] === undefined && (cell.dim.parent.isRoot || layoutInfos.topMostCells[cell.dim.depth + 1] === cell.dim.parent))) {
                        istopmost = true;
                        layoutInfos.topMostCells[cell.dim.depth] = cell.dim;
                    }
                } else if (!layoutInfos.topMostCells['0']) {
                    istopmost = layoutInfos.topMostCells['0'] = true;
                }

                if (!leftmostCellFound && (self.props.axetype === axe.Type.DATA || self.props.axetype === axe.Type.COLUMNS) &&
                    layoutInfos.lastLeftMostCellVSpan === 0) {

                    isleftmost = leftmostCellFound = true;
                    layoutInfos.lastLeftMostCellVSpan = cell.vspan() - 1;
                }
            }

            return React.createElement(PivotCell, {
                key: index,
                cell: cell,
                leftmost: isleftmost,
                topmost: istopmost,
                pivotTableComp: self.props.pivotTableComp
            });
        });

        // decrement lastLeftMostCellVSpan
        if (layoutInfos && layoutInfos.lastLeftMostCellVSpan > 0 && !leftmostCellFound) {
            layoutInfos.lastLeftMostCellVSpan--;
        }

        return (
            React.createElement("tr", {
                    style: rowstyle
                },
                cells
            )
        );
    }
});
/** @jsx React.DOM */

/* global module, require, React */
/*jshint eqnull: true*/

'use strict';

var _paddingLeft = null;
var _borderLeft = null;

module.exports.PivotCell = react.createClass({
    expand: function() {
        this.props.pivotTableComp.expandRow(this.props.cell);
    },
    collapse: function() {
        this.props.pivotTableComp.collapseRow(this.props.cell);
    },
    updateCellInfos: function() {
        var node = this.getDOMNode();
        var cell = this.props.cell;
        node.__orb = node.__orb || {};

        if (!cell.visible()) {

            node.__orb._visible = false;

        } else {
            var cellContentNode = this.refs.cellContent.getDOMNode();

            var text = node.textContent;
            var propList = [];
            var retPaddingLeft = _paddingLeft == null;
            var retBorderLeft = !this.props.leftmost && _borderLeft == null;

            if (retPaddingLeft) {
                propList.push('padding-left');
            }

            if (retBorderLeft) {
                propList.push('border-left-width');
            }

            if (propList.length > 0) {
                var nodeStyle = reactUtils.getStyle(node, propList, true);

                if (retPaddingLeft) {
                    _paddingLeft = parseFloat(nodeStyle[0]);
                }

                if (retBorderLeft) {
                    _borderLeft = parseFloat(nodeStyle[retPaddingLeft ? 1 : 0]);
                }
            }

            reactUtils.removeClass(node, 'cell-hidden');

            node.__orb._visible = true;
            node.__orb._textWidth = reactUtils.getSize(cellContentNode).width;
            node.__orb._colSpan = this.props.cell.hspan(true) || 1;
            node.__orb._rowSpan = this.props.cell.vspan(true) || 1;
            node.__orb._paddingLeft = _paddingLeft;
            node.__orb._paddingRight = _paddingLeft;
            node.__orb._borderLeftWidth = this.props.leftmost ? 0 : _borderLeft;
            node.__orb._borderRightWidth = 0;
        }
    },
    componentDidMount: function() {
        this.updateCellInfos();
    },
    componentDidUpdate: function() {
        this.updateCellInfos();
    },
    shouldComponentUpdate: function(nextProps, nextState) {
        if (nextProps.cell && nextProps.cell == this.props.cell && !this._latestVisibleState && !nextProps.cell.visible()) {
            return false;
        }
        return true;
    },
    _latestVisibleState: false,
    render: function() {
        var self = this;
        var cell = this.props.cell;
        var divcontent = [];
        var value;
        var cellClick;
        var headerPushed = false;

        this._latestVisibleState = cell.visible();

        switch (cell.template) {
            case 'cell-template-row-header':
            case 'cell-template-column-header':
                var isWrapper = cell.type === uiheaders.HeaderType.WRAPPER && cell.dim.field.subTotal.visible && cell.dim.field.subTotal.collapsible;
                var isSubtotal = cell.type === uiheaders.HeaderType.SUB_TOTAL && !cell.expanded;
                if (isWrapper || isSubtotal) {
                    headerPushed = true;

                    divcontent.push(React.createElement("table", {
                            key: "header-value",
                            ref: "cellContent"
                        },
                        React.createElement("tbody", null,
                            React.createElement("tr", null, React.createElement("td", {
                                    className: "orb-tgl-btn"
                                }, React.createElement("div", {
                                    className: 'orb-tgl-btn-' + (isWrapper ? 'down' : 'right'),
                                    onClick: (isWrapper ? this.collapse : this.expand)
                                })),
                                React.createElement("td", {
                                    className: "hdr-val"
                                }, React.createElement("div", {
                                    dangerouslySetInnerHTML: {
                                        __html: cell.value || '&#160;'
                                    }
                                })))
                        )));
                } else {
                    value = (cell.value || '&#160;') + (cell.type === uiheaders.HeaderType.SUB_TOTAL ? ' Total' : '');
                }
                break;
            case 'cell-template-dataheader':
                value = cell.value.caption;
                break;
            case 'cell-template-datavalue':
                value = (cell.datafield && cell.datafield.formatFunc) ? cell.datafield.formatFunc()(cell.value) : cell.value;
                cellClick = function() {
                    self.props.pivotTableComp.pgridwidget.drilldown(cell, self.props.pivotTableComp.id);
                };
                break;
            default:
                break;
        }

        if (!headerPushed) {
            var headerClassName;
            switch (cell.template) {
                case 'cell-template-datavalue':
                    headerClassName = 'cell-data';
                    break;
                default:
                    if (cell.template != 'cell-template-dataheader' && cell.type !== uiheaders.HeaderType.GRAND_TOTAL) {
                        headerClassName = 'hdr-val';
                    }
            }
            divcontent.push(React.createElement("div", {
                key: "cell-value",
                ref: "cellContent",
                className: headerClassName
            }, React.createElement("div", {
                dangerouslySetInnerHTML: {
                    __html: value || '&#160;'
                }
            })));
        }

        return React.createElement("td", {
                className: getClassname(this.props),
                onDoubleClick: cellClick,
                colSpan: cell.hspan(),
                rowSpan: cell.vspan()
            },
            React.createElement("div", null,
                divcontent
            )
        );
    }
});

function getClassname(compProps) {
        var cell = compProps.cell;
        var classname = cell.cssclass;
        var isEmpty = cell.template === 'cell-template-empty';

        if (!cell.visible()) {
            classname += ' cell-hidden';
        }

        if (cell.type === uiheaders.HeaderType.SUB_TOTAL && cell.expanded) {
            classname += ' header-st-exp';
        }

        if (cell.type === uiheaders.HeaderType.GRAND_TOTAL) {
            if (cell.dim.depth === 1) {
                classname += ' header-nofields';
            } else if (cell.dim.depth > 2) {
                classname += ' header-gt-exp';
            }
        }

        if (compProps.leftmost) {
            classname += ' ' + (cell.template === 'cell-template-datavalue' ? 'cell' : 'header') + '-leftmost';
        }

        if (compProps.topmost) {
            classname += ' cell-topmost';
        }

        return classname;
    }
    /* global module, require, react, reactUtils */
    /*jshint eqnull: true*/

'use strict';

var dragManager = module.exports.DragManager = (function() {

    var _pivotComp = null;

    var _currDragElement = null;
    var _currDropTarget = null;
    var _currDropIndicator = null;

    var _dragNode = null;
    var _dropTargets = [];
    var _dropIndicators = [];

    function doElementsOverlap(elem1Rect, elem2Rect) {
        return !(elem1Rect.right < elem2Rect.left ||
            elem1Rect.left > elem2Rect.right ||
            elem1Rect.bottom < elem2Rect.top ||
            elem1Rect.top > elem2Rect.bottom);
    }

    function setCurrDropTarget(dropTarget, callback) {
        if (_currDropTarget) {
            signalDragEnd(_currDropTarget, function() {
                _currDropTarget = dropTarget;
                signalDragOver(dropTarget, callback);
            });
        } else {
            _currDropTarget = dropTarget;
            signalDragOver(dropTarget, callback);
        }
    }

    function setCurrDropIndicator(dropIndicator) {
        if (_currDropIndicator) {
            signalDragEnd(_currDropIndicator, function() {
                _currDropIndicator = dropIndicator;
                signalDragOver(dropIndicator);
            });
        } else {
            _currDropIndicator = dropIndicator;
            signalDragOver(dropIndicator);
        }
    }

    function signalDragOver(target, callback) {
        if (target && target.onDragOver) {
            target.onDragOver(callback);
        } else if (callback) {
            callback();
        }
    }

    function signalDragEnd(target, callback) {
        if (target && target.onDragEnd) {
            target.onDragEnd(callback);
        } else if (callback) {
            callback();
        }
    }

    function getDropTarget() {
        return reactUtils.forEach(_dropTargets, function(target) {
            if (target.component.state.isover) {
                return target;
            }
        }, true);
    }

    function getDropIndicator() {
        return reactUtils.forEach(_dropIndicators, function(indicator) {
            if (indicator.component.state.isover) {
                return indicator;
            }
        }, true);
    }

    var _initialized = false;

    return {
        init: function(pivotComp) {
            _initialized = true;
            _pivotComp = pivotComp;
        },
        setDragElement: function(elem) {

            var prevDragElement = _currDragElement;
            _currDragElement = elem;
            if (_currDragElement != prevDragElement) {
                if (elem == null) {

                    if (_currDropTarget) {
                        var position = _currDropIndicator != null ? _currDropIndicator.position : null;
                        _pivotComp.moveButton(prevDragElement, _currDropTarget.component.props.axetype, position);
                    }

                    _dragNode = null;
                    setCurrDropTarget(null);
                    setCurrDropIndicator(null);

                } else {
                    _dragNode = _currDragElement.getDOMNode();
                }
            }
        },
        registerTarget: function(target, axetype, dragOverHandler, dargEndHandler) {
            _dropTargets.push({
                component: target,
                axetype: axetype,
                onDragOver: dragOverHandler,
                onDragEnd: dargEndHandler
            });
        },
        unregisterTarget: function(target) {
            var tindex;
            for (var i = 0; i < _dropTargets.length; i++) {
                if (_dropTargets[i].component == target) {
                    tindex = i;
                    break;
                }
            }
            if (tindex != null) {
                _dropTargets.splice(tindex, 1);
            }
        },
        registerIndicator: function(indicator, axetype, position, dragOverHandler, dargEndHandler) {
            _dropIndicators.push({
                component: indicator,
                axetype: axetype,
                position: position,
                onDragOver: dragOverHandler,
                onDragEnd: dargEndHandler
            });
        },
        unregisterIndicator: function(indicator) {
            var iindex;
            for (var i = 0; i < _dropIndicators.length; i++) {
                if (_dropIndicators[i].component == indicator) {
                    iindex = i;
                    break;
                }
            }
            if (iindex != null) {
                _dropIndicators.splice(iindex, 1);
            }
        },
        elementMoved: function() {
            if (_currDragElement != null) {
                var dragNodeRect = _dragNode.getBoundingClientRect();
                var foundTarget;

                reactUtils.forEach(_dropTargets, function(target) {
                    if (!foundTarget) {
                        var tnodeRect = target.component.getDOMNode().getBoundingClientRect();
                        var isOverlap = doElementsOverlap(dragNodeRect, tnodeRect);
                        if (isOverlap) {
                            foundTarget = target;
                            return;
                        }
                    }
                }, true);

                if (foundTarget) {
                    setCurrDropTarget(foundTarget, function() {
                        var foundIndicator = null;

                        reactUtils.forEach(_dropIndicators, function(indicator, index) {
                            if (!foundIndicator) {
                                var elementOwnIndicator = indicator.component.props.axetype === _currDragElement.props.axetype &&
                                    indicator.component.props.position === _currDragElement.props.position;

                                var targetIndicator = indicator.component.props.axetype === foundTarget.component.props.axetype;
                                if (targetIndicator && !elementOwnIndicator) {
                                    var tnodeRect = indicator.component.getDOMNode().getBoundingClientRect();
                                    var isOverlap = doElementsOverlap(dragNodeRect, tnodeRect);
                                    if (isOverlap) {
                                        foundIndicator = indicator;
                                        return;
                                    }
                                }
                            }
                        });

                        if (!foundIndicator) {
                            var axeIndicators = _dropIndicators.filter(function(indicator) {
                                return indicator.component.props.axetype === foundTarget.component.props.axetype;
                            });
                            if (axeIndicators.length > 0) {
                                foundIndicator = axeIndicators[axeIndicators.length - 1];
                            }
                        }
                        setCurrDropIndicator(foundIndicator);
                    });
                }
            }
        }
    };
}());
/** @jsx React.DOM */

/* global module, require, react */
/*jshint eqnull: true*/

'use strict';


module.exports.DropIndicator = react.createClass({
    displayName: 'DropIndicator',
    getInitialState: function() {
        dragManager.registerIndicator(this, this.props.axetype, this.props.position, this.onDragOver, this.onDragEnd);
        return {
            isover: false
        };
    },
    componentWillUnmount: function() {
        dragManager.unregisterIndicator(this);
    },
    onDragOver: function(callback) {
        if (this.isMounted()) {
            this.setState({
                isover: true
            }, callback);
        } else if (callback) {
            callback();
        }
    },
    onDragEnd: function(callback) {
        if (this.isMounted()) {
            this.setState({
                isover: false
            }, callback);
        } else if (callback) {
            callback();
        }
    },
    render: function() {
        var classname = 'drp-indic';

        if (this.props.isFirst) {
            classname += ' drp-indic-first';
        }

        if (this.props.isLast) {
            classname += ' drp-indic-last';
        }

        var style = {};
        if (this.state.isover) {
            classname += ' drp-indic-over';
        }

        return React.createElement("div", {
            style: style,
            className: classname
        });
    }
});
/** @jsx React.DOM */

/* global module, require, react */
/*jshint eqnull: true*/

'use strict';

var dtid = 0;

module.exports.DropTarget = react.createClass({
    getInitialState: function() {
        this.dtid = ++dtid;
        return {
            isover: false
        };
    },
    componentDidMount: function() {
        dragManager.registerTarget(this, this.props.axetype, this.onDragOver, this.onDragEnd);
    },
    componentWillUnmount: function() {
        dragManager.unregisterTarget(this);
    },
    onDragOver: function(callback) {
        if (this.isMounted()) {
            this.setState({
                isover: true
            }, callback);
        } else if (callback) {
            callback();
        }
    },
    onDragEnd: function(callback) {
        if (this.isMounted()) {
            this.setState({
                isover: false
            }, callback);
        } else if (callback) {
            callback();
        }
    },
    render: function() {
        var self = this;
        var DropIndicator = module.exports.DropIndicator;

        var buttons = this.props.buttons.map(function(button, index) {
            if (index < self.props.buttons.length - 1) {
                return [
                    React.createElement("td", null, React.createElement(DropIndicator, {
                        isFirst: index === 0,
                        position: index,
                        axetype: self.props.axetype
                    })),
                    React.createElement("td", null, button)
                ];
            } else {
                return [
                    React.createElement("td", null, React.createElement(DropIndicator, {
                        isFirst: index === 0,
                        position: index,
                        axetype: self.props.axetype
                    })),
                    React.createElement("td", null, button),
                    React.createElement("td", null, React.createElement(DropIndicator, {
                        isLast: true,
                        position: null,
                        axetype: self.props.axetype
                    }))
                ];
            }
        });

        var style = self.props.axetype === axe.Type.ROWS ? {
            position: 'absolute',
            left: 0,
            bottom: 11
        } : null;

        return React.createElement("div", {
                className: 'drp-trgt' + (this.state.isover ? ' drp-trgt-over' : '') + (buttons.length === 0 ? ' drp-trgt-empty' : ''),
                style: style
            },
            React.createElement("table", null,
                React.createElement("tbody", null,
                    React.createElement("tr", null,
                        buttons
                    )
                )
            )
        );
    }
});
/** @jsx React.DOM */

/* global module, require, react */
/*jshint eqnull: true*/

'use strict';

var pbid = 0;

module.exports.PivotButton = react.createClass({
    displayName: 'PivotButton',
    getInitialState: function() {
        this.pbid = ++pbid;

        // initial state, all zero.
        return {
            pos: {
                x: 0,
                y: 0
            },
            startpos: {
                x: 0,
                y: 0
            },
            mousedown: false,
            dragging: false
        };
    },
    onFilterMouseDown: function(e) {
        // left mouse button only
        if (e.button !== 0) return;

        var filterButton = this.refs.filterButton.getDOMNode();
        var filterButtonPos = reactUtils.getOffset(filterButton);
        var filterContainer = document.createElement('div');

        var filterPanelFactory = React.createFactory(comps.FilterPanel);
        var filterPanel = filterPanelFactory({
            field: this.props.field.name,
            pivotTableComp: this.props.pivotTableComp
        });

        filterContainer.className = this.props.pivotTableComp.pgrid.config.theme.getFilterClasses().container;
        filterContainer.style.top = filterButtonPos.y + 'px';
        filterContainer.style.left = filterButtonPos.x + 'px';
        document.body.appendChild(filterContainer);

        React.render(filterPanel, filterContainer);

        // prevent event bubbling (to prevent text selection while dragging for example)
        e.stopPropagation();
        e.preventDefault();
    },
    componentDidUpdate: function() {
        if (this.props.pivotTableComp.pgrid.config.canMoveFields) {
            if (!this.state.mousedown) {
                // mouse not down, don't care about mouse up/move events.
                dragManager.setDragElement(null);
                document.removeEventListener('mousemove', this.onMouseMove);
            } else if (this.state.mousedown) {
                // mouse down, interested by mouse up/move events.
                dragManager.setDragElement(this);
                document.addEventListener('mousemove', this.onMouseMove);
            }
        }
    },
    componentDidMount: function() {
        this.props.pivotTableComp.registerThemeChanged(this.updateClasses);
    },
    componentWillUnmount: function() {
        this.props.pivotTableComp.unregisterThemeChanged(this.updateClasses);
        document.removeEventListener('mousemove', this.onMouseMove);
    },
    onMouseDown: function(e) {
        // drag/sort with left mouse button
        if (e.button !== 0) return;

        if (e.ctrlKey) {
            this.props.pivotTableComp.toggleFieldExpansion(this.props.axetype, this.props.field);
        } else {

            var thispos = reactUtils.getOffset(this.getDOMNode());

            // inform mousedown, save start pos
            this.setState({
                mousedown: true,
                mouseoffset: {
                    x: thispos.x - e.pageX,
                    y: thispos.y - e.pageY,
                },
                startpos: {
                    x: e.pageX,
                    y: e.pageY
                }
            });
        }

        // prevent event bubbling (to prevent text selection while dragging for example)
        e.stopPropagation();
        e.preventDefault();
    },
    onMouseUp: function(e) {

        var isdragged = this.state.dragging;

        this.setState({
            mousedown: false,
            dragging: false,
            size: null,
            pos: {
                x: 0,
                y: 0
            }
        });

        if (!e.ctrlKey && !isdragged) {
            // if button was not dragged, proceed as a click
            this.props.pivotTableComp.sort(this.props.axetype, this.props.field);
        }
    },
    onMouseMove: function(e) {
        // if the mouse is not down while moving, return (no drag)
        if (!this.props.pivotTableComp.pgrid.config.canMoveFields || !this.state.mousedown) return;

        var size = null;
        if (!this.state.dragging) {
            size = reactUtils.getSize(this.getDOMNode());
        } else {
            size = this.state.size;
        }

        var newpos = {
            x: e.pageX + this.state.mouseoffset.x,
            y: e.pageY + this.state.mouseoffset.y
        };

        this.setState({
            dragging: true,
            size: size,
            pos: newpos
        });

        dragManager.elementMoved();

        e.stopPropagation();
        e.preventDefault();
    },
    updateClasses: function() {
        this.getDOMNode().className = this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().pivotButton;
    },
    render: function() {
        var self = this;
        var divstyle = {
            left: self.state.pos.x + 'px',
            top: self.state.pos.y + 'px',
            position: self.state.dragging ? 'fixed' : '',
            zIndex: 101
        };

        if (self.state.size) {
            divstyle.width = self.state.size.width + 'px';
        }

        var sortDirectionClass = self.props.field.sort.order === 'asc' ?
            'sort-asc' :
            //' \u2191' :
            (self.props.field.sort.order === 'desc' ?
                'sort-desc' :
                //' \u2193' :
                '');
        var filterClass = (self.state.dragging ? '' : 'fltr-btn') + (this.props.pivotTableComp.pgrid.isFieldFiltered(this.props.field.name) ? ' fltr-btn-active' : '');
        var fieldAggFunc = '';
        if (self.props.axetype === axe.Type.DATA) {
            fieldAggFunc = React.createElement("small", null, ' (' + self.props.field.aggregateFuncName + ')');
        }

        return React.createElement("div", {
                key: self.props.field.name,
                className: this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().pivotButton,
                onMouseDown: this.onMouseDown,
                onMouseUp: this.onMouseUp,
                style: divstyle
            },
            React.createElement("table", null,
                React.createElement("tbody", null,
                    React.createElement("tr", null,
                        React.createElement("td", {
                            className: "caption"
                        }, self.props.field.caption, fieldAggFunc),
                        React.createElement("td", null, React.createElement("div", {
                            className: 'sort-indicator ' + sortDirectionClass
                        })),
                        React.createElement("td", {
                                className: "filter"
                            },
                            React.createElement("div", {
                                ref: "filterButton",
                                className: filterClass,
                                onMouseDown: self.state.dragging ? null : this.onFilterMouseDown
                            })
                        )
                    )
                )
            )
        );
    }
});
/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableUpperButtons = react.createClass({
    render: function() {
        var self = this;
        var PivotButton = comps.PivotButton;
        var DropTarget = comps.DropTarget;

        var config = this.props.pivotTableComp.pgridwidget.pgrid.config;

        var fieldsDropTarget;
        if (config.canMoveFields) {
            var fieldsButtons = config.availablefields().map(function(field, index) {
                return React.createElement(PivotButton, {
                    key: field.name,
                    field: field,
                    axetype: null,
                    position: index,
                    pivotTableComp: self.props.pivotTableComp
                });
            });
            fieldsDropTarget = React.createElement("tr", null,
                React.createElement("td", {
                        className: "flds-grp-cap av-flds text-muted"
                    },
                    React.createElement("div", null, "Fields")
                ),
                React.createElement("td", {
                        className: "av-flds"
                    },
                    React.createElement(DropTarget, {
                        buttons: fieldsButtons,
                        axetype: null
                    })
                )
            );
        } else {
            fieldsDropTarget = null;
        }

        var dataButtons = config.dataFields.map(function(field, index) {
            return React.createElement(PivotButton, {
                key: field.name,
                field: field,
                axetype: axe.Type.DATA,
                position: index,
                pivotTableComp: self.props.pivotTableComp
            });
        });

        var dataDropTarget = React.createElement("tr", null,
            React.createElement("td", {
                    className: "flds-grp-cap text-muted"
                },
                React.createElement("div", null, "Data")
            ),
            React.createElement("td", {
                    className: "empty"
                },
                React.createElement(DropTarget, {
                    buttons: dataButtons,
                    axetype: axe.Type.DATA
                })
            )
        );

        return React.createElement("table", {
                className: "inner-table upper-buttons"
            },
            React.createElement("tbody", null,
                fieldsDropTarget,
                dataDropTarget
            )
        );
    }
});
/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableColumnButtons = react.createClass({
    render: function() {
        var self = this;
        var PivotButton = comps.PivotButton;
        var DropTarget = comps.DropTarget;

        var config = this.props.pivotTableComp.pgridwidget.pgrid.config;

        var columnButtons = config.columnFields.map(function(field, index) {
            return React.createElement(PivotButton, {
                key: field.name,
                field: field,
                axetype: axe.Type.COLUMNS,
                position: index,
                pivotTableComp: self.props.pivotTableComp
            });
        });

        return React.createElement(DropTarget, {
            buttons: columnButtons,
            axetype: axe.Type.COLUMNS
        });
    }
});
/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableRowButtons = react.createClass({
    render: function() {
        var self = this;
        var PivotButton = comps.PivotButton;
        var DropTarget = comps.DropTarget;

        var config = this.props.pivotTableComp.pgridwidget.pgrid.config;

        var rowButtons = config.rowFields.map(function(field, index) {
            return React.createElement(PivotButton, {
                key: field.name,
                field: field,
                axetype: axe.Type.ROWS,
                position: index,
                pivotTableComp: self.props.pivotTableComp
            });
        });

        return React.createElement(DropTarget, {
            buttons: rowButtons,
            axetype: axe.Type.ROWS
        });
    }
});
/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableColumnHeaders = react.createClass({
    render: function() {
        var self = this;
        var PivotRow = comps.PivotRow;
        var pgridwidget = this.props.pivotTableComp.pgridwidget;
        var cntrClass = pgridwidget.columns.headers.length === 0 ? '' : ' columns-cntr';

        var layoutInfos = {
            lastLeftMostCellVSpan: 0,
            topMostCells: {}
        };

        var columnHeaders = pgridwidget.columns.headers.map(function(headerRow, index) {
            return React.createElement(PivotRow, {
                key: index,
                row: headerRow,
                axetype: axe.Type.COLUMNS,
                pivotTableComp: self.props.pivotTableComp,
                layoutInfos: layoutInfos
            });
        });

        return React.createElement("div", {
                className: 'inner-table-container' + cntrClass,
                ref: "colHeadersContainer",
                onWheel: this.props.pivotTableComp.onWheel
            },
            React.createElement("table", {
                    className: "inner-table"
                },
                React.createElement("colgroup", null),
                React.createElement("tbody", null,
                    columnHeaders
                )
            )
        );
    }
});
/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableRowHeaders = react.createClass({
    setColGroup: function(widths) {
        var node = this.getDOMNode();
        var colGroupNode = this.refs.colgroup.getDOMNode();
        node.style.tableLayout = 'auto';

        colGroupNode.innerHTML = '';
        for (var i = 0; i < widths.length; i++) {
            var col = document.createElement('col');
            col.style.width = (widths[i] + 8) + 'px';
            colGroupNode.appendChild(col);
        }
        node.style.tableLayout = 'fixed';
    },
    render: function() {
        var self = this;
        var PivotRow = comps.PivotRow;
        var pgridwidget = this.props.pivotTableComp.pgridwidget;
        var cntrClass = pgridwidget.rows.headers.length === 0 ? '' : ' rows-cntr';

        var layoutInfos = {
            lastLeftMostCellVSpan: 0,
            topMostCells: {}
        };

        var rowHeaders = pgridwidget.rows.headers.map(function(headerRow, index) {
            return React.createElement(PivotRow, {
                key: index,
                row: headerRow,
                axetype: axe.Type.ROWS,
                layoutInfos: layoutInfos,
                pivotTableComp: self.props.pivotTableComp
            });
        });

        return React.createElement("div", {
                className: 'inner-table-container' + cntrClass,
                ref: "rowHeadersContainer",
                onWheel: this.props.pivotTableComp.onWheel
            },
            React.createElement("table", {
                    className: "inner-table"
                },
                React.createElement("colgroup", {
                    ref: "colgroup"
                }),
                React.createElement("tbody", null,
                    rowHeaders
                )
            )
        );
    }
});
/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableDataCells = react.createClass({
    render: function() {
        var self = this;
        var PivotRow = comps.PivotRow;

        var pgridwidget = this.props.pivotTableComp.pgridwidget;
        var layoutInfos = {
            lastLeftMostCellVSpan: 0,
            topMostCells: {}
        };

        var dataCells = pgridwidget.dataRows.map(function(dataRow, index) {
            return React.createElement(PivotRow, {
                key: index,
                row: dataRow,
                axetype: axe.Type.DATA,
                layoutInfos: layoutInfos,
                pivotTableComp: self.props.pivotTableComp
            });
        });

        return React.createElement("table", {
                className: "inner-table"
            },
            React.createElement("colgroup", null),
            React.createElement("tbody", null,
                dataCells
            )
        );
    }
});
/** @jsx React.DOM */

/* global module, require, React, react, reactUtils, document */
/*jshint eqnull: true*/

'use strict';

var scrollBarMixin = {
    scrollEvent: null,
    scrollClient: null,
    getInitialState: function() {
        // initial state, all zero.
        return {
            size: 16,
            mousedown: false,
            thumbOffset: 0
        };
    },
    componentDidMount: function() {
        this.scrollEvent = new ScrollEvent(this);
    },
    componentDidUpdate: function() {
        if (!this.state.mousedown) {
            // mouse not down, don't care about mouse up/move events.
            document.removeEventListener('mousemove', this.onMouseMove);
            document.removeEventListener('mouseup', this.onMouseUp);
        } else if (this.state.mousedown) {
            // mouse down, interested by mouse up/move events.
            document.addEventListener('mousemove', this.onMouseMove);
            document.addEventListener('mouseup', this.onMouseUp);
        }
    },
    componentWillUnmount: function() {
        document.removeEventListener('mousemove', this.onMouseMove);
        document.removeEventListener('mouseup', this.onMouseUp);
    },
    onMouseDown: function(e) {
        // drag with left mouse button
        if (e.button !== 0) return;

        var thumbElem = this.refs.scrollThumb.getDOMNode();
        var thumbposInParent = reactUtils.getParentOffset(thumbElem);

        reactUtils.addClass(thumbElem, 'orb-scrollthumb-hover');

        // inform mousedown, save start pos
        this.setState({
            mousedown: true,
            mouseoffset: e[this.mousePosProp],
            thumbOffset: thumbposInParent[this.posProp]
        });

        // prevent event bubbling (to prevent text selection while dragging for example)
        e.stopPropagation();
        e.preventDefault();
    },
    onMouseUp: function() {

        if (this.state.mousedown) {
            var thumbElem = this.refs.scrollThumb.getDOMNode();
            reactUtils.removeClass(thumbElem, 'orb-scrollthumb-hover');
        }

        this.setState({
            mousedown: false
        });
    },
    onMouseMove: function(e) {

        // if the mouse is not down while moving, return (no drag)
        if (!this.state.mousedown) return;

        e.stopPropagation();
        e.preventDefault();

        var amount = e[this.mousePosProp] - this.state.mouseoffset;
        this.state.mouseoffset = e[this.mousePosProp];

        this.scroll(amount);
    },
    getScrollSize: function() {
        if (this.scrollClient != null) {
            return reactUtils.getSize(this.scrollClient)[this.sizeProp];
        } else {
            return reactUtils.getSize(this.getDOMNode())[this.sizeProp];
        }
    },
    setScrollClient: function(scrollClient, scrollCallback) {
        this.scrollClient = scrollClient;
        this.scrollEvent.callback = scrollCallback;
    },
    getScrollPercent: function() {
        var maxOffset = this.getScrollSize() - this.state.size;
        return maxOffset <= 0 ? 0 : this.state.thumbOffset / maxOffset;
    },
    refresh: function() {
        if (this.scrollClient) {
            var scrolledElement = this.scrollClient.children[0];

            var clientSize = reactUtils.getSize(this.scrollClient);
            var elementSize = reactUtils.getSize(scrolledElement);

            var scrollBarContainerSize = this.getScrollSize();
            var newSize = clientSize[this.sizeProp] >= elementSize[this.sizeProp] ? 0 : (clientSize[this.sizeProp] / elementSize[this.sizeProp]) * scrollBarContainerSize;

            this.setState({
                    containerSize: scrollBarContainerSize,
                    size: newSize,
                    thumbOffset: Math.min(this.state.thumbOffset, scrollBarContainerSize - newSize)
                },
                this.scrollEvent.raise
            );

        }
    },
    scroll: function(amount, mode) {
        if (this.state.size > 0) {
            if (mode == 1) amount *= 8;

            var maxOffset = this.getScrollSize() - this.state.size;
            var newOffset = this.state.thumbOffset + amount;
            if (newOffset < 0) newOffset = 0;
            if (newOffset > maxOffset) newOffset = maxOffset;

            this.setState({
                    thumbOffset: newOffset
                },
                this.scrollEvent.raise
            );
            return true;
        }
        return false;
    },
    onWheel: function(e) {
        this.scroll(e.deltaY, e.deltaMode);
        e.stopPropagation();
        e.preventDefault();
    },
    render: function() {
        var self = this;

        var thumbStyle = {
            padding: 0
        };
        thumbStyle[this.sizeProp] = this.state.size;
        thumbStyle[this.offsetCssProp] = this.state.thumbOffset;

        var thisStyle = {};
        thisStyle[this.sizeProp] = this.state.containerSize;

        var thumbClass = "orb-scrollthumb " + this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().scrollBar;

        var scrollThumb = this.state.size <= 0 ?
            null :
            React.createElement("div", {
                className: thumbClass,
                style: thumbStyle,
                ref: "scrollThumb",
                onMouseDown: this.onMouseDown
            });

        return React.createElement("div", {
                className: this.cssClass,
                style: thisStyle,
                onWheel: this.onWheel
            },
            scrollThumb
        );
    }
};

function ScrollEvent(scrollBarComp) {
    var self = this;
    this.scrollBarComp = scrollBarComp;
    this.callback = null;
    this.raise = function() {
        if (self.callback) {
            self.callback(self.scrollBarComp.getScrollPercent());
        }
    };
}

module.exports.HorizontalScrollBar = react.createClass({
    mixins: [scrollBarMixin],
    posProp: 'x',
    mousePosProp: 'pageX',
    sizeProp: 'width',
    offsetCssProp: 'left',
    cssClass: 'orb-h-scrollbar'
});

module.exports.VerticalScrollBar = react.createClass({
    mixins: [scrollBarMixin],
    posProp: 'y',
    mousePosProp: 'pageY',
    sizeProp: 'height',
    offsetCssProp: 'top',
    cssClass: 'orb-v-scrollbar'
});
/** @jsx React.DOM */

/* global module, react, React */
/*jshint eqnull: true*/

'use strict';

module.exports.FilterPanel = react.createClass({
    pgridwidget: null,
    values: null,
    filterManager: null,
    getInitialState: function() {
        this.pgridwidget = this.props.pivotTableComp.pgridwidget;
        return {};
    },
    destroy: function() {
        var container = this.getDOMNode().parentNode;
        React.unmountComponentAtNode(container);
        container.parentNode.removeChild(container);
    },
    onFilter: function(operator, term, staticValue, excludeStatic) {
        this.props.pivotTableComp.applyFilter(this.props.field, operator, term, staticValue, excludeStatic);
        this.destroy();
    },
    onMouseDown: function(e) {
        var container = this.getDOMNode().parentNode;
        var target = e.target;
        while (target != null) {
            if (target == container) {
                return true;
            }
            target = target.parentNode;
        }

        this.destroy();
    },
    onMouseWheel: function(e) {
        var valuesTable = this.refs.valuesTable.getDOMNode();
        var target = e.target;
        while (target != null) {
            if (target == valuesTable) {
                if (valuesTable.scrollHeight <= valuesTable.clientHeight) {
                    e.stopPropagation();
                    e.preventDefault();
                }
                return;
            }
            target = target.parentNode;
        }

        this.destroy();
    },
    componentWillMount: function() {
        document.addEventListener('mousedown', this.onMouseDown);
        document.addEventListener('wheel', this.onMouseWheel);
        window.addEventListener('resize', this.destroy);
    },
    componentDidMount: function() {
        this.filterManager.init(this.getDOMNode());
    },
    componentWillUnmount: function() {
        document.removeEventListener('mousedown', this.onMouseDown);
        document.removeEventListener('wheel', this.onMouseWheel);
        window.removeEventListener('resize', this.destroy);
    },
    render: function() {
        var Dropdown = comps.Dropdown;
        var checkboxes = [];

        this.filterManager = new FilterManager(this, this.pgridwidget.pgrid.getFieldFilter(this.props.field));
        this.values = this.pgridwidget.pgrid.getFieldValues(this.props.field);

        function addCheckboxRow(value, text) {
            return checkboxes.push(React.createElement("tr", {
                    key: value
                },
                React.createElement("td", {
                        className: "fltr-chkbox"
                    },
                    React.createElement("input", {
                        type: "checkbox",
                        value: value,
                        defaultChecked: "checked"
                    })
                ),
                React.createElement("td", {
                    className: "fltr-val",
                    title: text || value
                }, text || value)
            ));
        }

        addCheckboxRow(filtering.ALL, '(Show All)');
        if (this.values.containsBlank) {
            addCheckboxRow(filtering.BLANK, '(Blank)');
        }

        for (var i = 0; i < this.values.length; i++) {
            addCheckboxRow(this.values[i]);
        }

        var buttonClass = this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().orbButton;
        var pivotStyle = window.getComputedStyle(this.props.pivotTableComp.getDOMNode(), null);
        var style = {
            fontFamily: pivotStyle.getPropertyValue('font-family'),
            fontSize: pivotStyle.getPropertyValue('font-size')
        };

        var currentFilter = this.pgridwidget.pgrid.getFieldFilter(this.props.field);

        return React.createElement("table", {
                className: "fltr-scntnr",
                style: style
            },
            React.createElement("tbody", null,
                React.createElement("tr", null,
                    React.createElement("td", {
                            className: "srchop-col"
                        },
                        React.createElement(Dropdown, {
                            values: [
                                filtering.Operators.MATCH.name,
                                filtering.Operators.NOTMATCH.name,
                                filtering.Operators.EQ.name,
                                filtering.Operators.NEQ.name,
                                filtering.Operators.GT.name,
                                filtering.Operators.GTE.name,
                                filtering.Operators.LT.name,
                                filtering.Operators.LTE.name
                            ],
                            selectedValue: currentFilter && currentFilter.operator ? currentFilter.operator.name : filtering.Operators.MATCH.name,
                            onValueChanged: this.filterManager.onOperatorChanged
                        })
                    ),
                    React.createElement("td", {
                        className: "srchtyp-col",
                        title: "Enable/disable Regular expressions"
                    }, ".*"),
                    React.createElement("td", {
                            className: "srchbox-col"
                        },
                        React.createElement("table", {
                                style: {
                                    width: '100%'
                                }
                            },
                            React.createElement("tbody", null,
                                React.createElement("tr", null,
                                    React.createElement("td", null, React.createElement("input", {
                                        type: "text",
                                        placeholder: "search"
                                    })),
                                    React.createElement("td", null, React.createElement("div", {
                                        className: "srchclear-btn",
                                        onClick: this.clearFilter
                                    }, "x"))
                                )
                            )
                        )
                    )
                ),
                React.createElement("tr", null,
                    React.createElement("td", {
                            colSpan: "3",
                            className: "fltr-vals-col"
                        },
                        React.createElement("table", {
                                className: "fltr-vals-tbl",
                                ref: "valuesTable"
                            },
                            React.createElement("tbody", null,
                                checkboxes
                            )
                        )
                    )
                ),
                React.createElement("tr", {
                        className: "bottom-row"
                    },
                    React.createElement("td", {
                            className: "cnfrm-btn-col",
                            colSpan: "2"
                        },
                        React.createElement("input", {
                            type: "button",
                            className: buttonClass,
                            value: "Ok",
                            style: {
                                float: 'left'
                            }
                        }),
                        React.createElement("input", {
                            type: "button",
                            className: buttonClass,
                            value: "Cancel",
                            style: {
                                float: 'left'
                            }
                        })
                    ),
                    React.createElement("td", {
                            className: "resize-col"
                        },
                        React.createElement("div", null)
                    )
                )
            )
        );
    }
});

function FilterManager(reactComp, initialFilterObject) {

    var self = this;
    var INDETERMINATE = 'indeterminate';

    var savedCheckedValues;
    var isSearchMode = false;
    var isRegexMode = false;
    var operator = filtering.Operators.MATCH;
    var lastSearchTerm = '';

    var elems = {
        filterContainer: null,
        checkboxes: {},
        searchBox: null,
        operatorBox: null,
        allCheckbox: null,
        addCheckbox: null,
        enableRegexButton: null,
        clearSearchButton: null,
        okButton: null,
        cancelButton: null,
        resizeGrip: null
    };

    var resizeManager;

    this.init = function(filterContainerElement) {

        elems.filterContainer = filterContainerElement;
        elems.checkboxes = {};
        elems.searchBox = elems.filterContainer.rows[0].cells[2].children[0].rows[0].cells[0].children[0];
        elems.clearSearchButton = elems.filterContainer.rows[0].cells[2].children[0].rows[0].cells[1].children[0];
        elems.operatorBox = elems.filterContainer.rows[0].cells[0].children[0];
        elems.okButton = elems.filterContainer.rows[2].cells[0].children[0];
        elems.cancelButton = elems.filterContainer.rows[2].cells[0].children[1];
        elems.resizeGrip = elems.filterContainer.rows[2].cells[1].children[0];

        var rows = elems.filterContainer.rows[1].cells[0].children[0].rows;
        for (var i = 0; i < rows.length; i++) {
            var checkbox = rows[i].cells[0].children[0];
            elems.checkboxes[checkbox.value] = checkbox;
        }

        elems.allCheckbox = elems.checkboxes[filtering.ALL];
        elems.addCheckbox = null;
        elems.enableRegexButton = elems.filterContainer.rows[0].cells[1];

        resizeManager = new ResizeManager(elems.filterContainer.parentNode, elems.filterContainer.rows[1].cells[0].children[0], elems.resizeGrip);

        applyInitialFilterObject();
        addEventListeners();
    };

    this.onOperatorChanged = function(newOperator) {
        if (operator.name !== newOperator) {
            operator = filtering.Operators.get(newOperator);
            self.toggleRegexpButtonVisibility();
            self.searchChanged('operatorChanged');
        }
    };

    function checkboxVisible(checkbox, isVisible) {
        if (isVisible != null) {
            checkbox.parentNode.parentNode.style.display = isVisible ? '' : 'none';
        } else {
            return checkbox.parentNode.parentNode.style.display != 'none';
        }
    }

    function applyInitialFilterObject() {
        if (initialFilterObject) {
            var staticInfos = {
                values: initialFilterObject.staticValue,
                toExclude: initialFilterObject.excludeStatic
            };

            if (initialFilterObject.term) {
                isSearchMode = true;

                operator = initialFilterObject.operator;
                self.toggleRegexpButtonVisibility();

                if (initialFilterObject.regexpMode) {
                    isRegexMode = true;
                    self.toggleRegexpButtonState();
                    lastSearchTerm = initialFilterObject.term.source;
                } else {
                    lastSearchTerm = initialFilterObject.term;
                }

                elems.searchBox.value = lastSearchTerm;

                self.applyFilterTerm(initialFilterObject.operator, initialFilterObject.term);
            } else {
                savedCheckedValues = staticInfos;
            }

            self.updateCheckboxes(staticInfos);
            self.updateAllCheckbox();
        }
    }

    function addEventListeners() {
        self.toggleRegexpButtonVisibility();

        elems.filterContainer.addEventListener('click', self.valueChecked);
        elems.searchBox.addEventListener('keyup', self.searchChanged);

        elems.clearSearchButton.addEventListener('click', self.clearSearchBox);

        elems.okButton.addEventListener('click', function() {
            var checkedObj = self.getCheckedValues();
            reactComp.onFilter(operator.name, operator.regexpSupported && isSearchMode && isRegexMode ? new RegExp(lastSearchTerm, 'i') : lastSearchTerm, checkedObj.values, checkedObj.toExclude);
        });
        elems.cancelButton.addEventListener('click', function() {
            reactComp.destroy();
        });
    }

    function ResizeManager(outerContainerElem, valuesTableElem, resizeGripElem) {

        var minContainerWidth = 301;
        var minContainerHeight = 223;

        var mousedownpos = {
            x: 0,
            y: 0
        };
        var isMouseDown = false;

        this.resizeMouseDown = function(e) {
            // drag/sort with left mouse button
            if (e.button !== 0) return;

            isMouseDown = true;
            document.body.style.cursor = 'se-resize';

            mousedownpos.x = e.pageX;
            mousedownpos.y = e.pageY;

            // prevent event bubbling (to prevent text selection while dragging for example)
            e.stopPropagation();
            e.preventDefault();
        };

        this.resizeMouseUp = function() {
            isMouseDown = false;
            document.body.style.cursor = 'auto';
            return true;
        };

        this.resizeMouseMove = function(e) {
            // if the mouse is not down while moving, return (no drag)
            if (!isMouseDown) return;

            var resizeGripSize = resizeGripElem.getBoundingClientRect();
            var outerContainerSize = outerContainerElem.getBoundingClientRect();
            var valuesTableSize = valuesTableElem.getBoundingClientRect();

            var outerContainerWidth = outerContainerSize.right - outerContainerSize.left;
            var outerContainerHeight = outerContainerSize.bottom - outerContainerSize.top;

            var offset = {
                x: outerContainerWidth <= minContainerWidth && e.pageX < resizeGripSize.left ? 0 : e.pageX - mousedownpos.x,
                y: outerContainerHeight <= minContainerHeight && e.pageY < resizeGripSize.top ? 0 : e.pageY - mousedownpos.y
            };

            var newContainerWidth = outerContainerWidth + offset.x;
            var newContainerHeight = outerContainerHeight + offset.y;

            mousedownpos.x = e.pageX;
            mousedownpos.y = e.pageY;

            if (newContainerWidth >= minContainerWidth) {
                outerContainerElem.style.width = newContainerWidth + 'px';
            }

            if (newContainerHeight >= minContainerHeight) {
                outerContainerElem.style.height = newContainerHeight + 'px';
                valuesTableElem.style.height = (valuesTableSize.bottom - valuesTableSize.top + offset.y) + 'px';
            }

            e.stopPropagation();
            e.preventDefault();
        };

        resizeGripElem.addEventListener('mousedown', this.resizeMouseDown);
        document.addEventListener('mouseup', this.resizeMouseUp);
        document.addEventListener('mousemove', this.resizeMouseMove);
    }

    this.clearSearchBox = function() {
        elems.searchBox.value = '';
        self.searchChanged();
    };

    this.toggleRegexpButtonVisibility = function() {
        if (operator.regexpSupported) {
            elems.enableRegexButton.addEventListener('click', self.regexpActiveChanged);
            reactUtils.removeClass(elems.enableRegexButton, 'srchtyp-col-hidden');

        } else {
            elems.enableRegexButton.removeEventListener('click', self.regexpActiveChanged);
            reactUtils.addClass(elems.enableRegexButton, 'srchtyp-col-hidden');
        }
    };

    this.toggleRegexpButtonState = function() {
        elems.enableRegexButton.className = elems.enableRegexButton.className.replace('srchtyp-col-active', '');
        if (isRegexMode) {
            reactUtils.addClass(elems.enableRegexButton, 'srchtyp-col-active');
        } else {
            reactUtils.removeClass(elems.enableRegexButton, 'srchtyp-col-active');
        }
    };

    this.regexpActiveChanged = function() {
        isRegexMode = !isRegexMode;
        self.toggleRegexpButtonState();
        self.searchChanged('regexModeChanged');
    };

    this.valueChecked = function(e) {
        var target = e.target;
        if (target && target.type && target.type === 'checkbox') {
            if (target == elems.allCheckbox) {
                self.updateCheckboxes({
                    values: elems.allCheckbox.checked
                });
            } else {
                self.updateAllCheckbox();
            }
        }
    };

    this.applyFilterTerm = function(operator, term) {
        var defaultVisible = term ? false : true;
        var opterm = operator.regexpSupported && isSearchMode ? (isRegexMode ? term : utils.escapeRegex(term)) : term;
        checkboxVisible(elems.allCheckbox, defaultVisible);
        for (var i = 0; i < reactComp.values.length; i++) {
            var val = reactComp.values[i];
            var checkbox = elems.checkboxes[val];
            var visible = !isSearchMode || operator.func(val, opterm);
            checkboxVisible(checkbox, visible);
            checkbox.checked = visible;
        }
    };

    this.searchChanged = function(e) {
        var search = (elems.searchBox.value || '').trim();
        if (e === 'operatorChanged' || (e === 'regexModeChanged' && search) || search != lastSearchTerm) {
            lastSearchTerm = search;

            var previousIsSearchMode = isSearchMode;
            isSearchMode = search !== '';

            if (isSearchMode && !previousIsSearchMode) {
                savedCheckedValues = self.getCheckedValues();
            }

            //var searchTerm = operator.regexpSupported && isSearchMode ? new RegExp(isRegexMode ? search : utils.escapeRegex(search), 'i') : search;
            if (e !== 'operatorChanged' || isSearchMode) {
                self.applyFilterTerm(operator, search);
            }

            if (!isSearchMode && previousIsSearchMode) {
                self.updateCheckboxes(savedCheckedValues);
            }

            self.updateAllCheckbox();
        }
    };

    this.getCheckedValues = function() {
        if (!isSearchMode && !elems.allCheckbox.indeterminate) {
            return {
                values: elems.allCheckbox.checked ? filtering.ALL : filtering.NONE,
                toExclude: false
            };
        } else {
            var staticValue;
            var i,
                val,
                checkbox;
            var valuesCount = 0,
                checkedCount = 0;

            for (i = 0; i < reactComp.values.length; i++) {
                val = reactComp.values[i];
                checkbox = elems.checkboxes[val];
                if (checkboxVisible(checkbox)) {
                    valuesCount++;
                    if (checkbox.checked) {
                        checkedCount++;
                    }
                }
            }

            var excludeUnchecked = false;

            if (checkedCount === 0) {
                staticValue = filtering.NONE;
            } else if (checkedCount == valuesCount) {
                staticValue = filtering.ALL;
            } else {
                staticValue = [];
                excludeUnchecked = checkedCount > (valuesCount / 2 + 1);

                for (i = 0; i < reactComp.values.length; i++) {
                    val = reactComp.values[i];
                    checkbox = elems.checkboxes[val];
                    if (checkboxVisible(checkbox)) {
                        if ((!excludeUnchecked && checkbox.checked) || (excludeUnchecked && !checkbox.checked)) {
                            staticValue.push(val);
                        }
                    }
                }
            }
            return {
                values: staticValue,
                toExclude: excludeUnchecked
            };
        }
    };

    this.updateCheckboxes = function(checkedList) {
        var values = checkedList ? checkedList.values : null;
        var allchecked = utils.isArray(values) ?
            null :
            (values == null || values === filtering.ALL ?
                true :
                (values === filtering.NONE ?
                    false :
                    !!values
                )
            );
        for (var i = 0; i < reactComp.values.length; i++) {
            var val = reactComp.values[i];
            var checkbox = elems.checkboxes[val];
            if (checkboxVisible(checkbox)) {
                if (allchecked != null) {
                    checkbox.checked = allchecked;
                } else {
                    var valInList = values.indexOf(val) >= 0;
                    checkbox.checked = checkedList.toExclude ? !valInList : valInList;
                }
            }
        }
    };

    this.updateAllCheckbox = function() {
        if (!isSearchMode) {
            var allchecked = null;
            for (var i = 0; i < reactComp.values.length; i++) {
                var checkbox = elems.checkboxes[reactComp.values[i]];
                if (allchecked == null) {
                    allchecked = checkbox.checked;
                } else {
                    if (allchecked !== checkbox.checked) {
                        allchecked = INDETERMINATE;
                        break;
                    }
                }
            }

            if (allchecked === INDETERMINATE) {
                elems.allCheckbox.indeterminate = true;
                elems.allCheckbox.checked = false;
            } else {
                elems.allCheckbox.indeterminate = false;
                elems.allCheckbox.checked = allchecked;
            }
        }
    };
}

/** @jsx React.DOM */

/* global module, react, React */
/*jshint eqnull: true*/

'use strict';

module.exports.Dropdown = react.createClass({
    openOrClose: function(e) {
        var valueNode = this.refs.valueElement.getDOMNode();
        var valuesListNode = this.refs.valuesList.getDOMNode();
        if (e.target === valueNode && valuesListNode.style.display === 'none') {
            valuesListNode.style.display = 'block';
        } else {
            valuesListNode.style.display = 'none';
        }
    },
    onMouseEnter: function() {
        var valueNode = this.refs.valueElement.getDOMNode();
        valueNode.className = "orb-tgl-btn-down";
        valueNode.style.backgroundPosition = 'right center';
    },
    onMouseLeave: function() {
        this.refs.valueElement.getDOMNode().className = "";
    },
    componentDidMount: function() {
        document.addEventListener('click', this.openOrClose);
    },
    componentWillUnmount: function() {
        document.removeEventListener('click', this.openOrClose);
    },
    selectValue: function(e) {
        var listNode = this.refs.valuesList.getDOMNode();
        var target = e.target;
        var isli = false;
        while (!isli && target != null) {
            if (target.parentNode == listNode) {
                isli = true;
                break;
            }
            target = target.parentNode;
        }

        if (isli) {
            var value = target.textContent;
            var valueElement = this.refs.valueElement.getDOMNode();
            if (valueElement.textContent != value) {
                valueElement.textContent = value;
                if (this.props.onValueChanged) {
                    this.props.onValueChanged(value);
                }
            }
        }
    },
    render: function() {
        function createSelectValueFunc(value) {
            return function() {
                this.selectValue(value);
            };
        }

        var values = [];
        for (var i = 0; i < this.props.values.length; i++) {
            values.push(React.createElement("li", {
                key: 'item' + i,
                dangerouslySetInnerHTML: {
                    __html: this.props.values[i]
                }
            }));
        }

        return React.createElement("div", {
                className: "orb-select"
            },
            React.createElement("div", {
                ref: "valueElement",
                dangerouslySetInnerHTML: {
                    __html: this.props.selectedValue
                },
                onMouseEnter: this.onMouseEnter,
                onMouseLeave: this.onMouseLeave
            }),
            React.createElement("ul", {
                    ref: "valuesList",
                    style: {
                        display: 'none'
                    },
                    onClick: this.selectValue
                },
                values
            )
        );
    }
});
/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.Grid = react.createClass({
    render: function() {
        var data = this.props.data;
        var headers = this.props.headers;
        var tableClasses = this.props.theme.getGridClasses();

        var rows = [];

        if (headers && headers.length > 0) {
            var headerRow = [];
            for (var h = 0; h < headers.length; h++) {
                headerRow.push(React.createElement("th", {
                    key: 'h' + h
                }, headers[h]));
            }
            rows.push(React.createElement("tr", {
                key: 'h'
            }, headerRow));
        }

        if (data && data.length > 0) {
            for (var i = 0; i < data.length; i++) {
                var row = [];
                if (utils.isArray(data[i])) {
                    for (var j = 0; j < data[i].length; j++) {
                        row.push(React.createElement("td", {
                            key: i + '' + j
                        }, data[i][j]));
                    }
                } else {
                    for (var prop in data[i]) {
                        if (data[i].hasOwnProperty(prop)) {
                            row.push(React.createElement("td", {
                                key: i + '' + prop
                            }, data[i][prop]));
                        }
                    }
                }
                rows.push(React.createElement("tr", {
                    key: i
                }, row));
            }
        }

        return React.createElement("table", {
                className: tableClasses.table
            },
            React.createElement("tbody", null,
                rows
            )
        );
    }
});
/** @jsx React.DOM */

/* global module, require, React */

'use strict';

function createOverlay() {
    var overlayElement = document.createElement('div');
    overlayElement.className = 'orb-overlay orb-overlay-hidden';
    document.body.appendChild(overlayElement);
    return overlayElement;
}

var Dialog = module.exports.Dialog = react.createClass({
    statics: {
        create: function() {
            var dialogFactory = React.createFactory(Dialog);
            var overlay = createOverlay();

            return {
                show: function(props) {
                    React.render(dialogFactory(props), overlay);
                }
            };
        }
    },
    overlayElement: null,
    setOverlayClass: function(visible) {
        this.overlayElement.className = this.props.theme.getDialogClasses(visible).overlay;
    },
    componentDidMount: function() {
        this.overlayElement = this.getDOMNode().parentNode;
        this.setOverlayClass(true);
        this.overlayElement.addEventListener('click', this.close);

        var dialogElement = this.overlayElement.children[0];
        var dialogBodyElement = dialogElement.children[0].children[1];

        var screenWidth = Math.max(document.documentElement.clientWidth, window.innerWidth || 0);
        var screenHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0);
        var maxHeight = 2 * screenHeight / 3;
        maxHeight = maxHeight < 301 ? 301 : maxHeight;
        var dWidth = dialogElement.offsetWidth + (dialogElement.offsetHeight > maxHeight ? 11 : 0);
        var dHeight = dialogElement.offsetHeight > maxHeight ? maxHeight : dialogElement.offsetHeight;

        dialogElement.style.top = (screenHeight > dHeight ? (screenHeight - dHeight) / 2 : 0) + 'px';
        dialogElement.style.left = (screenWidth > dWidth ? (screenWidth - dWidth) / 2 : 0) + 'px';
        dialogElement.style.height = dHeight + 'px';
        dialogBodyElement.style.width = dWidth + 'px';
        dialogBodyElement.style.height = (dHeight - 45) + 'px';
    },
    close: function(e) {
        if (e.target == this.overlayElement || e.target.className === 'button-close') {
            this.overlayElement.removeEventListener('click', this.close);
            React.unmountComponentAtNode(this.overlayElement);
            this.setOverlayClass(false);
        }
    },
    render: function() {
        if (this.props.comp) {
            var comp = React.createElement(this.props.comp.type, this.props.comp.props);
            var classes = this.props.theme.getDialogClasses();

            return React.createElement("div", {
                    className: classes.dialog,
                    style: this.props.style || {}
                },
                React.createElement("div", {
                        className: classes.content
                    },
                    React.createElement("div", {
                        className: classes.header
                    }, React.createElement("div", {
                        className: "button-close",
                        onClick: this.close
                    }), React.createElement("div", {
                        className: classes.title
                    }, this.props.title)),
                    React.createElement("div", {
                            className: classes.body
                        },
                        comp
                    )
                )
            );
        }
    }
});
/** @jsx React.DOM */

/* global module, require, react */
/*jshint node: true, eqnull: true*/

'use strict';

module.exports.Toolbar = react.createClass({
    _toInit: [],
    componentDidMount: function() {
        for (var i = 0; i < this._toInit.length; i++) {
            var btn = this._toInit[i];
            btn.init(this.props.pivotTableComp, this.refs[btn.ref].getDOMNode());
        }
    },
    componentDidUpdate: function() {
        for (var i = 0; i < this._toInit.length; i++) {
            var btn = this._toInit[i];
            btn.init(this.props.pivotTableComp, this.refs[btn.ref].getDOMNode());
        }
    },
    createCallback: function(action) {
        if (action != null) {
            var pgridComponent = this.props.pivotTableComp;
            return function(e) {
                action(pgridComponent, e.target);
            };
        }
        return null;
    },
    render: function() {

        var config = this.props.pivotTableComp.pgridwidget.pgrid.config;

        if (config.toolbar && config.toolbar.visible) {

            var configButtons = config.toolbar.buttons ?
                defaultToolbarConfig.buttons.concat(config.toolbar.buttons) :
                defaultToolbarConfig.buttons;

            var buttons = [];
            for (var i = 0; i < configButtons.length; i++) {
                var btnConfig = configButtons[i];
                var refName = 'btn' + i;

                if (btnConfig.type == 'separator') {
                    buttons.push(React.createElement("div", {
                        key: i,
                        className: "orb-tlbr-sep"
                    }));
                } else if (btnConfig.type == 'label') {
                    buttons.push(React.createElement("div", {
                        key: i,
                        className: "orb-tlbr-lbl"
                    }, btnConfig.text));
                } else {
                    buttons.push(React.createElement("div", {
                        key: i,
                        className: 'orb-tlbr-btn ' + btnConfig.cssClass,
                        title: btnConfig.tooltip,
                        ref: refName,
                        onClick: this.createCallback(btnConfig.action)
                    }));
                }
                if (btnConfig.init) {
                    this._toInit.push({
                        ref: refName,
                        init: btnConfig.init
                    });
                }
            }

            return React.createElement("div", null,
                buttons
            );
        }

        return React.createElement("div", null);
    }
});

var excelExport = require('../orb.export.excel');

var defaultToolbarConfig = {
    exportToExcel: function(pgridComponent, button) {
        var a = document.createElement('a');
        a.download = "orbpivotgrid.xls";
        a.href = excelExport(pgridComponent.props.pgridwidget);
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
    },
    expandAllRows: function(pgridComponent, button) {
        pgridComponent.toggleFieldExpansion(axe.Type.ROWS, null, true);
    },
    collapseAllRows: function(pgridComponent, button) {
        pgridComponent.toggleFieldExpansion(axe.Type.ROWS, null, false);
    },
    expandAllColumns: function(pgridComponent, button) {
        pgridComponent.toggleFieldExpansion(axe.Type.COLUMNS, null, true);
    },
    collapseAllColumns: function(pgridComponent, button) {
        pgridComponent.toggleFieldExpansion(axe.Type.COLUMNS, null, false);
    },
    updateSubtotalsButton: function(axetype, pgridComponent, button) {
        var subTotalsState = pgridComponent.pgridwidget.areSubtotalsVisible(axetype);
        button.style.display = subTotalsState === null ? 'none' : '';

        var classToAdd = '';
        var classToRemove = '';
        if (subTotalsState) {
            classToAdd = 'subtotals-visible';
            classToRemove = 'subtotals-hidden';
        } else {
            classToAdd = 'subtotals-hidden';
            classToRemove = 'subtotals-visible';
        }

        reactUtils.removeClass(button, classToRemove);
        reactUtils.addClass(button, classToAdd);
    },
    initSubtotals: function(axetype) {
        var self = this;
        return function(pgridComponent, button) {
            self.updateSubtotalsButton(axetype, pgridComponent, button);
        };
    },
    toggleSubtotals: function(axetype) {
        var self = this;
        return function(pgridComponent, button) {
            pgridComponent.toggleSubtotals(axetype);
            self.updateSubtotalsButton(axetype, pgridComponent, button);
        };
    },
    updateGrandtotalButton: function(axetype, pgridComponent, button) {
        var subTotalsState = pgridComponent.pgridwidget.isGrandtotalVisible(axetype);
        button.style.display = subTotalsState === null ? 'none' : '';

        var classToAdd = '';
        var classToRemove = '';
        if (subTotalsState) {
            classToAdd = 'grndtotal-visible';
            classToRemove = 'grndtotal-hidden';
        } else {
            classToAdd = 'grndtotal-hidden';
            classToRemove = 'grndtotal-visible';
        }

        reactUtils.removeClass(button, classToRemove);
        reactUtils.addClass(button, classToAdd);
    },
    initGrandtotal: function(axetype) {
        var self = this;
        return function(pgridComponent, button) {
            self.updateGrandtotalButton(axetype, pgridComponent, button);
        };
    },
    toggleGrandtotal: function(axetype) {
        var self = this;
        return function(pgridComponent, button) {
            pgridComponent.toggleGrandtotal(axetype);
            self.updateGrandtotalButton(axetype, pgridComponent, button);
        };
    }
};

defaultToolbarConfig.buttons = [{
    type: 'label',
    text: 'Rows:'
}, {
    type: 'button',
    tooltip: 'Expand all rows',
    cssClass: 'expand-all',
    action: defaultToolbarConfig.expandAllRows
}, {
    type: 'button',
    tooltip: 'Collapse all rows',
    cssClass: 'collapse-all',
    action: defaultToolbarConfig.collapseAllRows
}, {
    type: 'button',
    tooltip: 'Toggle rows sub totals',
    init: defaultToolbarConfig.initSubtotals(axe.Type.ROWS),
    action: defaultToolbarConfig.toggleSubtotals(axe.Type.ROWS)
}, {
    type: 'button',
    tooltip: 'Toggle rows grand total',
    init: defaultToolbarConfig.initGrandtotal(axe.Type.ROWS),
    action: defaultToolbarConfig.toggleGrandtotal(axe.Type.ROWS)
}, {
    type: 'separator'
}, {
    type: 'label',
    text: 'Columns:'
}, {
    type: 'button',
    tooltip: 'Expand all columns',
    cssClass: 'expand-all',
    action: defaultToolbarConfig.expandAllColumns
}, {
    type: 'button',
    tooltip: 'Collapse all columns',
    cssClass: 'collapse-all',
    action: defaultToolbarConfig.collapseAllColumns
}, {
    type: 'button',
    tooltip: 'Toggle columns sub totals',
    init: defaultToolbarConfig.initSubtotals(axe.Type.COLUMNS),
    action: defaultToolbarConfig.toggleSubtotals(axe.Type.COLUMNS)
}, {
    type: 'button',
    tooltip: 'Toggle columns grand total',
    init: defaultToolbarConfig.initGrandtotal(axe.Type.COLUMNS),
    action: defaultToolbarConfig.toggleGrandtotal(axe.Type.COLUMNS)
}, {
    type: 'separator'
}, {
    type: 'label',
    text: 'Export:'
}, {
    type: 'button',
    tooltip: 'Export to Excel',
    cssClass: 'export-xls',
    action: defaultToolbarConfig.exportToExcel
}, ];
