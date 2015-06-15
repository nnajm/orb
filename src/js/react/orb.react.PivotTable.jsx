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
    if(this.pgridwidget.moveField(button.props.field.name, button.props.axetype, newAxeType, position)) {
      this.setProps({});
    }
  },
  toggleFieldExpansion: function(axetype, field, newState) {
    if(this.pgridwidget.toggleFieldExpansion(axetype, field, newState)) {
      this.setProps({});
    }
  },
  toggleSubtotals: function(axetype) {
    if(this.pgridwidget.toggleSubtotals(axetype)) {
      this.setProps({});
    }
  },
  toggleGrandtotal: function(axetype) {
    if(this.pgridwidget.toggleGrandtotal(axetype)) {
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
    if(compCallback) {
      themeChangeCallbacks[this.id].push(compCallback);
    }
  },
  unregisterThemeChanged: function(compCallback) {
    var i;
    if(compCallback && (i = themeChangeCallbacks[this.id].indexOf(compCallback)) >= 0) {
      themeChangeCallbacks[this.id].splice(i, 1);
    }
  },
  changeTheme: function(newTheme) {
    if(this.pgridwidget.pgrid.config.setTheme(newTheme)) {
      // notify self/sub-components of the theme change
      for(var i = 0; i < themeChangeCallbacks[this.id].length; i++) {
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

    if(e.currentTarget == (elem = this.refs.colHeadersContainer.getDOMNode())) {
      scrollbar = this.refs.horizontalScrollBar;
      amount = e.deltaX || e.deltaY;
    } else if((e.currentTarget == (elem = this.refs.rowHeadersContainer.getDOMNode())) ||
              (e.currentTarget == (elem = this.refs.dataCellsContainer.getDOMNode())) ) {
      scrollbar = this.refs.verticalScrollBar;
      amount = e.deltaY;
    }

    if(scrollbar && scrollbar.scroll(amount, e.deltaMode)) {
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
         /*'colHeadersTable',*/ 'colHeadersContainer', /*'rowHeadersTable',*/ 'rowHeadersContainer', 'rowButtonsContainer',
         'toolbar', 'horizontalScrollBar', 'verticalScrollBar'].forEach(function(refname) {
          if(self.refs[refname]) {
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

      for(var i = 0; i < nodes.dataCellsTable.widthArray.length; i++) {
        var mxwidth = Math.max(nodes.dataCellsTable.widthArray[i], nodes.colHeadersTable.widthArray[i]);
        dataCellsTableMaxWidthArray.push(mxwidth);
        dataCellsTableMaxWidth += mxwidth;
      }

      var rowHeadersTableWidth = Math.max(nodes.rowHeadersTable.size.width, rowButtonsContainerWidth, 67);
      var rowDiff = rowHeadersTableWidth - nodes.rowHeadersTable.size.width;
      if(rowDiff > 0) {
        nodes.rowHeadersTable.size.width += rowDiff;
        nodes.rowHeadersTable.widthArray[nodes.rowHeadersTable.widthArray.length - 1] += rowDiff;
      }

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

    if(pivotContainerHeight) {
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
      pivotWrapperTable, 
      [
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
    if(config.width) { tblStyle.width = config.width; }
    if(config.height) { tblStyle.height = config.height; }

    return (
    <div className={classes.container} style={tblStyle} ref="pivotContainer">
      {config.toolbar && config.toolbar.visible ? <div ref="toolbar" className="orb-toolbar">
        <Toolbar pivotTableComp={self}></Toolbar>
      </div> : null}
      <table id={'tbl-' + self.id} ref="pivotWrapperTable" className={classes.table} style={{tableLayout: 'fixed'}}>
        <colgroup>
          <col ref="column1"></col>
          <col ref="column2"></col>
          <col ref="column3"></col>
          <col ref="column4"></col>
        </colgroup>
        <tbody>
          <tr ref="upperbuttonsRow">
            <td colSpan="4">
              <PivotTableUpperButtons pivotTableComp={self}></PivotTableUpperButtons>              
            </td>
          </tr>
          <tr ref="columnbuttonsRow">
            <td></td>
            <td style={{padding: '11px 4px !important'}}>
              <PivotTableColumnButtons pivotTableComp={self}></PivotTableColumnButtons>
            </td>
            <td colSpan="2"></td>
          </tr>
          <tr>
            <td style={{ position: 'relative'}}>
              <PivotTableRowButtons pivotTableComp={self} ref="rowButtonsContainer"></PivotTableRowButtons>
            </td>
            <td>
              <PivotTableColumnHeaders pivotTableComp={self} ref="colHeadersContainer"></PivotTableColumnHeaders> 
            </td>
            <td colSpan="2"></td>
          </tr>
          <tr>
            <td>
              <PivotTableRowHeaders pivotTableComp={self} ref="rowHeadersContainer"></PivotTableRowHeaders>
            </td>
            <td>
              <div className="inner-table-container data-cntr" ref="dataCellsContainer" onWheel={this.onWheel}>
                <PivotTableDataCells pivotTableComp={self} ref="dataCellsTable"></PivotTableDataCells>
              </div>
            </td>
            <td>
              <VerticalScrollBar pivotTableComp={self} ref="verticalScrollBar"></VerticalScrollBar>
            </td>
            <td></td>
          </tr>
          <tr>
            <td></td>
            <td>
              <HorizontalScrollBar pivotTableComp={self} ref="horizontalScrollBar"></HorizontalScrollBar>
            </td>
            <td colSpan="2"></td>
          </tr>
        </tbody>
      </table>
      <div className="orb-overlay orb-overlay-hidden" id={'drilldialog' + self.id}></div>
    </div>
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
  if(tblObject && tblObject.node) {

    var tbl = tblObject.node;
    var widthArray = [];

    for(var rowIndex = 0; rowIndex < tbl.rows.length ; rowIndex++) {
      // current row
      var currRow = tbl.rows[rowIndex];
      // reset widthArray index
      var arrayIndex = 0;
      var currWidth = null;

      // get the width of each cell within current row
      for(var cellIndex = 0; cellIndex < currRow.cells.length; cellIndex++) {
        // current cell
        var currCell = currRow.cells[cellIndex];

        if(currCell.__orb._visible) {
          // cell width
          //var cellwidth = Math.ceil(reactUtils.getSize(currCell.children[0]).width/currCell.colSpan);
          var cellwidth = Math.ceil((currCell.__orb._textWidth/currCell.__orb._colSpan) + currCell.__orb._paddingLeft + currCell.__orb._paddingRight + currCell.__orb._borderLeftWidth + currCell.__orb._borderRightWidth);
          // whether current cell spans vertically to the last row
          var rowsSpan = currCell.__orb._rowSpan > 1 && currCell.__orb._rowSpan >= tbl.rows.length - rowIndex;

          // if current cell spans over more than one column, add its width (its) 'colSpan' number of times
          for(var cspan = 0; cspan < currCell.__orb._colSpan; cspan++) {
            // If cell span over more than 1 row: insert its width into widthArray at arrayIndex
            // Else: either expand widthArray if necessary or replace the width if its smaller than current cell width

            currWidth = widthArray[arrayIndex];
            // skip inhibited widths (width that belongs to an upper cell than spans vertically to current row)
            while(currWidth && currWidth.inhibit > 0) {
              currWidth.inhibit--;
              arrayIndex++;
              currWidth = widthArray[arrayIndex];
            }

            if(widthArray.length - 1 < arrayIndex) {
              widthArray.push({
                width: cellwidth
              });
            } else if(cellwidth > widthArray[arrayIndex].width) {
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
      while(currWidth) {
        if(currWidth.inhibit > 0) {
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
  if(tblObject && tblObject.node) {

    // reset table width
    (tblObject.size = (tblObject.size || {})).width = 0;

    var tbl = tblObject.node;

    // for each row, set its cells width
    for(var rowIndex = 0; rowIndex < tbl.rows.length; rowIndex++) {
      
      // current row
      var currRow = tbl.rows[rowIndex];
      // index in newWidthArray
      var arrayIndex = 0;
      var currWidth = null;

      // set width of each cell
      for(var cellIndex = 0; cellIndex < currRow.cells.length; cellIndex++) {
        
        // current cell
        var currCell = currRow.cells[cellIndex];
        if(currCell.__orb._visible) {
          // cell width
          var newCellWidth = 0;
          // whether current cell spans vertically more than 1 row
          var rowsSpan = currCell.__orb._rowSpan > 1 && rowIndex < tbl.rows.length - 1;

          // current cell width is the sum of (its) "colspan" items in newWidthArray starting at 'arrayIndex'
          // 'arrayIndex' should be incremented by an amount equal to current cell 'colspan' but should also skip 'inhibited' cells
          for(var cspan = 0; cspan < currCell.__orb._colSpan; cspan++) {
            currWidth = newWidthArray[arrayIndex];
            // skip inhibited widths (width that belongs to an upper cell than spans vertically to current row)
            while(currWidth && currWidth.inhibit > 0) {
              currWidth.inhibit--;
              arrayIndex++;
              currWidth = newWidthArray[arrayIndex];
            }

            if(currWidth) {
              // add width of cells participating in the span
              newCellWidth += currWidth.width;
              // if current cell spans vertically more than 1 row, mark its width as inhibited for all cells participating in this span
              if(rowsSpan) {
                currWidth.inhibit = currCell.__orb._rowSpan - 1;
              }

              // advance newWidthArray index
              arrayIndex++;
            }
          }

          currCell.children[0].style.width = newCellWidth + 'px';

          // set table width (only in first iteration)
          if(rowIndex === 0) {
            var outerCellWidth = 0;
            if(currCell.__orb) {
              outerCellWidth = currCell.__orb._colSpan * (Math.ceil(currCell.__orb._paddingLeft + currCell.__orb._paddingRight + currCell.__orb._borderLeftWidth + currCell.__orb._borderRightWidth));
            }
            tblObject.size.width += newCellWidth + outerCellWidth;
          }
        }
      }

      // decrement inhibited state of all widths unsed in newWidthArray (not reached by current row cells)
      currWidth = newWidthArray[arrayIndex];
      while(currWidth) {
        if(currWidth.inhibit > 0) {
          currWidth.inhibit--;
        }
        arrayIndex++;
        currWidth = newWidthArray[arrayIndex];
      }
    }
  }
}

function clearTableWidths(tbl) {
  if(tbl) {
    for(var rowIndex = 0; rowIndex < tbl.rows.length; rowIndex++) {
      var row = tbl.rows[rowIndex];
      for(var cellIndex = 0; cellIndex < row.cells.length; cellIndex++) {
        row.cells[cellIndex].children[0].style.width = '';
      }
    }
    tbl.style.width = '';
  }
}