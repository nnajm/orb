/** @jsx React.DOM */

/* global module, require, React */

'use strict';

var pivotId = 1;
var themeChangeCallbacks = {};

function getAllColumnsWidth(tblObject, onlyLastRow) {
  if(tblObject && tblObject.node) {

    var tbl = tblObject.node;
    var widthArray = [];
    var startRow = onlyLastRow ? tbl.rows.length - 1 : 0;

    for(var rowIndex = startRow; rowIndex < tbl.rows.length; rowIndex++) {
      var row = tbl.rows[rowIndex];
      for(var colIndex = 0; colIndex < row.cells.length; colIndex++) {
        var brect = row.cells[colIndex].getBoundingClientRect();
        var colwidth = brect.right - brect.left;
        if(widthArray.length - 1 < colIndex) {
          widthArray.push(colwidth);
        } else if(colwidth > widthArray[colIndex]) {
          widthArray[colIndex] = colwidth;
        }
      }
    }

    tblObject.widthArray = widthArray;
  }
}

function setTableWidths(tblObject, onlyLastRow) {
  if(tblObject && tblObject.node) {
    tblObject.width = 0;
    var tbl = tblObject.node;
    var startRow = onlyLastRow ? tbl.rows.length - 1 : 0;

    for(var rowIndex = startRow; rowIndex < tbl.rows.length; rowIndex++) {
      var row = tbl.rows[rowIndex];
      for(var colIndex = 0; colIndex < row.cells.length; colIndex++) {
        row.cells[colIndex].style.width = tblObject.widthArray[colIndex] + 'px';
        if(rowIndex === startRow) {
          tblObject.width += tblObject.widthArray[colIndex];
        }
      }
    }

    tbl.style.width = tblObject.width + 'px';
  }
}

function clearTableWidths(tbl) {
  if(tbl) {
    for(var rowIndex = 0; rowIndex < tbl.rows.length; rowIndex++) {
      var row = tbl.rows[rowIndex];
      for(var colIndex = 0; colIndex < row.cells.length; colIndex++) {
        row.cells[colIndex].style.width = '';
      }
    }
    tbl.style.width = '';
  }
}

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
    this.pgridwidget.moveField(button.props.field.name, button.props.axetype, newAxeType, position);
    this.setProps({});
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
    this.optimizeColumnsWidth();
  },
  componentDidMount: function() {
    this.optimizeColumnsWidth();

    var dataCellsContainer = this.refs.dataCellsContainer.getDOMNode();
    var colHeadersTable = this.refs.colHeadersTable.getDOMNode();
    var rowHeadersTable = this.refs.rowHeadersTable.getDOMNode();

    dataCellsContainer.addEventListener('scroll', function() {
      colHeadersTable.style.marginLeft = -dataCellsContainer.scrollLeft + 'px';
      rowHeadersTable.style.marginTop = -dataCellsContainer.scrollTop + 'px';
    });
  },
  optimizeColumnsWidth: function() {
    var self = this;

    var pivotWrapperTable = self.refs.pivotWrapperTable.getDOMNode();
    var column1 = self.refs.column1.getDOMNode();
    var column2 = self.refs.column2.getDOMNode();
    var column3 = self.refs.column3.getDOMNode();

    pivotWrapperTable.style.tableLayout = 'auto';
    column1.style.width = '';
    column2.style.width = '';
    column3.style.width = '';

    var nodes = (function() {
      var nds = {};
      ['pivotContainer', 'dataCellsContainer', 'dataCellsTable', 'upperbuttonsRow', 'columnbuttonsRow',
       'colHeadersTable', 'rowHeadersTable', 'rowHeadersContainer', 'horizontalScrollBar'].forEach(function(refname) {
        nds[refname] = {
          node: self.refs[refname].getDOMNode()
        };
        nds[refname].size = reactUtils.getSize(nds[refname].node);
      });
      return nds;
    }());

    // clear table widths
    clearTableWidths(nodes.dataCellsTable.node);
    clearTableWidths(nodes.colHeadersTable.node);

    // clear data cells container width
    nodes.dataCellsContainer.node.style.width = '';

    // get array of dataCellsTable column widths
    getAllColumnsWidth(nodes.dataCellsTable);
    // get array of colHeadersTable column widths
    getAllColumnsWidth(nodes.colHeadersTable, true);

    // get the array of max widths between dataCellsTable and colHeadersTable
    var maxWidthArray = [];

    for(var i = 0; i < nodes.dataCellsTable.widthArray.length; i++) {
      var dataCellWidth = nodes.dataCellsTable.widthArray[i];
      var colHeaderWidth = nodes.colHeadersTable.widthArray[i];
      maxWidthArray.push(
        dataCellWidth < colHeaderWidth ?
          colHeaderWidth :
          dataCellWidth
      );
    }

    // Set dataCellsTable cells widths according to the computed maxWidthArray
    setTableWidths(nodes.dataCellsTable);
    // Set colHeadersTable last row cells widths according to the computed maxWidthArray
    setTableWidths(nodes.colHeadersTable, true);

    // Adjust data cells container width
    nodes.dataCellsContainer.node.style.width = (nodes.pivotContainer.size.width - nodes.rowHeadersTable.size.width - 16) + 'px';

    // Adjust data cells container height
    var dataCellsTableHeight = 
      nodes.pivotContainer.size.height -
      nodes.upperbuttonsRow.size.height -
      nodes.columnbuttonsRow.size.height -
      nodes.colHeadersTable.size.height -
      nodes.horizontalScrollBar.size.height - 3;

    nodes.dataCellsContainer.node.style.height = dataCellsTableHeight + 'px';
    nodes.rowHeadersContainer.node.style.height = dataCellsTableHeight + 'px';

    column1.style.width = nodes.rowHeadersTable.size.width + 'px';
    column2.style.width = nodes.dataCellsContainer.node.style.width + 'px';
    column3.style.width = '16px';
    pivotWrapperTable.style.tableLayout = 'fixed';

    this.refs.horizontalScrollBar.setScrollComps(nodes.dataCellsContainer.node, nodes.dataCellsTable.node, function(scrollPercent) {
      var scrollAmount = scrollPercent * (reactUtils.getSize(nodes.dataCellsTable.node).width - reactUtils.getSize(nodes.dataCellsContainer.node).width);
      nodes.colHeadersTable.node.style.marginLeft = -scrollAmount + 'px';
      nodes.dataCellsTable.node.style.marginLeft = -scrollAmount + 'px';
    });

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

    var classes = config.theme.getPivotClasses();    

    var tblStyle = {};
    if(config.width) { tblStyle.width = config.width; }
    if(config.height) { tblStyle.height = config.height; }

    return (
    <div className={classes.container} style={tblStyle} ref="pivotContainer">
      <div className="orb-toolbar" style={{ display: config.showToolbar ? 'block' : 'none' }}>
        <Toolbar pivotTableComp={self}></Toolbar>
      </div>
      <table id={'tbl-' + self.id}  ref="pivotWrapperTable" className={classes.table} style={{width: '100%', tableLayout: 'fixed'}}>
        <colgroup>
          <col ref="column1"></col>
          <col ref="column2"></col>
          <col ref="column3"></col>
        </colgroup>
        <tbody>
          <tr ref="upperbuttonsRow">
            <td colSpan="3">
              <PivotTableUpperButtons pivotTableComp={self}></PivotTableUpperButtons>              
            </td>
          </tr>
          <tr ref="columnbuttonsRow">
            <td></td>
            <td colSpan="2">
              <PivotTableColumnButtons pivotTableComp={self}></PivotTableColumnButtons>
            </td>
          </tr>
          <tr>
            <td>
              <PivotTableRowButtons pivotTableComp={self}></PivotTableRowButtons>
            </td>
            <td style={{ overflow: 'hidden' }}>
              <PivotTableColumnHeaders pivotTableComp={self} ref="colHeadersTable"></PivotTableColumnHeaders>
            </td>
            <td></td>
          </tr>
          <tr>
            <td className="cell-topmost" style={{ overflow: 'hidden' }}>
              <div className="inner-table-container" ref="rowHeadersContainer">
                <PivotTableRowHeaders pivotTableComp={self} ref="rowHeadersTable"></PivotTableRowHeaders>
              </div>
            </td>
            <td>
              <div className="inner-table-container" ref="dataCellsContainer">
                <PivotTableDataCells pivotTableComp={self} ref="dataCellsTable"></PivotTableDataCells>
              </div>
            </td>
            <td>
              
            </td>
          </tr>
          <tr>
            <td></td>
            <td>
              <HorizontalScrollBar ref="horizontalScrollBar"></HorizontalScrollBar>
            </td>
            <td></td>
          </tr>
        </tbody>
      </table>
      <div className="orb-overlay orb-overlay-hidden" id={'drilldialog' + self.id}></div>
    </div>
    );
  }
});