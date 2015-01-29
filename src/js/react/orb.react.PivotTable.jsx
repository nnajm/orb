/** @jsx React.DOM */

/* global module, require, React */

'use strict';

var pivotId = 1;
var themeChangeCallbacks = {};

function getAllColumnsWidth(tbl, onlyLastRow) {
  var w = [];
  var startRow = onlyLastRow ? tbl.rows.length - 1 : 0;

  for(var rowIndex = startRow; rowIndex < tbl.rows.length; rowIndex++) {
    var row = tbl.rows[rowIndex];
    for(var colIndex = 0; colIndex < row.cells.length; colIndex++) {
      var brect = row.cells[colIndex].getBoundingClientRect();
      var cw = brect.right - brect.left;
      if(w.length - 1 < colIndex) {
        w.push(cw);
      } else if(cw > w[colIndex]) {
        w[colIndex] = cw;
      }
    }
  }

  return w;
}

function setAllColumnsWidth(tbl, w, onlyLastRow) {
  var startRow = onlyLastRow ? tbl.rows.length - 1 : 0;

  for(var rowIndex = startRow; rowIndex < tbl.rows.length; rowIndex++) {
    var row = tbl.rows[rowIndex];
    for(var colIndex = 0; colIndex < row.cells.length; colIndex++) {
      row.cells[colIndex].style.width = w[colIndex] + 'px';
    }
  }
}

function clearAllColumnsWidth(tbl) {
  for(var rowIndex = 0; rowIndex < tbl.rows.length; rowIndex++) {
    var row = tbl.rows[rowIndex];
    for(var colIndex = 0; colIndex < row.cells.length; colIndex++) {
      row.cells[colIndex].style.width = '';
    }
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
    var pivotContainerTable = this.refs.pivotContainerTable.getDOMNode();
    var dataCellsContainer = this.refs.dataCellsContainer.getDOMNode();
    var dataCellsTable = this.refs.dataCellsTable.getDOMNode();
    var colHeadersTable = this.refs.colHeadersTable.getDOMNode();
    var rowHeadersTable = this.refs.rowHeadersTable.getDOMNode();

    clearAllColumnsWidth(dataCellsTable, maxWidth);
    clearAllColumnsWidth(colHeadersTable, maxWidth);
    dataCellsTable.style.width = '';
    colHeadersTable.style.width = '';
    dataCellsContainer.style.width = '';

    var dataCellsTableWidth = getAllColumnsWidth(dataCellsTable);
    var colHeadersTableWidth = getAllColumnsWidth(colHeadersTable, true);
    var maxWidth = [];
    var tableWidth = 0;
    for(var i = 0; i < dataCellsTableWidth.length; i++) {
      if(dataCellsTableWidth[i] < colHeadersTableWidth[i]) {
        maxWidth.push(colHeadersTableWidth[i]);
        tableWidth += colHeadersTableWidth[i];
      } else {
        maxWidth.push(dataCellsTableWidth[i]);
        tableWidth += dataCellsTableWidth[i];
      }
    }

    setAllColumnsWidth(dataCellsTable, maxWidth);
    setAllColumnsWidth(colHeadersTable, maxWidth, true);

    dataCellsTable.style.width = tableWidth + 'px';
    colHeadersTable.style.width = tableWidth + 'px';

    var pivotSize = reactUtils.getSize(pivotContainerTable);
    var rowHeadersSize = reactUtils.getSize(rowHeadersTable);

    var maxContainerWidth = pivotSize.width - rowHeadersSize.width;
    if(maxContainerWidth > tableWidth) {
      dataCellsContainer.style.width = (tableWidth + 13) + 'px';
    } else {
      dataCellsContainer.style.width = maxContainerWidth + 'px';
    }

    var dataCellsTableSize = reactUtils.getSize(dataCellsTable);
    var upperbuttonsRowSize = reactUtils.getSize(this.refs.upperbuttonsRow.getDOMNode());
    var columnbuttonsRowSize = reactUtils.getSize(this.refs.columnbuttonsRow.getDOMNode());
    var colHeadersTableSize = reactUtils.getSize(colHeadersTable);
    var maxContainerHeight = pivotSize.height - upperbuttonsRowSize.height - columnbuttonsRowSize.height - colHeadersTableSize.height;
    if(maxContainerHeight > dataCellsTableSize.height) {
      dataCellsContainer.style.height = (dataCellsTableSize.height + 13) + 'px';
    } else {
      dataCellsContainer.style.height = maxContainerHeight + 'px';
    }    
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

    var classes = config.theme.getPivotClasses();    

    var tblStyle = {};
    if(config.width) { tblStyle.width = config.width; }
    if(config.height) { tblStyle.height = config.height; }

    return (
    <div className={classes.container} style={tblStyle} ref="pivotContainerTable">
      <div className="orb-toolbar" style={{ display: config.showToolbar ? 'block' : 'none' }}>
        <Toolbar pivotTableComp={self}></Toolbar>
      </div>
      <table id={'tbl-' + self.id} className={classes.table} style={{width: '100%'}}>
        <tbody>
          <tr ref="upperbuttonsRow">
            <td colSpan="2">
              <PivotTableUpperButtons pivotTableComp={self}></PivotTableUpperButtons>              
            </td>
          </tr>
          <tr ref="columnbuttonsRow">
            <td></td>
            <td>
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
          </tr>
          <tr>
            <td className="cell-topmost" style={{ overflow: 'hidden' }}>
              <PivotTableRowHeaders pivotTableComp={self} ref="rowHeadersTable"></PivotTableRowHeaders>
            </td>
            <td>
              <div className="datacells-container" ref="dataCellsContainer">
                <PivotTableDataCells pivotTableComp={self} ref="dataCellsTable"></PivotTableDataCells>
              </div>
            </td>
          </tr>
        </tbody>
      </table>
      <div className="orb-overlay orb-overlay-hidden" id={'drilldialog' + self.id}></div>
    </div>
    );
  }
});