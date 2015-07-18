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
    this.synchronizeWidths();
  },
  componentDidMount: function() {
    var dataCellsNode = this.refs.dataCells.getDOMNode();
    var dataCellsTableNode = dataCellsNode.children[0];
    var colHeadersNode = this.refs.colHeaders.getDOMNode();
    var rowHeadersNode = this.refs.rowHeaders.getDOMNode();

    this.refs.horizontalScrollBar.setScrollClient(dataCellsNode, function(scrollPercent) {
      var scrollAmount = Math.ceil(
        scrollPercent * (
          reactUtils.getSize(dataCellsTableNode).width - 
          reactUtils.getSize(dataCellsNode).width
        )
      );
      colHeadersNode.scrollLeft = scrollAmount;
      dataCellsNode.scrollLeft = scrollAmount;
    });

    this.refs.verticalScrollBar.setScrollClient(dataCellsNode, function(scrollPercent) {
      var scrollAmount = Math.ceil(
        scrollPercent * (
          reactUtils.getSize(dataCellsTableNode).height - 
          reactUtils.getSize(dataCellsNode).height
        )
      );
      rowHeadersNode.scrollTop = scrollAmount;
      dataCellsNode.scrollTop = scrollAmount;
    });

    this.synchronizeWidths();
  },
  onWheel: function(e) {
    var elem;
    var scrollbar;
    var amount;

    if(e.currentTarget == (elem = this.refs.colHeaders.getDOMNode())) {
      scrollbar = this.refs.horizontalScrollBar;
      amount = e.deltaX || e.deltaY;
    } else if((e.currentTarget == (elem = this.refs.rowHeaders.getDOMNode())) ||
              (e.currentTarget == (elem = this.refs.dataCells.getDOMNode())) ) {
      scrollbar = this.refs.verticalScrollBar;
      amount = e.deltaY;
    }

    if(scrollbar && scrollbar.scroll(amount, e.deltaMode)) {
      e.stopPropagation();
      e.preventDefault();
    }
  },
  synchronizeWidths: function() {
    comps.SizingManager.synchronizeWidths(this);
    this.refs.horizontalScrollBar.refresh();
    this.refs.verticalScrollBar.refresh();
  },
  render: function() {

    var self = this;

    var config = this.pgridwidget.pgrid.config;
    var Toolbar = comps.Toolbar;
    var UpperButtons = comps.PivotTableUpperButtons;
    var ColumnButtons = comps.PivotTableColumnButtons;
    var RowButtons = comps.PivotTableRowButtons;
    var RowHeaders = comps.PivotTableRowHeaders;
    var ColumnHeaders = comps.PivotTableColumnHeaders;
    var DataCells = comps.PivotTableDataCells;
    var HorizontalScrollBar = comps.HorizontalScrollBar;
    var VerticalScrollBar = comps.VerticalScrollBar;

    var classes = config.theme.getPivotClasses();    

    var tblStyle = {};
    if(config.width) { tblStyle.width = config.width; }
    if(config.height) { tblStyle.height = config.height; }

    return (
    <div className={classes.container} style={tblStyle} ref="pivot">
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
          <tr ref="upperButtons">
            <td colSpan="4">
              <UpperButtons pivotTableComp={self}></UpperButtons>              
            </td>
          </tr>
          <tr ref="colButtons">
            <td></td>
            <td style={{padding: '11px 4px !important'}}>
              <ColumnButtons pivotTableComp={self}></ColumnButtons>
            </td>
            <td colSpan="2"></td>
          </tr>
          <tr>
            <td style={{ position: 'relative'}}>
              <RowButtons pivotTableComp={self} ref="rowButtons"></RowButtons>
            </td>
            <td>
              <ColumnHeaders pivotTableComp={self} ref="colHeaders"></ColumnHeaders> 
            </td>
            <td colSpan="2"></td>
          </tr>
          <tr>
            <td>
              <RowHeaders pivotTableComp={self} ref="rowHeaders"></RowHeaders>
            </td>
            <td>
              <DataCells pivotTableComp={self} ref="dataCells"></DataCells>
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