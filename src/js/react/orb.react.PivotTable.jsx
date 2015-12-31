/* global module, require, React */

'use strict';

var React = typeof window === 'undefined' ? require('react') : window.React,
    ReactDOM = typeof window === 'undefined' ? require('react-dom') : window.ReactDOM,
    DragManager = require('./orb.react.DragManager.jsx'),
    SizingManager = require('./orb.react.PivotTable.SizingManager.jsx'),
    Toolbar = require('./orb.react.Toolbar.jsx'),
    UpperButtons =  require('./orb.react.PivotTable.UpperButtons.jsx'),
    ColumnButtons = require('./orb.react.PivotTable.ColumnButtons.jsx'),
    RowButtons = require('./orb.react.PivotTable.RowButtons.jsx'),    
    RowHeaders = require('./orb.react.PivotTable.RowHeaders.jsx'),
    ColumnHeaders = require('./orb.react.PivotTable.ColumnHeaders.jsx'),
    DataCells = require('./orb.react.PivotTable.DataCells.jsx'),
    ScrollBars = require('./orb.react.ScrollBars.jsx'),
    HorizontalScrollBar = ScrollBars.HorizontalScrollBar,
    VerticalScrollBar = ScrollBars.VerticalScrollBar,

    utils = require('../orb.utils'),
    domUtils = require('../orb.utils.dom'),

    pivotId = 1,
    themeChangeCallbacks = {};

module.exports = React.createClass({
  id: pivotId++,
  pgrid: null,
  pgridwidget: null,
  fontStyle: null,
  getInitialState: function() {
    DragManager.init(this);
    
    themeChangeCallbacks[this.id] = [];
    this.registerThemeChanged(this.updateClasses);

    this.pgridwidget = this.props.pgridwidget;
    this.pgrid = this.pgridwidget.pgrid;
    return {};
  },
  sort: function(axetype, field) {
    this.pgridwidget.sort(axetype, field);
  },
  moveButton: function(button, newAxeType, position) {
    this.pgridwidget.moveField(button.props.field.name, button.props.axetype, newAxeType, position);
  },
  toggleSubtotals: function(axetype) {
    this.pgridwidget.toggleSubtotals(axetype);
  },
  toggleGrandtotal: function(axetype) {
    this.pgridwidget.toggleGrandtotal(axetype);
  },
  applyFilter: function(fieldname, operator, term, staticValue, excludeStatic) {
    this.pgridwidget.applyFilter(fieldname, operator, term, staticValue, excludeStatic);
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
      var thisnode = ReactDOM.findDOMNode(this);
      var classes = this.pgridwidget.pgrid.config.theme.getPivotClasses();    
      thisnode.className = classes.container;
      thisnode.children[1].className = classes.table;
  },
  componentDidUpdate: function() {    
    this.synchronizeWidths();
  },
  componentDidMount: function() {
    var fontInfos = domUtils.getStyle(ReactDOM.findDOMNode(this), ['font-family', 'font-size'], true);
    this.fontStyle = {
      fontFamily: fontInfos[0], 
      fontSize: fontInfos[1]
    };

    var dataCellsNode = ReactDOM.findDOMNode(this.refs.dataCells);
    var dataCellsTableNode = dataCellsNode.children[0];
    var colHeadersNode = ReactDOM.findDOMNode(this.refs.colHeaders);
    var rowHeadersNode = ReactDOM.findDOMNode(this.refs.rowHeaders);

    this.refs.horizontalScrollBar.setScrollClient(dataCellsNode, function(scrollPercent) {
      var scrollAmount = Math.ceil(
        scrollPercent * (
          domUtils.getSize(dataCellsTableNode).width - 
          domUtils.getSize(dataCellsNode).width
        )
      );
      colHeadersNode.scrollLeft = scrollAmount;
      dataCellsNode.scrollLeft = scrollAmount;
    });

    this.refs.verticalScrollBar.setScrollClient(dataCellsNode, function(scrollPercent) {
      var scrollAmount = Math.ceil(
        scrollPercent * (
          domUtils.getSize(dataCellsTableNode).height - 
          domUtils.getSize(dataCellsNode).height
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

    if(e.currentTarget == (elem = ReactDOM.findDOMNode(this.refs.colHeaders))) {
      scrollbar = this.refs.horizontalScrollBar;
      amount = e.deltaX || e.deltaY;
    } else if ((e.currentTarget == (elem = ReactDOM.findDOMNode(this.refs.rowHeaders))) ||
              (e.currentTarget == (elem = ReactDOM.findDOMNode(this.refs.dataCells))) ) {
      scrollbar = this.refs.verticalScrollBar;
      amount = e.deltaY;
    }

    if(scrollbar && scrollbar.scroll(amount, e.deltaMode)) {
      utils.stopPropagation(e);
      utils.preventDefault(e);
    }
  },
  synchronizeWidths: function() {
    SizingManager.synchronizeWidths(this);
    this.refs.horizontalScrollBar.refresh();
    this.refs.verticalScrollBar.refresh();
  },
  render: function() {

    var self = this;

    var config = this.pgridwidget.pgrid.config;
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