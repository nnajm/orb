/** @jsx React.DOM */

/* global module, require, React */

'use strict';

var pivotId = 1;
var themeChangeCallbacks = {};

module.exports.PivotChart = react.createClass({
  id: pivotId++,
  pgrid: null,
  pgridwidget: null,
  fontStyle: null,
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
    var fontInfos = domUtils.getStyle(this.getDOMNode(), ['font-family', 'font-size'], true);
    this.fontStyle = {
      fontFamily: fontInfos[0], 
      fontSize: fontInfos[1]
    };

    this.synchronizeWidths();
  },
  synchronizeWidths: function() {
    var chartStyle = comps.SizingManager.synchronizeWidths(this);
    chartStyle.fontFamily = this.fontStyle.fontFamily;
    chartStyle.fontSize = this.fontStyle.fontSize;

    this.refs.chart.setState({
      canRender: true,
      chartStyle: chartStyle
    });
  },
  render: function() {

    var self = this;

    var config = this.pgridwidget.pgrid.config;
    var Toolbar = comps.Toolbar;
    var UpperButtons = comps.PivotTableUpperButtons;
    var ColumnButtons = comps.PivotTableColumnButtons;
    var RowButtons = comps.PivotTableRowButtons;
    var Chart = comps.Chart;

    var classes = config.theme.getPivotClasses();    

    var tblStyle = {};
    if(config.width) { tblStyle.width = config.width; }
    if(config.height) { tblStyle.height = config.height; }

    return (<div className={classes.container} style={tblStyle} ref="pivot">
      <table id={'tbl-' + self.id} ref="pivotWrapperTable" className={classes.table}>
        <colgroup>
          <col ref="column1"></col>
          <col ref="column2"></col>
        </colgroup>
        <tbody>
          <tr ref="upperButtons">
            <td colSpan="2">
              <UpperButtons pivotTableComp={self}></UpperButtons>              
            </td>
          </tr>
          <tr ref="colButtons">
            <td></td>
            <td style={{padding: '11px 4px !important'}}>
              <ColumnButtons pivotTableComp={self}></ColumnButtons>
            </td>
          </tr>
          <tr>
            <td style={{ position: 'relative'}}>
              <RowButtons pivotTableComp={self} ref="rowButtons"></RowButtons>
            </td>
            <td>
              <Chart pivotTableComp={self} chartMode={config.chartMode} ref="chart"></Chart>
            </td>
          </tr>
        </tbody>
      </table>
    </div>);
  }
});