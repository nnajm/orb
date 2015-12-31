/* global module, require, React */

'use strict';

var React = typeof window === 'undefined' ? require('react') : window.React,
    axe = require('../orb.axe'),
    PivotRow = require('./orb.react.PivotRow.jsx');

module.exports = React.createClass({
  render: function() {
    var self = this;
    var pgridwidget = this.props.pivotTableComp.pgridwidget;
    var layoutInfos = { 
      lastLeftMostCellVSpan: 0,
      topMostCells: {}
    };

    var dataCells = pgridwidget.dataRows.map(function(dataRow, index) {
      return <PivotRow key={index}
                       row={dataRow}
                       axetype={axe.Type.DATA}
                       layoutInfos={layoutInfos}
                       pivotTableComp={self.props.pivotTableComp}>
      </PivotRow>;
    });

    return <div className="inner-table-container data-cntr" onWheel={this.props.pivotTableComp.onWheel}>
        <table className="inner-table">
            <colgroup>
            </colgroup>
            <tbody>
              {dataCells}
            </tbody>
          </table>
      </div>;
  }
});