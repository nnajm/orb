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
      return <PivotRow key={index}
                       row={dataRow}
                       axetype={axe.Type.DATA}
                       layoutInfos={layoutInfos}
                       pivotTableComp={self.props.pivotTableComp}>
      </PivotRow>;
    });

    return <table className="inner-table">
        <colgroup>
        </colgroup>
        <tbody>
          {dataCells}
        </tbody>
      </table>;
  }
});