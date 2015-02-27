/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableColumnHeaders = react.createClass({
  render: function() {
    var self = this;
    var PivotRow = comps.PivotRow;

    var pgridwidget = this.props.pivotTableComp.pgridwidget;
    var layoutInfos = { 
      lastLeftMostCellVSpan: 0,
      topMostRowFound: false
    };

    var columnHeaders = pgridwidget.columns.headers.map(function(headerRow, index) {
      return <PivotRow key={index}
                       row={headerRow}
                       axetype={axe.Type.COLUMNS}
                       pivotTableComp={self.props.pivotTableComp}
                       layoutInfos={layoutInfos}>
      </PivotRow>;
    });

    return  <table className="inner-table">
        <tbody>
          {columnHeaders}
        </tbody>
      </table>;
  }
});