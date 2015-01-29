/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableDataCells = react.createClass({
  render: function() {
    var self = this;
    var PivotRow = comps.PivotRow;

    var pgridwidget = this.props.pivotTableComp.pgridwidget;

    var dataCells = pgridwidget.dataRows.map(function(dataRow, index) {
      return <PivotRow key={index}
                       topmost={index === 0}
                       row={dataRow}
                       pivotTableComp={self.props.pivotTableComp}>
      </PivotRow>;
    });

    return <table className="inner-table">
        <tbody>
          {dataCells}
        </tbody>
      </table>;
  }
});