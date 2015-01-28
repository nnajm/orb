/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableColumnHeaders = react.createClass({
  render: function() {
    var self = this;
    var PivotRow = comps.PivotRow;

    var pgridwidget = this.props.pivotTableComp.pgridwidget;

    var columnHeaders = pgridwidget.columns.headers.map(function(headerRow, index) {
      return <PivotRow key={index}
                       topmost={index === 0}
                       row={headerRow}
                       pivotTableComp={self.props.pivotTableComp}>
      </PivotRow>;
    });

    return  <table className="inner-table">
        <tbody>
          {columnHeaders}
        </tbody>
      </table>;
  }
});