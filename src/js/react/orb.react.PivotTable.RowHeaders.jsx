/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableRowHeaders = react.createClass({
  render: function() {
    var self = this;
    var PivotRow = comps.PivotRow;

    var pgridwidget = this.props.pivotTableComp.pgridwidget;

    var rowHeaders = pgridwidget.rows.headers.map(function(headerRow, index) {
      return <PivotRow key={index}
                       topmost={index === 0}
                       row={headerRow}
                       pivotTableComp={self.props.pivotTableComp}>
      </PivotRow>;
    });

    return  <table className="inner-table">
        <tbody>
          {rowHeaders}
        </tbody>
      </table>;
  }
});