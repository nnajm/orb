/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableColumnHeaders = react.createClass({
  render: function() {
    var self = this;
    var PivotRow = comps.PivotRow;
    var pgridwidget = this.props.pivotTableComp.pgridwidget;
    var cntrClass = pgridwidget.columns.headers.length === 0 ? '' : ' columns-cntr';

    var layoutInfos = { 
      lastLeftMostCellVSpan: 0,
      topMostCells: {}
    };

    var columnHeaders = pgridwidget.columns.headers.map(function(headerRow, index) {
      return <PivotRow key={index}
                       row={headerRow}
                       axetype={axe.Type.COLUMNS}
                       pivotTableComp={self.props.pivotTableComp}
                       layoutInfos={layoutInfos}>
      </PivotRow>;
    });              

    return  <div className={'inner-table-container' + cntrClass} ref="colHeadersContainer" onWheel={this.props.pivotTableComp.onWheel}>
      <table className="inner-table">
        <colgroup>
        </colgroup>
        <tbody>
          {columnHeaders}
        </tbody>
      </table>
    </div>;
  }
});