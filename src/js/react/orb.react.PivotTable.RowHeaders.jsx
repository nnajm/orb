/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableRowHeaders = react.createClass({
  setColGroup: function(widths) {
    var node = this.getDOMNode();
    var colGroupNode = this.refs.colgroup.getDOMNode();
    node.style.tableLayout = 'auto';

    colGroupNode.innerHTML = '';
    for(var i = 0; i < widths.length; i++) {
      var col = document.createElement('col');
      col.style.width = (widths[i] + 8) + 'px';
      colGroupNode.appendChild(col);
    }
    node.style.tableLayout = 'fixed';
  },
  render: function() {
    var self = this;
    var PivotRow = comps.PivotRow;
    var pgridwidget = this.props.pivotTableComp.pgridwidget;
    var cntrClass = pgridwidget.rows.headers.length === 0 ? '' : ' rows-cntr';
    
    var layoutInfos = { 
      lastLeftMostCellVSpan: 0,
      topMostCells: {}
    };

    var rowHeaders = pgridwidget.rows.headers.map(function(headerRow, index) {
      return <PivotRow key={index}
                       row={headerRow}
                       axetype={axe.Type.ROWS}
                       layoutInfos={layoutInfos}
                       pivotTableComp={self.props.pivotTableComp}>
      </PivotRow>;
    });

    return  <div className={ 'inner-table-container' + cntrClass } ref="rowHeadersContainer" onWheel={this.props.pivotTableComp.onWheel}>
      <table className="inner-table">
        <colgroup ref="colgroup">
        </colgroup>
        <tbody>
          {rowHeaders}
        </tbody>
      </table>
    </div>;
  }
});