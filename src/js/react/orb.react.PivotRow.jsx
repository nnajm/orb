/** @jsx React.DOM */

/* global module, require, React */

'use strict';


module.exports.PivotRow = react.createClass({
  render: function() {
    var self = this;
    var PivotCell = comps.PivotCell;
    
    var lastCellIndex = this.props.row.length - 1;
    var cell0 = this.props.row[0];
    var cells;

    var rowstyle = {};

    if(this.props.rowButtonsCell !== undefined) {
      cells = this.props.row.slice(this.props.rowButtonsCount).map(function(cell, index) {
        var isrightmost = index === (lastCellIndex - self.props.rowButtonsCount);
        var isleftmostHeader = index === 0;
        return <PivotCell key={index} 
                          cell={cell}
                          topmost={self.props.topmost}
                          rightmost={isrightmost}
                          leftmostheader={isleftmostHeader}
                          pivotTableComp={self.props.pivotTableComp}>
               </PivotCell>;
      });

      return (
        <tr>
          {this.props.rowButtonsCell}
          {cells}
        </tr>
      );

    } else {

      if(cell0.template == 'cell-template-row-header' && cell0.visible && !cell0.visible()) {
        rowstyle.display = 'none';
      }

      cells = this.props.row.map(function(cell, index) {
        var isrightmost = index === lastCellIndex;
        var isleftmost = index === 0 && (
                           cell.type === uiheaders.HeaderType.EMPTY ||
                           (cell.type === uiheaders.HeaderType.SUB_TOTAL && cell.dim.parent.isRoot) || 
                           (cell.type === uiheaders.HeaderType.GRAND_TOTAL) || 
                           (cell.dim && (cell.dim.isRoot || cell.dim.parent.isRoot))
                         );
        var isleftmostHeader = cell.template === 'cell-template-column-header' && index === 1;
        var isleftmostDataValue = cell.template === 'cell-template-datavalue' && ((index === 0 &&  cell.visible()) || (index > 0 && !self.props.row[index - 1].visible()));

        return <PivotCell key={index} 
                          cell={cell}
                          topmost={self.props.topmost}
                          leftmostheader={isleftmostHeader}
                          leftmostdatavalue={isleftmostDataValue}
                          rightmost={isrightmost}
                          leftmost={isleftmost}
                          pivotTableComp={self.props.pivotTableComp}>
               </PivotCell>;
      });

      return (
        <tr style={rowstyle}>
          {cells}
        </tr>
      );
    }
  }
});