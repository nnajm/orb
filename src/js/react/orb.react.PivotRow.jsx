/** @jsx React.DOM */

/* global module, require, React */

'use strict';


module.exports.PivotRow = react.createClass({
  render: function() {
    var self = this;
    var PivotCell = comps.PivotCell;
    
    var lastCellIndex = this.props.row.length - 1;
    var cell0 = this.props.row[0];
    var leftmostCellFound = false;
    var layoutInfos = self.props.layoutInfos;
    var cells;

    var rowstyle = {};

    var istopmost = false;

    cells = this.props.row.map(function(cell, index) {

      var isleftmost = false;

      // If current cells are column/data headers and left most cell is not found yet
      // and last row left most cell does not span vertically over the current one and current one is visible 
      // then mark IT as the left most cell
      if(cell.visible() && layoutInfos) {
        if(cell.dim) {
          if((cell.dim.isRoot && layoutInfos.topMostCells[cell.dim.depth - 1] === undefined) || (!cell.dim.isRoot && layoutInfos.topMostCells[cell.dim.depth] === undefined && (cell.dim.parent.isRoot || layoutInfos.topMostCells[cell.dim.depth + 1] === cell.dim.parent))) {
            istopmost = true;
            layoutInfos.topMostCells[cell.dim.depth] = cell.dim;
          }
        } else if(!layoutInfos.topMostCells['0']) {
          istopmost = layoutInfos.topMostCells['0'] = true;
        }

        if(!leftmostCellFound && (self.props.axetype === axe.Type.DATA || self.props.axetype === axe.Type.COLUMNS) &&
            layoutInfos.lastLeftMostCellVSpan === 0) {

          isleftmost = leftmostCellFound = true;
          layoutInfos.lastLeftMostCellVSpan = cell.vspan() - 1;
        }
      }

      return <PivotCell key={index} 
                        cell={cell}
                        leftmost={isleftmost}
                        topmost={istopmost}
                        pivotTableComp={self.props.pivotTableComp}>
             </PivotCell>;
    });

    // decrement lastLeftMostCellVSpan
    if(layoutInfos && layoutInfos.lastLeftMostCellVSpan > 0 && !leftmostCellFound) {
      layoutInfos.lastLeftMostCellVSpan--;
    }

    return (
      <tr style={rowstyle}>
        {cells}
      </tr>
    );
  }
});