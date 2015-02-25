/** @jsx React.DOM */

/* global module, require, React */

'use strict';


module.exports.PivotRow = react.createClass({
  render: function() {
    var self = this;
    var PivotCell = comps.PivotCell;
    
    var lastCellIndex = this.props.row.length - 1;
    var cell0 = this.props.row[0];
    var firstVisibleCellFound = false;
    var lastLeftmostInfos = self.props.lastLeftmostInfos;
    var cells;

    var rowstyle = {};

    if(self.props.axetype === axe.Type.ROWS && cell0.visible && !cell0.visible()) {
      rowstyle.display = 'none';
    }

    cells = this.props.row.map(function(cell, index) {

      var isleftmostHeader = false;

      // If current cells are column headers and left most cell is not found yet
      // and last row left most cell does not span vertically over the current one and current one is visible 
      // then mark IT as the left most cell
      if(self.props.axetype === axe.Type.COLUMNS && !firstVisibleCellFound) {
        if(lastLeftmostInfos && lastLeftmostInfos.span === 0 && cell.visible()) {
          isleftmostHeader = firstVisibleCellFound = true;
          lastLeftmostInfos.span = cell.vspan() - 1;
        }
      }

      return <PivotCell key={index} 
                        cell={cell}
                        leftmostheader={isleftmostHeader}
                        pivotTableComp={self.props.pivotTableComp}>
             </PivotCell>;
    });

    if(lastLeftmostInfos && !firstVisibleCellFound) {
      lastLeftmostInfos.span--;
    }

    return (
      <tr style={rowstyle}>
        {cells}
      </tr>
    );
  }
});