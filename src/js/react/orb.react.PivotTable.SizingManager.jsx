/** @jsx React.DOM */

/* global module, domUtils */

'use strict';

module.exports.SizingManager = {
  synchronizeWidths: function(pivotComp) {

    var pivotWrapperTable = pivotComp.refs.pivotWrapperTable.getDOMNode(),
        pivot = new ComponentSizeInfo(pivotComp.refs.pivot),
        toolbar = new ComponentSizeInfo(pivotComp.refs.toolbar),
        cHeadersTbl = new ComponentSizeInfo(pivotComp.refs.colHeaders, true, 'table'),
        rHeadersTbl = new ComponentSizeInfo(pivotComp.refs.rowHeaders, true, 'table'),
        dataCellsTbl = new ComponentSizeInfo(pivotComp.refs.dataCells, true, 'table'),
        topBtns = new ComponentSizeInfo(pivotComp.refs.upperButtons),
        cBtns = new ComponentSizeInfo(pivotComp.refs.colButtons),
        rBtnsTbl = new ComponentSizeInfo(pivotComp.refs.rowButtons, true),
        hScroll = new ComponentSizeInfo(pivotComp.refs.horizontalScrollBar),
        vScroll = new ComponentSizeInfo(pivotComp.refs.verticalScrollBar),

        dataCellsWidths = dataCellsTbl.getLargestWidths(cHeadersTbl),
        rHeadersWidth = Math.max(rHeadersTbl.w, rBtnsTbl.w, 67),
        dataCellsContainerWidth = Math.min(dataCellsWidths.total + 1, pivot.w - rHeadersWidth - vScroll.w),

        pivotHeight = pivotComp.pgridwidget.pgrid.config.height,
        dataCellsRemHeight = !pivotHeight ? null : (pivotHeight - (toolbar ? toolbar.h + 17 : 0) - (topBtns.h + cBtns.h + cHeadersTbl.h + hScroll.h)), 
        dataCellsTableHeight = !dataCellsRemHeight ? null : Math.ceil(Math.min(dataCellsRemHeight, dataCellsTbl.h));


    // get rowHeaders table width to match with rowButtons table width
    rHeadersTbl.addToWidth(rHeadersWidth - rHeadersTbl.w);

    // Set dataCellsTable cells widths according to the computed dataCellsWidths
    domUtils.updateTableColGroup(dataCellsTbl.node, dataCellsWidths.max);

    // Set colHeadersTable cells widths according to the computed dataCellsWidths
    domUtils.updateTableColGroup(cHeadersTbl.node, dataCellsWidths.max);

    // Set rowHeadersTable cells widths
    domUtils.updateTableColGroup(rHeadersTbl.node, rHeadersTbl.colWidths);

    dataCellsTbl.setStyle('width', dataCellsWidths.total);
    cHeadersTbl.setStyle('width', dataCellsWidths.total);
    rHeadersTbl.setStyle('width', rHeadersWidth);

    // Adjust data cells container and column headers container width
    dataCellsTbl.setParentStyle('width', dataCellsContainerWidth);
    cHeadersTbl.setParentStyle('width', dataCellsContainerWidth);

    if(dataCellsTableHeight) {
      // Adjust data cells container and row headers container height
      dataCellsTbl.setParentStyle('height', dataCellsTableHeight);
      rHeadersTbl.setParentStyle('height', dataCellsTableHeight);
    }

    // set pivotWrapperTable columns width to fixed value
    domUtils.updateTableColGroup(pivotWrapperTable, [
        rHeadersWidth,
        dataCellsContainerWidth,
        vScroll.w,
        Math.max(pivot.w - (rHeadersWidth + dataCellsContainerWidth + vScroll.w), 0)
    ]);

    pivotComp.refs.horizontalScrollBar.refresh();
    pivotComp.refs.verticalScrollBar.refresh();
  }
};

function ComponentSizeInfo(component, isWrapper, childType) {
  var self = this,
      node = component.getDOMNode(),
      size;

  this.node = isWrapper ? node.children[0] : node;

  size = domUtils.getSize(this.node);
  this.w = size.width;
  this.h = size.height;
  
  this.setStyle = function(styleProp, value) {
    self.node.style[styleProp] = value + 'px';
  };

  this.setParentStyle = function(styleProp, value) {
    self.node.parentNode.style[styleProp] = value + 'px';
  };

  this.getLargestWidths = function(otherCompInfo) {
    var result = {
      max: [],
      total: 0
    };

    // get the array of max widths between dataCellsTable and colHeadersTable
    for(var i = 0; i < self.colWidths.length; i++) {
      result.max.push(Math.max(self.colWidths[i], otherCompInfo.colWidths[i]));
      result.total += result.max[i];
    }

    return result;
  };

  this.addToWidth = function(value) {
    if(value > 0) {
      self.w += value;
      self.colWidths[self.colWidths.length - 1] += value;
    }
  };

  if(childType === 'table') {
    // get array of column widths
    getAllColumnsWidth(this);
  }
}

/**
 * Gets the width of all columns (maximum width of all column cells) of a html table element
 * @param  {Object}  tblObject - object having a table element in its 'node' property
 * @returns {Array} An array of numeric values representing the width of each column.
 *                  Its length is equal to the greatest number of cells of all rows
 *                  (in case of cells having colSpan/rowSpan greater than 1.)
 */
function getAllColumnsWidth(tblObject) {
  if(tblObject && tblObject.node) {

    var tbl = tblObject.node;
    var colWidths = [];

    for(var rowIndex = 0; rowIndex < tbl.rows.length ; rowIndex++) {
      // current row
      var currRow = tbl.rows[rowIndex];
      // reset colWidths index
      var arrayIndex = 0;
      var currWidth = null;

      // get the width of each cell within current row
      for(var cellIndex = 0; cellIndex < currRow.cells.length; cellIndex++) {
        // current cell
        var currCell = currRow.cells[cellIndex];

        if(currCell.__orb._visible) {
          // cell width
          //var cellwidth = Math.ceil(domUtils.getSize(currCell.children[0]).width/currCell.colSpan);
          var cellwidth = Math.ceil((currCell.__orb._textWidth/currCell.__orb._colSpan) + currCell.__orb._paddingLeft + currCell.__orb._paddingRight + currCell.__orb._borderLeftWidth + currCell.__orb._borderRightWidth);
          // whether current cell spans vertically to the last row
          var rowsSpan = currCell.__orb._rowSpan > 1 && currCell.__orb._rowSpan >= tbl.rows.length - rowIndex;

          // if current cell spans over more than one column, add its width (its) 'colSpan' number of times
          for(var cspan = 0; cspan < currCell.__orb._colSpan; cspan++) {
            // If cell span over more than 1 row: insert its width into colWidths at arrayIndex
            // Else: either expand colWidths if necessary or replace the width if its smaller than current cell width

            currWidth = colWidths[arrayIndex];
            // skip inhibited widths (width that belongs to an upper cell than spans vertically to current row)
            while(currWidth && currWidth.inhibit > 0) {
              currWidth.inhibit--;
              arrayIndex++;
              currWidth = colWidths[arrayIndex];
            }

            if(colWidths.length - 1 < arrayIndex) {
              colWidths.push({
                width: cellwidth
              });
            } else if(cellwidth > colWidths[arrayIndex].width) {
              colWidths[arrayIndex].width = cellwidth;
            }

            colWidths[arrayIndex].inhibit = currCell.__orb._rowSpan - 1;

            // increment colWidths index
            arrayIndex++;
          }
        }
      }

      // decrement inhibited state of all widths unsed in colWidths (not reached by current row cells)
      currWidth = colWidths[arrayIndex];
      while(currWidth) {
        if(currWidth.inhibit > 0) {
          currWidth.inhibit--;
        }
        arrayIndex++;
        currWidth = colWidths[arrayIndex];
      }
    }

    // set colWidths to the tblObject
    tblObject.w = 0;
    tblObject.colWidths = colWidths.map(function(item, index) {
      tblObject.w += item.width;
      return item.width;
    });
  }
}

/**
 * Sets the width of all cells of a html table element
 * @param  {Object}  tblObject - object having a table element in its 'node' property
 * @param  {Array}  colWidths - an array of numeric values representing the width of each individual cell.
 *                                  Its length is equal to the greatest number of cells of all rows
 *                                  (in case of cells having colSpan/rowSpan greater than 1.)
 */
function setTableWidths(tblObject, colWidths) {
  if(tblObject && tblObject.node) {

    // reset table width
    (tblObject.size = (tblObject.size || {})).width = 0;

    var tbl = tblObject.node;

    // for each row, set its cells width
    for(var rowIndex = 0; rowIndex < tbl.rows.length; rowIndex++) {
      
      // current row
      var currRow = tbl.rows[rowIndex];
      // index in colWidths
      var arrayIndex = 0;
      var currWidth = null;

      // set width of each cell
      for(var cellIndex = 0; cellIndex < currRow.cells.length; cellIndex++) {
        
        // current cell
        var currCell = currRow.cells[cellIndex];
        if(currCell.__orb._visible) {
          // cell width
          var newCellWidth = 0;
          // whether current cell spans vertically more than 1 row
          var rowsSpan = currCell.__orb._rowSpan > 1 && rowIndex < tbl.rows.length - 1;

          // current cell width is the sum of (its) "colspan" items in colWidths starting at 'arrayIndex'
          // 'arrayIndex' should be incremented by an amount equal to current cell 'colspan' but should also skip 'inhibited' cells
          for(var cspan = 0; cspan < currCell.__orb._colSpan; cspan++) {
            currWidth = colWidths[arrayIndex];
            // skip inhibited widths (width that belongs to an upper cell than spans vertically to current row)
            while(currWidth && currWidth.inhibit > 0) {
              currWidth.inhibit--;
              arrayIndex++;
              currWidth = colWidths[arrayIndex];
            }

            if(currWidth) {
              // add width of cells participating in the span
              newCellWidth += currWidth.width;
              // if current cell spans vertically more than 1 row, mark its width as inhibited for all cells participating in this span
              if(rowsSpan) {
                currWidth.inhibit = currCell.__orb._rowSpan - 1;
              }

              // advance colWidths index
              arrayIndex++;
            }
          }

          currCell.children[0].style.width = newCellWidth + 'px';

          // set table width (only in first iteration)
          if(rowIndex === 0) {
            var outerCellWidth = 0;
            if(currCell.__orb) {
              outerCellWidth = currCell.__orb._colSpan * (Math.ceil(currCell.__orb._paddingLeft + currCell.__orb._paddingRight + currCell.__orb._borderLeftWidth + currCell.__orb._borderRightWidth));
            }
            tblObject.w += newCellWidth + outerCellWidth;
          }
        }
      }

      // decrement inhibited state of all widths unsed in colWidths (not reached by current row cells)
      currWidth = colWidths[arrayIndex];
      while(currWidth) {
        if(currWidth.inhibit > 0) {
          currWidth.inhibit--;
        }
        arrayIndex++;
        currWidth = colWidths[arrayIndex];
      }
    }
  }
}