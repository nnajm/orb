/** @jsx React.DOM */

/* global module, require, React */

'use strict';

var pivotId = 1;

module.exports.PivotTable = react.createClass({
  id: pivotId++,
  pgrid: null,
  pgridwidget: null,
  getInitialState: function() {
    comps.DragManager.init(this);
    this.pgridwidget = this.props.pgridwidget;
    this.pgrid = this.pgridwidget.pgrid;
    return {};
  },
  sort: function(axetype, field) {
    this.pgridwidget.sort(axetype, field);
    this.setProps(this.props);
  },
  moveButton: function(button, newAxeType, position) {
    this.pgridwidget.moveField(button.props.field.name, button.props.axetype, newAxeType, position);
    this.setProps(this.props);
  },
  expandRow: function(cell) {
    cell.expand();
    this.setProps({});
  },
  collapseRow: function(cell) {
    cell.subtotalHeader.collapse();
    this.setProps({});
  },
  render: function() {

    var self = this;

    var config = this.pgridwidget.pgrid.config;
    var PivotButton = comps.PivotButton;
    var PivotRow = comps.PivotRow;
    var DropTarget = comps.DropTarget;

    var fieldButtons = config.availablefields().map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={null}
                          position={index}
                          pivotTableComp={self}>
             </PivotButton>;
    });

    var dataButtons = config.dataFields.map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={axe.Type.DATA}
                          position={index}
                          pivotTableComp={self}>
             </PivotButton>;
    });

    var columnButtons = config.columnFields.map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={axe.Type.COLUMNS}
                          position={index}
                          pivotTableComp={self}>
             </PivotButton>;
    });

    // get 'row buttons' row (also last row containing column headers)
    var rowButtons = utils.findInArray(this.pgridwidget.cells, function(row) {
      return row[0].template === 'cell-template-fieldbutton';
    });

    // build row buttons
    if(rowButtons !== undefined) {
      rowButtons = rowButtons.filter(function(buttonCell) {
        return buttonCell.template === 'cell-template-fieldbutton';
      }).map(function(buttonCell, index) {
          return <PivotButton key={buttonCell.value.name}
                              field={buttonCell.value}
                              axetype={axe.Type.ROWS}
                              position={index}
                              pivotTableComp={self}>
                 </PivotButton>;
      });
    } else {
      rowButtons = [];
    }

    // build the cell that will contains 'row buttons'
    var rowButtonsCell = <td className="empty" colSpan={this.pgridwidget.rowHeadersWidth + extraCol} rowSpan="1">
                          <DropTarget buttons={rowButtons} axetype={axe.Type.ROWS}>
                          </DropTarget>
                         </td>;

    var rows = this.pgridwidget.cells.map(function(row, index) {
      if(index == self.pgridwidget.columnHeadersHeight - 1) {
        return <PivotRow key={index}
                         row={row}
                         topmost={index === 0}
                         rowButtonsCount={self.pgridwidget.rowHeadersWidth}
                         rowButtonsCell={rowButtonsCell}
                         pivotTableComp={self}>
               </PivotRow>;
      } else {
        return <PivotRow key={index}
                         topmost={index === 0}
                         row={row}
                         pivotTableComp={self}>
               </PivotRow>;
      }
    });

    var useBootstrap = config.theme === 'bootstrap';
    var containerClass = "orb-container orb-" + config.theme;
    var orbtableClass = "orb" + (useBootstrap ? " table" : "");

    var tblStyle = {};
    if(config.width) { tblStyle.width = config.width; }
    if(config.height) { tblStyle.height = config.height; }

    return (
    <div className={containerClass} style={tblStyle}>
      <table id="{'tbl' + self.id}" className={orbtableClass} style={{width: '100%'}}>
        <tbody>
          <tr>
            <td className="flds-grp-cap av-flds text-muted" colSpan={extraCol} rowSpan="1">
              <div>Fields</div>
            </td>
            <td className="av-flds" colSpan={this.pgridwidget.totalWidth} rowSpan="1">
              <DropTarget buttons={fieldButtons} axetype={null}>
              </DropTarget>
            </td>
          </tr>
          <tr>
            <td className="flds-grp-cap text-muted" colSpan={extraCol} rowSpan="1">
              <div>Data</div>
            </td>
            <td className="empty" colSpan={this.pgridwidget.totalWidth} rowSpan="1">
              <DropTarget buttons={dataButtons} axetype={axe.Type.DATA}>
              </DropTarget>
            </td>
          </tr>
          <tr>
            <td className="empty" colSpan={this.pgridwidget.rowHeadersWidth + extraCol} rowSpan="1"></td>
            <td className="empty" colSpan={this.pgridwidget.columnHeadersWidth} rowSpan="1">
              <DropTarget buttons={columnButtons} axetype={axe.Type.COLUMNS}>
              </DropTarget>
            </td>
          </tr>
          {rows}
        </tbody>
      </table>
      <div className="orb-overlay orb-overlay-hidden" id={'drilldialog' + self.id}></div>
    </div>
    );
  }
});