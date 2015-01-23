/** @jsx React.DOM */

/* global module, require, React */

'use strict';

var pivotId = 1;
var themeChangeCallbacks = {};

module.exports.PivotTable = react.createClass({
  id: pivotId++,
  pgrid: null,
  pgridwidget: null,
  getInitialState: function() {
    comps.DragManager.init(this);
    
    themeChangeCallbacks[this.id] = [];
    this.registerThemeChanged(this.updateClasses);

    this.pgridwidget = this.props.pgridwidget;
    this.pgrid = this.pgridwidget.pgrid;
    return {};
  },
  sort: function(axetype, field) {
    this.pgridwidget.sort(axetype, field);
    this.setProps({});
  },
  moveButton: function(button, newAxeType, position) {
    this.pgridwidget.moveField(button.props.field.name, button.props.axetype, newAxeType, position);
    this.setProps({});
  },
  expandRow: function(cell) {
    cell.expand();
    this.setProps({});
  },
  collapseRow: function(cell) {
    cell.subtotalHeader.collapse();
    this.setProps({});
  },
  applyFilter: function(fieldname, operator, term, staticValue, excludeStatic) {
    this.pgridwidget.applyFilter(fieldname, operator, term, staticValue, excludeStatic);
    this.setProps({});
  },
  registerThemeChanged: function(compCallback) {
    if(compCallback) {
      themeChangeCallbacks[this.id].push(compCallback);
    }
  },
  unregisterThemeChanged: function(compCallback) {
    var i;
    if(compCallback && (i = themeChangeCallbacks[this.id].indexOf(compCallback)) >= 0) {
      themeChangeCallbacks[this.id].splice(i, 1);
    }
  },
  changeTheme: function(newTheme) {
    if(this.pgridwidget.pgrid.config.setTheme(newTheme)) {
      // notify self/sub-components of the theme change
      for(var i = 0; i < themeChangeCallbacks[this.id].length; i++) {
        themeChangeCallbacks[this.id][i]();
      }
    }
  },
  updateClasses: function() {
      var thisnode = this.getDOMNode();
      var classes = this.pgridwidget.pgrid.config.theme.getPivotClasses();    
      thisnode.className = classes.container;
      thisnode.children[1].className = classes.table;
  },
  render: function() {

    var self = this;

    var config = this.pgridwidget.pgrid.config;
    var PivotButton = comps.PivotButton;
    var PivotRow = comps.PivotRow;
    var DropTarget = comps.DropTarget;
    var Toolbar = comps.Toolbar;

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
    var rowButtonsCell = <td className="empty" colSpan={this.pgridwidget.layout.rowHeaders.width + extraCol} rowSpan="1">
                          <DropTarget buttons={rowButtons} axetype={axe.Type.ROWS}>
                          </DropTarget>
                         </td>;

    var rows = this.pgridwidget.cells.map(function(row, index) {
      if(index == self.pgridwidget.layout.columnHeaders.height - 1) {
        return <PivotRow key={index}
                         row={row}
                         topmost={index === 0}
                         rowButtonsCount={self.pgridwidget.layout.rowHeaders.width}
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

    var classes = config.theme.getPivotClasses();    

    var tblStyle = {};
    if(config.width) { tblStyle.width = config.width; }
    if(config.height) { tblStyle.height = config.height; }

    return (
    <div className={classes.container} style={tblStyle}>
      <div className="orb-toolbar" style={{ display: config.showToolbar ? 'block' : 'none' }}>
        <Toolbar pivotTableComp={self}></Toolbar>
      </div>
      <table id="{'tbl' + self.id}" className={classes.table} style={{width: '100%'}}>
        <tbody>
          <tr>
            <td className="flds-grp-cap av-flds text-muted" colSpan={extraCol} rowSpan="1">
              <div>Fields</div>
            </td>
            <td className="av-flds" colSpan={this.pgridwidget.layout.pivotTable.width} rowSpan="1">
              <DropTarget buttons={fieldButtons} axetype={null}>
              </DropTarget>
            </td>
          </tr>
          <tr>
            <td className="flds-grp-cap text-muted" colSpan={extraCol} rowSpan="1">
              <div>Data</div>
            </td>
            <td className="empty" colSpan={this.pgridwidget.layout.pivotTable.width} rowSpan="1">
              <DropTarget buttons={dataButtons} axetype={axe.Type.DATA}>
              </DropTarget>
            </td>
          </tr>
          <tr>
            <td className="empty" colSpan={this.pgridwidget.layout.rowHeaders.width + extraCol} rowSpan="1"></td>
            <td className="empty" colSpan={this.pgridwidget.layout.columnHeaders.width} rowSpan="1">
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