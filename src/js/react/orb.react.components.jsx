/** @jsx React.DOM */

/* global module, require, React */

'use strict';

var react = typeof window === 'undefined' ? require('react') : window.React;
var utils = require('../orb.utils');
var axe = require('../orb.axe');
var uiheaders = require('../orb.ui.header');

var pivotId = 1;
var extraCol = 1;
var comps = module.exports;

module.exports.PivotTable = react.createClass({
  getInitialState: function() {
    comps.DragManager.init(this);
    return {};
  },
  id: pivotId++,
  sort: function(axetype, field) {
    this.props.data.sort(axetype, field);
    this.setProps(this.props);
  },
  moveButton: function(button, newAxeType, position) {
    this.props.data.moveField(button.props.field.name, button.props.axetype, newAxeType, position);
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

    var ptc = this.props.data;
    var PivotButton = comps.PivotButton;
    var PivotRow = comps.PivotRow;
    var DropTarget = comps.DropTarget;

    var fieldButtons = ptc.pgrid.config.availablefields().map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={null}
                          position={index}
                          rootComp={self}>
             </PivotButton>;
    });

    var dataButtons = ptc.pgrid.config.dataFields.map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={axe.Type.DATA}
                          position={index}
                          rootComp={self}>
             </PivotButton>;
    });

    var columnButtons = ptc.pgrid.config.columnFields.map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={axe.Type.COLUMNS}
                          position={index}
                          rootComp={self}>
             </PivotButton>;
    });

    // get 'row buttons' row (also last row containing column headers)
    var rowButtons = utils.findInArray(ptc.cells, function(row) {
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
                              rootComp={self}>
                 </PivotButton>;
      });
    } else {
      rowButtons = [];
    }

    // build the cell that will contains 'row buttons'
    var rowButtonsCell = <td className="empty" colSpan={ptc.rowHeadersWidth + extraCol} rowSpan="1">
                          <DropTarget data={rowButtons} axetype={axe.Type.ROWS}>
                          </DropTarget>
                         </td>;

    var rows = ptc.cells.map(function(row, index) {
      if(index == ptc.columnHeadersHeight - 1) {
        return <PivotRow key={index}
                         row={row}
                         rowButtonsCount={ptc.rowHeadersWidth}
                         rowButtonsCell={rowButtonsCell}
                         rootComp={self}>
               </PivotRow>;
      } else {
        return <PivotRow key={index}
                         row={row}
                         rootComp={self}>
               </PivotRow>;
      }
    });

    var tblStyle = {};
    if(this.props.config.width) { tblStyle.width = this.props.config.width; }
    if(this.props.config.height) { tblStyle.height = this.props.config.height; }

    return (
    <div className="orb-container" style={tblStyle}>
      <table id="{'tbl' + self.id}" className="orb" style={{width: '100%'}}>
        <tbody>
          <tr>
            <td className="available-fields field-group" colSpan={extraCol} rowSpan="1">
              <div className="field-group-caption">Fields</div>
            </td>
            <td className="available-fields" colSpan={ptc.totalWidth} rowSpan="1">
              <DropTarget data={fieldButtons} axetype={null}>
              </DropTarget>
            </td>
          </tr>
          <tr>
            <td className="field-group" colSpan={extraCol} rowSpan="1">
              <div className="field-group-caption">Data</div>
            </td>
            <td className="empty" colSpan={ptc.totalWidth} rowSpan="1">
              <DropTarget data={dataButtons} axetype={axe.Type.DATA}>
              </DropTarget>
            </td>
          </tr>
          <tr>
            <td className="empty" colSpan={ptc.rowHeadersWidth + extraCol} rowSpan="1"></td>
            <td className="empty" colSpan={ptc.columnHeadersWidth} rowSpan="1">
              <DropTarget data={columnButtons} axetype={axe.Type.COLUMNS}>
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
        return <PivotCell key={index} 
                          cell={cell}
                          rightmost={isrightmost}
                          leftmost={false}
                          rootComp={self.props.rootComp}>
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
                           cell.type === uiheaders.HeaderType.GRAND_TOTAL || 
                           (cell.dim && (cell.dim.isRoot || cell.dim.parent.isRoot))
                         );

        return <PivotCell key={index} 
                          cell={cell}
                          rightmost={isrightmost}
                          leftmost={isleftmost}
                          rootComp={self.props.rootComp}>
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

module.exports.PivotCell = react.createClass({
  expand: function() {
    this.props.rootComp.expandRow(this.props.cell);
  },
  collapse: function() {
    this.props.rootComp.collapseRow(this.props.cell);
  },
  render: function() {
    var self = this;
    var cell = this.props.cell;
    var divcontent = [];
    var value;
    var vArrow = '\u25bc';
    var hArrow = '\u25b6';
    var cellClick;

    switch(cell.template) {
      case 'cell-template-row-header':
      case 'cell-template-column-header':
        if(cell.type === uiheaders.HeaderType.WRAPPER && cell.dim.field.subTotal.visible && cell.dim.field.subTotal.collapsible && cell.subtotalHeader.expanded) {
          divcontent.push(<span key="toggle-button" className="toggle-button toggle-button-down" onClick={this.collapse}></span>);
        } else if(cell.type === uiheaders.HeaderType.SUB_TOTAL && !cell.expanded){
          divcontent.push(<span key="toggle-button" className="toggle-button toggle-button-right" onClick={this.expand}></span>);
        }
        value = cell.value;
        break;
      case 'cell-template-dataheader':
        value = cell.value.caption;
        break;
      case 'cell-template-datavalue':
        value = (cell.datafield && cell.datafield.formatFunc) ? cell.datafield.formatFunc()(cell.value) : cell.value;
        cellClick = function() {
          self.props.rootComp.props.data.drilldown(cell, self.props.rootComp.id);
        }
        break;
      default:
        break;
    }

    divcontent.push(<span key="cell-value" style={{whiteSpace: 'nowrap'}}>{value}</span>);

    var classname = cell.cssclass;
    var isHidden = !cell.visible();
    if(isHidden || this.props.rightmost || this.props.leftmost) {
      
      if(isHidden) {
        classname += ' cell-hidden';
      }

      if(this.props.rightmost && (cell.axetype !== axe.Type.COLUMNS || cell.type === uiheaders.HeaderType.GRAND_TOTAL)) {
        classname += ' cell-rightmost';
      }

      if(this.props.leftmost) {
        classname += ' cell-leftmost';
      }
    }

    if(cell.template === 'cell-template-column-header' || cell.template === 'cell-template-dataheader') {
      classname += ' centered';
    }

    return <td className={classname} onDoubleClick={ cellClick }
               colSpan={cell.hspan() + (this.props.leftmost ? extraCol : 0)}
               rowSpan={cell.vspan()}>
                <div>
                  {divcontent}
                </div>
           </td>;
  }
});

module.exports.Grid = react.createClass({
  render: function() {
    var data = this.props.data;
    var headers = this.props.headers;

    var rows = [];

    if(headers && headers.length > 0) {
      var headerRow = [];
      for(var h = 0; h < headers.length; h++) {
        headerRow.push(<th>{ headers[h] }</th>);
      }
      rows.push(<tr>{ headerRow }</tr>);
    }
    
    if(data && data.length > 0) {
      for(var i = 0; i < data.length; i++) {
        var row = [];
        for(var j = 0; j < data.length; j++) {
          row.push(<td>{ data[i][j] }</td>);
        }
        rows.push(<tr>{ row }</tr>);
      }
    }

    return <table>
    <tbody>
    { rows }
    </tbody>
    </table>;
  }
});

module.exports.Dialog = react.createClass({
  overlayElement: null,
  componentDidMount: function() {
    this.overlayElement = document.getElementById('drilldialog' + this.props.pivotId);    
    this.overlayElement.className = 'orb-overlay orb-overlay-visible';
    this.overlayElement.addEventListener('click', this.close);

    var dialogElement = this.overlayElement.children[0];

    var screenWidth = Math.max(document.documentElement.clientWidth, window.innerWidth || 0)
    var screenHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0)

    dialogElement.style.top = (screenHeight > dialogElement.offsetHeight ? (screenHeight - dialogElement.offsetHeight) / 2 : 0) + 'px';
    dialogElement.style.left = (screenWidth > dialogElement.offsetWidth ? (screenWidth - dialogElement.offsetWidth) / 2 : 0) + 'px';
  },
  close: function() {
    if(this.overlayElement) {
      this.overlayElement.removeEventListener('click', this.close);
      React.unmountComponentAtNode(this.overlayElement);
      this.overlayElement.className = 'orb-overlay orb-overlay-hidden';
    }
  },
  render: function() {
    var Grid = comps.Grid;
    return <div className="orb-dialog"> 
        <div className="orb-dialog-body">
        <Grid headers={this.props.headers} data={this.props.data}></Grid>
        </div>
      </div>;
  }
});