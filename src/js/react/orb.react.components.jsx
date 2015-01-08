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
                         topmost={index === 0}
                         rowButtonsCount={ptc.rowHeadersWidth}
                         rowButtonsCell={rowButtonsCell}
                         rootComp={self}>
               </PivotRow>;
      } else {
        return <PivotRow key={index}
                         topmost={index === 0}
                         row={row}
                         rootComp={self}>
               </PivotRow>;
      }
    });

    var useBootstrap = this.props.config.bootstrap;
    var containerClass = "orb-container" + (useBootstrap ? "" : " orb-theme");
    var orbtableClass = "orb" + (useBootstrap ? " table" : "");

    var tblStyle = {};
    if(this.props.config.width) { tblStyle.width = this.props.config.width; }
    if(this.props.config.height) { tblStyle.height = this.props.config.height; }


    return (
    <div className={containerClass} style={tblStyle}>
      <table id="{'tbl' + self.id}" className={orbtableClass} style={{width: '100%'}}>
        <tbody>
          <tr>
            <td className="fields-group-caption available-fields text-muted" colSpan={extraCol} rowSpan="1">
              <div>Fields</div>
            </td>
            <td className="available-fields" colSpan={ptc.totalWidth} rowSpan="1">
              <DropTarget data={fieldButtons} axetype={null}>
              </DropTarget>
            </td>
          </tr>
          <tr>
            <td className="fields-group-caption text-muted" colSpan={extraCol} rowSpan="1">
              <div>Data</div>
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
        var isleftmostHeader = index === 0;
        return <PivotCell key={index} 
                          cell={cell}
                          topmost={self.props.topmost}
                          rightmost={isrightmost}
                          leftmostheader={isleftmostHeader}
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
                           (cell.type === uiheaders.HeaderType.GRAND_TOTAL) || 
                           (cell.dim && (cell.dim.isRoot || cell.dim.parent.isRoot))
                         );
        var isleftmostHeader = cell.template === 'cell-template-column-header' && index === 1;
        var isleftmostDataValue = cell.template === 'cell-template-datavalue' && cell.visible() && (self.props.row[index - 1].template !== 'cell-template-datavalue' || !self.props.row[index - 1].visible());

        return <PivotCell key={index} 
                          cell={cell}
                          topmost={self.props.topmost}
                          leftmostheader={isleftmostHeader}
                          leftmostdatavalue={isleftmostDataValue}
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
    var headerPushed = false;

    switch(cell.template) {
      case 'cell-template-row-header':
      case 'cell-template-column-header':
        var isWrapper = cell.type === uiheaders.HeaderType.WRAPPER && cell.dim.field.subTotal.visible && cell.dim.field.subTotal.collapsible && cell.subtotalHeader.expanded;
        var isSubtotal = cell.type === uiheaders.HeaderType.SUB_TOTAL && !cell.expanded;
        if(isWrapper || isSubtotal) {
          headerPushed = true;

          divcontent.push(<table key="header-value">
            <tbody>
            <tr><td className="toggle-button"><div className={'toggle-button-' + (isWrapper ? 'down' : 'right')} onClick={(isWrapper ? this.collapse : this.expand)}></div></td>
            <td className="header-value"><div>{cell.value}</div></td></tr>
            </tbody></table>);
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

    if(!headerPushed) {
      divcontent.push(<div key="cell-value" className={cell.template !== 'cell-template-datavalue' ? 'header-value' : ''}><div>{value}</div></div>);
    }

    var classname = cell.cssclass;
    var isHidden = !cell.visible();
      
    if(isHidden) {
      classname += ' cell-hidden';
    }

    if(this.props.topmost && cell.template !== 'cell-template-empty') {
      classname += ' cell-topmost';
    }

    if(this.props.rightmost && (cell.axetype !== axe.Type.COLUMNS || cell.type === uiheaders.HeaderType.GRAND_TOTAL)) {
      classname += ' cell-rightmost';
    }

    if((this.props.leftmost && cell.template !== 'cell-template-empty') || this.props.leftmostheader || this.props.leftmostdatavalue) {
      classname += ' cell-leftmost';
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
    var tableClass = this.props.bootstrap ? "table table-striped table-condensed" : "";

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
        for(var j = 0; j < data[i].length; j++) {
          row.push(<td>{ data[i][j] }</td>);
        }
        rows.push(<tr>{ row }</tr>);
      }
    }

    return <table className={tableClass}>
    <tbody>
    { rows }
    </tbody>
    </table>;
  }
});

function createOverlay() {
  var overlayElement = document.createElement('div');
  overlayElement.className = 'orb-overlay orb-overlay-hidden';
  document.body.appendChild(overlayElement);  
  return overlayElement;
}

var Dialog = module.exports.Dialog = react.createClass({
  statics: {
    create: function() {
        var dialogFactory = React.createFactory(Dialog);
        var dialog = dialogFactory({});
        var overlay = createOverlay();

        return {
          show: function(props) {
            dialog.props = props;
            React.render(dialog, overlay);
          }
        }
    }
  },
  overlayElement: null,
  componentDidMount: function() {
    this.overlayElement = this.getDOMNode().parentNode;
    this.overlayElement.className = "orb-overlay orb-overlay-visible" + (this.props.bootstrap ? " modal" : " orb-theme");
    this.overlayElement.addEventListener('click', this.close);

    var dialogElement = this.overlayElement.children[0];
    var dialogBodyElement = dialogElement.children[0].children[1];

    var screenWidth = Math.max(document.documentElement.clientWidth, window.innerWidth || 0)
    var screenHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0)
    var maxHeight = 2*screenHeight/3;
    maxHeight = maxHeight < 301 ? 301 : maxHeight;
    var dWidth = dialogElement.offsetWidth + (dialogElement.offsetHeight > maxHeight ?  11 : 0);
    var dHeight = dialogElement.offsetHeight > maxHeight ? maxHeight : dialogElement.offsetHeight;

    dialogElement.style.top = (screenHeight > dHeight ? (screenHeight - dHeight) / 2 : 0) + 'px';
    dialogElement.style.left = (screenWidth > dWidth ? (screenWidth - dWidth) / 2 : 0) + 'px';
    dialogElement.style.height = dHeight + 'px';
    dialogBodyElement.style.width = dWidth + 'px';
    dialogBodyElement.style.height = (dHeight - 45) + 'px';
  },
  close: function(e) {
    if(e.target == this.overlayElement || e.target.className === 'button-close') {
      this.overlayElement.removeEventListener('click', this.close);
      React.unmountComponentAtNode(this.overlayElement);
      this.overlayElement.className = "orb-overlay orb-overlay-hidden" + (this.props.bootstrap ? " modal" : " orb-theme");
    }
  },
  render: function() {
    var comp = React.createElement(this.props.comp.type, this.props.comp.props);
    var useBootstrap = this.props.bootstrap;
    var dialogClass = "orb-dialog" + (useBootstrap ? " modal-dialog" : "");
    var contentClass = useBootstrap ? "modal-content" : "";
    var headerClass = "orb-dialog-header" + (useBootstrap ? " modal-header" : "");
    var titleClass = useBootstrap ? "modal-title" : "";
    var bodyClass = "orb-dialog-body" + (useBootstrap ? " modal-body" : "");

    return <div className={dialogClass} style={ this.props.style || {} }> 
    <div className={contentClass}>
        <div className={headerClass}><div className="button-close" onClick={ this.close }></div><div className={titleClass}>{ this.props.title }</div></div>
        <div className={bodyClass}>
        { comp }
        </div>
        </div>
      </div>;
  }
});