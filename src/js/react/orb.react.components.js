/** @jsx React.DOM */

// Ensure orb.react namespace is created
orb.utils.ns('orb.react');

(function() {

var extraCol = 1;

orb.react.PivotTable = React.createClass({
  getInitialState: function() {
    orb.react.DragManager.init(this);
    return {};
  },
  sort: function(axetype, field) {
    this.props.data.sort(axetype, field);
    this.setProps(this.props);
  },
  moveButton: function(button, newAxeType, position) {
    this.props.data.moveField(button.props.field.name, button.props.axetype, newAxeType, position);
    this.setProps(this.props);
  },
  expandRow: function(cell) {
    cell.expanded = true;
    this.setProps({});
  },
  collapseRow: function(cell) {
    cell.subtotalHeader.expanded = false;
    this.setProps({});
  },
  render: function() {

    var self = this;

    var ptc = this.props.data;
    var PivotButton = orb.react.PivotButton;
    var PivotRow = orb.react.PivotRow;
    var DropTarget = orb.react.DropTarget;

    var fieldButtons = ptc.pgrid.config.availablefields().map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={null}
                          position={index}
                          rootComp={self}>
             </PivotButton>;
    });

    var dataButtons = ptc.pgrid.config.datafields.map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={orb.axe.Type.DATA}
                          position={index}
                          rootComp={self}>
             </PivotButton>;
    });

    var columnButtons = ptc.pgrid.config.columnfields.map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={orb.axe.Type.COLUMNS}
                          position={index}
                          rootComp={self}>
             </PivotButton>;
    });

    // get 'row buttons' row (also last row containing column headers)
    var rowButtons = orb.utils.findInArray(ptc.cells, function(row) {
      return row[0].template === 'cell-template-fieldbutton';
    });

    // build row buttons
    if(rowButtons !== undefined) {
      rowButtons = rowButtons.filter(function(buttonCell) {
        return buttonCell.template === 'cell-template-fieldbutton';
      }).map(function(buttonCell, index) {
          return <PivotButton key={buttonCell.value.name}
                              field={buttonCell.value}
                              axetype={orb.axe.Type.ROWS}
                              position={index}
                              rootComp={self}>
                 </PivotButton>;
      });
    } else {
      rowButtons = [];
    }

    // build the cell that will contains 'row buttons'
    var rowButtonsCell = <td className="empty" colSpan={ptc.rowHeadersWidth + extraCol} rowSpan="1">
                          <DropTarget data={rowButtons} axetype={orb.axe.Type.ROWS}>
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
      };
    });

    var tblStyle = {width: '901px'};

    return (
    <table id='tbl' className="orb" style={tblStyle}>
      <tbody>
        <tr>
          <td className="available-fields field-group" colSpan={extraCol} rowSpan="1">
            <div className="field-group-caption">Fields:</div>
          </td>
          <td className="available-fields" colSpan={ptc.totalWidth} rowSpan="1">
            <DropTarget data={fieldButtons} axetype={null}>
            </DropTarget>
          </td>
        </tr>
        <tr>
          <td className="field-group" colSpan={extraCol} rowSpan="1">
            <div className="field-group-caption">Data fields:</div>
          </td>
          <td className="empty" colSpan={ptc.totalWidth} rowSpan="1">
            <DropTarget data={dataButtons} axetype={orb.axe.Type.DATA}>
            </DropTarget>
          </td>
        </tr>
        <tr>
          <td className="empty" colSpan={ptc.rowHeadersWidth + extraCol} rowSpan="1"></td>
          <td className="empty" colSpan={ptc.columnHeadersWidth} rowSpan="1">
            <DropTarget data={columnButtons} axetype={orb.axe.Type.COLUMNS}>
            </DropTarget>
          </td>
        </tr>
        {rows}
      </tbody>
    </table>
    );
  }
});

orb.react.PivotRow = React.createClass({
  render: function() {
    var self = this;
    var PivotCell = orb.react.PivotCell;
    
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
                           cell.type === orb.ui.HeaderType.EMPTY ||
                           cell.type === orb.ui.HeaderType.SUB_TOTAL || 
                           cell.type === orb.ui.HeaderType.GRAND_TOTAL || 
                           (cell.dim && cell.dim.parent.isRoot)
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

orb.react.PivotCell = React.createClass({
  expand: function() {
    this.props.rootComp.expandRow(this.props.cell);
  },
  collapse: function() {
    this.props.rootComp.collapseRow(this.props.cell);
  },
  render: function() {
    var cell = this.props.cell;
    var divcontent = [];
    var value;
    var vArrow = '\u25bc';
    var hArrow = '\u25b6';

    switch(cell.template) {
      case 'cell-template-row-header':
      case 'cell-template-column-header':
        if(cell.type === orb.ui.HeaderType.WRAPPER && cell.dim.field.subtotal.visible && cell.dim.field.subtotal.collapsible && cell.subtotalHeader.expanded) {
          divcontent.push(<span key="toggle-button" className="toggle-button" onClick={this.collapse}>{vArrow}</span>);
        } else if(cell.type === orb.ui.HeaderType.SUB_TOTAL && !cell.expanded){
          divcontent.push(<span key="toggle-button" className="toggle-button" onClick={this.expand}>{hArrow}</span>);
        }
        value = cell.value;
        break;
      case 'cell-template-dataheader':
        value = cell.value.caption;
        break;
      case 'cell-template-datavalue':
        value = cell.datafield && cell.datafield.formatfunc ? cell.datafield.formatfunc(cell.value) : cell.value;
        break;
      default:
        break;
    }

    divcontent.push(<span key="cell-value" style={{whiteSpace: 'nowrap'}}>{value}</span>);

    var classname = cell.cssclass;
    var isHidden = !cell.visible();
    if(isHidden || this.props.rightmost) {
      
      if(isHidden) {
        classname += ' cell-hidden';
      }

      if(this.props.rightmost && (cell.axetype !== orb.axe.Type.COLUMNS || cell.type === orb.ui.HeaderType.GRAND_TOTAL)) {
        classname += ' cell-rightmost';
      }
    }

    return <td className={classname}
               colSpan={cell.hspan() + (this.props.leftmost ? extraCol : 0)}
               rowSpan={cell.vspan()}>
                <div>
                  {divcontent}
                </div>
           </td>;
  }
});

})();