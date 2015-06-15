/** @jsx React.DOM */

/* global module, require, React */
/*jshint eqnull: true*/

'use strict';

var _paddingLeft = null;
var _borderLeft = null;

module.exports.PivotCell = react.createClass({
  expand: function() {
    this.props.pivotTableComp.expandRow(this.props.cell);
  },
  collapse: function() {
    this.props.pivotTableComp.collapseRow(this.props.cell);
  },
  updateCellInfos: function() {
    var node = this.getDOMNode();
    var cell = this.props.cell;
    node.__orb = node.__orb || {};

    if(!cell.visible()) {

      node.__orb._visible = false;

    } else {
      var cellContentNode = this.refs.cellContent.getDOMNode();

      var text = node.textContent;
      var propList = [];
      var retPaddingLeft = _paddingLeft == null;
      var retBorderLeft = !this.props.leftmost && _borderLeft == null;

      if(retPaddingLeft) {
        propList.push('padding-left');
      }

      if(retBorderLeft) {
        propList.push('border-left-width');
      }

      if(propList.length > 0) {
        var nodeStyle = reactUtils.getStyle(node, propList, true);

        if(retPaddingLeft) {
          _paddingLeft = parseFloat(nodeStyle[0]);
        }

        if(retBorderLeft) {
          _borderLeft = parseFloat(nodeStyle[retPaddingLeft ? 1 : 0]);
        }
      }

      reactUtils.removeClass(node, 'cell-hidden');

      node.__orb._visible = true;
      node.__orb._textWidth = reactUtils.getSize(cellContentNode).width;
      node.__orb._colSpan = this.props.cell.hspan(true) || 1;
      node.__orb._rowSpan = this.props.cell.vspan(true) || 1;
      node.__orb._paddingLeft = _paddingLeft;
      node.__orb._paddingRight = _paddingLeft;
      node.__orb._borderLeftWidth = this.props.leftmost ? 0 : _borderLeft;
      node.__orb._borderRightWidth = 0;
    }
  },
  componentDidMount: function() {
    this.updateCellInfos();
  },
  componentDidUpdate: function() {
    this.updateCellInfos();
  },
  shouldComponentUpdate: function(nextProps, nextState) {
    if(nextProps.cell && nextProps.cell == this.props.cell && !this._latestVisibleState && !nextProps.cell.visible()) {
      return false;
    }
    return true;
  },
  _latestVisibleState: false,
  render: function() {
    var self = this;
    var cell = this.props.cell;
    var divcontent = [];
    var value;
    var cellClick;
    var headerPushed = false;

    this._latestVisibleState = cell.visible();

    switch(cell.template) {
      case 'cell-template-row-header':
      case 'cell-template-column-header':
        var isWrapper = cell.type === uiheaders.HeaderType.WRAPPER && cell.dim.field.subTotal.visible && cell.dim.field.subTotal.collapsible;
        var isSubtotal = cell.type === uiheaders.HeaderType.SUB_TOTAL && !cell.expanded;
        if(isWrapper || isSubtotal) {
          headerPushed = true;

          divcontent.push(<table key="header-value" ref="cellContent">
            <tbody>
            <tr><td className="orb-tgl-btn"><div className={'orb-tgl-btn-' + (isWrapper ? 'down' : 'right')} onClick={(isWrapper ? this.collapse : this.expand)}></div></td>
            <td className="hdr-val"><div dangerouslySetInnerHTML={{__html: cell.value || '&#160;'}}></div></td></tr>
            </tbody></table>);
        } else {
          value = (cell.value || '&#160;') + (cell.type === uiheaders.HeaderType.SUB_TOTAL ? ' Total' : '');
        }
        break;
      case 'cell-template-dataheader':
        value = cell.value.caption;
        break;
      case 'cell-template-datavalue':
        value = (cell.datafield && cell.datafield.formatFunc) ? cell.datafield.formatFunc()(cell.value) : cell.value;
        cellClick = function() {
          self.props.pivotTableComp.pgridwidget.drilldown(cell, self.props.pivotTableComp.id);
        };
        break;
      default:
        break;
    }

    if(!headerPushed) {
      var headerClassName;
      switch(cell.template){
        case 'cell-template-datavalue':
          headerClassName = 'cell-data';
        break;
        default:
        if(cell.template != 'cell-template-dataheader' && cell.type !== uiheaders.HeaderType.GRAND_TOTAL) {
          headerClassName = 'hdr-val';
        }
      }
      divcontent.push(<div key="cell-value" ref="cellContent" className={headerClassName}><div dangerouslySetInnerHTML={{__html: value || '&#160;'}}></div></div>);
    }

    return <td className={getClassname(this.props)}
               onDoubleClick={ cellClick }
               colSpan={cell.hspan()}
               rowSpan={cell.vspan()}>
                <div>
                  {divcontent}
                </div>
           </td>;
  }
});

function getClassname(compProps) {
    var cell = compProps.cell;
    var classname = cell.cssclass;
    var isEmpty = cell.template === 'cell-template-empty';

    if(!cell.visible()) {
      classname += ' cell-hidden'; 
    }

    if(cell.type === uiheaders.HeaderType.SUB_TOTAL && cell.expanded) {
      classname += ' header-st-exp'; 
    }

    if(cell.type === uiheaders.HeaderType.GRAND_TOTAL) {
      if(cell.dim.depth === 1) {
        classname += ' header-nofields'; 
      } else if(cell.dim.depth > 2) {
        classname += ' header-gt-exp'; 
      }
    }

    if(compProps.leftmost) {
      classname += ' ' + (cell.template === 'cell-template-datavalue' ? 'cell' : 'header') + '-leftmost';
    }

    if(compProps.topmost) {
      classname += ' cell-topmost';
    }

    return classname;
}