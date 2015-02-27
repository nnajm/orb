/** @jsx React.DOM */

/* global module, require, React */

'use strict';

var widthDiv;

function getTextWidth(fontFamily, fontSize, text) {
  if(!widthDiv) {
    widthDiv = document.createElement('div');
    widthDiv.style.position = "absolute";
    widthDiv.style.right = "11em";
    widthDiv.style.whiteSpace = "nowrap";
    document.body.appendChild(widthDiv);
  }
  widthDiv.style.fontFamily = fontFamily;
  widthDiv.style.fontSize = fontSize;
  widthDiv.innerHTML = text;
  return widthDiv.offsetWidth;
}

module.exports.PivotCell = react.createClass({
  expand: function() {
    this.props.pivotTableComp.expandRow(this.props.cell);
  },
  collapse: function() {
    this.props.pivotTableComp.collapseRow(this.props.cell);
  },
  componentDidMount: function() {
    var node = this.getDOMNode();
    var cellContentNode = this.refs.cellContent.getDOMNode();

    var text = node.textContent;
    var nodeStyle = reactUtils.getStyle(node, ['font-family', 'font-size', 'padding-left', 'padding-right', 'border-left-width', 'border-right-width'], true);

    console.log('cell-componentDidMount: ' + text);

    /*var disp = node.style.display;
    node.style.display = 'block';*/
    reactUtils.removeClass(node, 'cell-hidden');
    var w = reactUtils.getSize(cellContentNode).width;
    if(text == 'Wide World Importers') {
      console.log('found w=137.58: ' + text);
    }

    node.__orb = {
      _textWidth: reactUtils.getSize(cellContentNode).width,// getTextWidth(nodeStyle[0], nodeStyle[1], text),
      _colSpan: this.props.cell.hspan(),
      _rowSpan: this.props.cell.vspan(),
      _paddingLeft: parseFloat(nodeStyle[2]),
      _paddingRight: parseFloat(nodeStyle[3]),
      _borderLeftWidth: parseFloat(nodeStyle[4]),
      _borderRightWidth: parseFloat(nodeStyle[5])
    };

    //node.style.display = disp;
    if(!this.props.cell.visible()) {
      reactUtils.addClass(node, 'cell-hidden');
    } else {
      reactUtils.removeClass(node, 'cell-hidden');
    }
  },
  componentDidUpdate: function() {
    var node = this.getDOMNode();
    var cellContentNode = this.refs.cellContent.getDOMNode();

    var text = node.textContent;
    var nodeStyle = reactUtils.getStyle(node, ['font-family', 'font-size', 'padding-left', 'padding-right', 'border-left-width', 'border-right-width'], true);

    console.log('cell-componentDidUpdate: ' + text);
    
    /*var disp = node.style.display;
    node.style.display = 'block';*/
    reactUtils.removeClass(node, 'cell-hidden');

    node.__orb = {
      _textWidth: reactUtils.getSize(cellContentNode).width,// getTextWidth(nodeStyle[0], nodeStyle[1], text),
      _colSpan: this.props.cell.hspan(),
      _rowSpan: this.props.cell.vspan(),
      _paddingLeft: parseFloat(nodeStyle[2]),
      _paddingRight: parseFloat(nodeStyle[3]),
      _borderLeftWidth: parseFloat(nodeStyle[4]),
      _borderRightWidth: parseFloat(nodeStyle[5])
    };

    //node.style.display = disp;
    if(!this.props.cell.visible()) {
      reactUtils.addClass(node, 'cell-hidden');
    } else {
      reactUtils.removeClass(node, 'cell-hidden');
    }
  },                                                                                                                
  render: function() {
    var self = this;
    var cell = this.props.cell;
    var divcontent = [];
    var value;
    var cellClick;
    var headerPushed = false;

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
          value = cell.value + (cell.type === uiheaders.HeaderType.SUB_TOTAL ? ' Total' : '');
        }
        break;
      case 'cell-template-dataheader':
        value = cell.value.caption;
        break;
      case 'cell-template-datavalue':
        value = (cell.datafield && cell.datafield.formatFunc) ? cell.datafield.formatFunc()(cell.value) : cell.value;
        cellClick = function() {
          self.props.pivotTableComp.pgridwidget.drilldown(cell, self.props.pivotTableComp.id);
        }
        break;
      default:
        break;
    }

    if(!headerPushed) {
      var headerClassName = cell.template !== 'cell-template-dataheader' && cell.template !== 'cell-template-datavalue' && cell.type !== uiheaders.HeaderType.GRAND_TOTAL ? 'hdr-val' : '';
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

    if(cell.type === uiheaders.HeaderType.SUB_TOTAL && cell.expanded) {
      classname += ' header-st-exp'; 
    }

    if(cell.type === uiheaders.HeaderType.GRAND_TOTAL && cell.dim.depth > 2) {
      classname += ' header-gt-exp'; 
    }

    if(compProps.leftmost) {
      classname += ' ' + (cell.template === 'cell-template-datavalue' ? 'cell' : 'header') + '-leftmost';
    }

    if(compProps.topmost) {
      classname += ' cell-topmost';
    }

    if(cell.template === 'cell-template-column-header' || cell.template === 'cell-template-dataheader') {
      classname += ' cntr';
    }

    return classname;
}