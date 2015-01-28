/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableRowButtons = react.createClass({
  render: function() {
    var self = this;
    var PivotButton = comps.PivotButton;
    var DropTarget = comps.DropTarget;

    var config = this.props.pivotTableComp.pgridwidget.pgrid.config;

    var rowButtons = config.rowFields.map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={axe.Type.ROWS}
                          position={index}
                          pivotTableComp={self.props.pivotTableComp}>
             </PivotButton>;
    });

    return  <DropTarget buttons={rowButtons} axetype={axe.Type.ROWS}>
            </DropTarget>;
  }
});