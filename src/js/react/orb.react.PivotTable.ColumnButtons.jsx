/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableColumnButtons = react.createClass({
  render: function() {
    var self = this;
    var PivotButton = comps.PivotButton;
    var DropTarget = comps.DropTarget;

    var config = this.props.pivotTableComp.pgridwidget.pgrid.config;

    var columnButtons = config.columnFields.map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={axe.Type.COLUMNS}
                          position={index}
                          pivotTableComp={self.props.pivotTableComp}>
             </PivotButton>;
    });

    return  <DropTarget buttons={columnButtons} axetype={axe.Type.COLUMNS}>
            </DropTarget>;
  }
});