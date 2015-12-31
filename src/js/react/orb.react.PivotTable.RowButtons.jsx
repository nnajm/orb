/* global module, require, React */

'use strict';

var React = typeof window === 'undefined' ? require('react') : window.React,
    PivotButton = require('./orb.react.PivotButton.jsx'),
    DropTarget = require('./orb.react.DropTarget.jsx'),
    DropTargetVertical = require('./orb.react.DropTargetVertical.jsx'),
    axe = require('../orb.axe');

module.exports = React.createClass({
  render: function() {
    var self = this;
    var config = this.props.pivotTableComp.pgridwidget.pgrid.config;

    var rowButtons = config.rowFields.map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={axe.Type.ROWS}
                          position={index}
                          pivotTableComp={self.props.pivotTableComp}>
             </PivotButton>;
    });

    if(config.chartMode.enabled) {
      return  <DropTargetVertical buttons={rowButtons} axetype={axe.Type.ROWS}>
              </DropTargetVertical>;
    } else {
      return  <DropTarget buttons={rowButtons} axetype={axe.Type.ROWS}>
              </DropTarget>;
    }
  }
});