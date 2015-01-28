/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotTableUpperButtons = react.createClass({
  render: function() {
    var self = this;
    var PivotButton = comps.PivotButton;
    var DropTarget = comps.DropTarget;

    var config = this.props.pivotTableComp.pgridwidget.pgrid.config;

    var fieldButtons = config.availablefields().map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={null}
                          position={index}
                          pivotTableComp={self.props.pivotTableComp}>
             </PivotButton>;
    });

    var dataButtons = config.dataFields.map(function(field, index) {
      return <PivotButton key={field.name}
                          field={field}
                          axetype={axe.Type.DATA}
                          position={index}
                          pivotTableComp={self.props.pivotTableComp}>
             </PivotButton>;
    });

    return <table className="inner-table">
        <tbody>
          <tr>
            <td className="flds-grp-cap av-flds text-muted">
              <div>Fields</div>
            </td>
            <td className="av-flds">
              <DropTarget buttons={fieldButtons} axetype={null}>
              </DropTarget>
            </td>
          </tr>
          <tr>
            <td className="flds-grp-cap text-muted">
              <div>Data</div>
            </td>
            <td className="empty">
              <DropTarget buttons={dataButtons} axetype={axe.Type.DATA}>
              </DropTarget>
            </td>
          </tr>
        </tbody>
    </table>;
  }
});