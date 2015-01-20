/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.Grid = react.createClass({
  render: function() {
    var data = this.props.data;
    var headers = this.props.headers;
    var tableClass = this.props.theme == 'bootstrap' ? "table table-striped table-condensed" : "orb-table";

    var rows = [];

    if(headers && headers.length > 0) {
      var headerRow = [];
      for(var h = 0; h < headers.length; h++) {
        headerRow.push(<th key={'h' + h}>{ headers[h] }</th>);
      }
      rows.push(<tr key={'h'}>{ headerRow }</tr>);
    }
    
    if(data && data.length > 0) {
      for(var i = 0; i < data.length; i++) {
        var row = [];
        for(var j = 0; j < data[i].length; j++) {
          row.push(<td key={i + '' + j}>{ data[i][j] }</td>);
        }
        rows.push(<tr key={i}>{ row }</tr>);
      }
    }

    return <table className={tableClass}>
    <tbody>
    { rows }
    </tbody>
    </table>;
  }
});