/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.Chart = react.createClass({
  getInitialState: function() {
    return {
      canRender: false
    };
  },
  canRender: function() {
    return this.state.canRender &&
      typeof this.props.chartMode.type === 'string' &&
      typeof google.visualization[this.props.chartMode.type] === 'function';
  },
  drawChart: function() {
    if(this.canRender()) {
      var chartData = this.props.pivotTableComp.pgridwidget.pgrid.getChartData();
      var data = new google.visualization.DataTable();        
          
      data.addColumn('string', chartData.hAxisLabel);
      for(var ri=0; ri < chartData.colNames.length; ri++) {
        data.addColumn('number', chartData.colNames[ri]);
      }

      data.addRows(chartData.dataTable);

      var options = {
        title: chartData.title,
        //isStacked: true,
        fontName: this.state.chartStyle.fontFamily,
        fontSize: parseFloat(this.state.chartStyle.fontSize),
        hAxis: {
          title: chartData.hAxisLabel
        },
        vAxis: {
          title: chartData.vAxisLabel
        }
      };

      if(typeof google.visualization[this.props.chartMode.type] === 'function') {
        var chart = new google.visualization[this.props.chartMode.type](this.getDOMNode());
        chart.draw(data, options);
      }
    }
  },
  componentDidMount: function() {
    this.drawChart();
  },
  componentDidUpdate: function() {
    this.drawChart();
  },
  render: function() {
    if(this.canRender()) {
      return <div className="chart" style={this.state.chartStyle}></div>;
    }
    return null;
  }
});