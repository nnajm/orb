/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.Toolbar = react.createClass({
  onThemeChanged: function(newTheme) {
  	this.props.pivotTableComp.changeTheme(newTheme);
  },
  render: function() {

  	var Dropdown = comps.Dropdown;
  	
  	var themeColors = require('../orb.themes').themes;
  	var values = [];
  	for(var color in themeColors) {
		  values.push('<div key="' + color + '-rect" style="float: left; width: 16px; height: 16px; margin-right: 3px; border: 1px dashed lightgray; background-color: ' + themeColors[color] + '"></div>' +
                  '<div key="' + color + '-name" style="float: left;">' + color + '</div>');
  	}
  	values.push('<div key="bootstrap-rect" style="float: left; width: 16px; height: 16px; margin-right: 3px; border: 1px dashed lightgray;"></div>' +
                '<div key="bootstrap-name" style="float: left;">bootstrap</div>');

  	var buttons = [
		  <div key="themeButton" className="orb-tlbr-btn" style={{ width: 101 }}><Dropdown values={values} selectedValue={'Theme'} onValueChanged={ this.onThemeChanged }></Dropdown></div>
  	];

  	return <div>
  		{ buttons }
  		</div>;
  }
});