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
		values.push('<div style="float: left; width: 16px; height: 16px; margin-right: 3px; border: 1px dashed lightgray; background-color: ' + themeColors[color] + '"></div><div style="float: left;">' + color + '</div>');
  	}
  	values.push('<div style="float: left; width: 16px; height: 16px; margin-right: 3px; border: 1px dashed lightgray;"></div><div style="float: left;">bootstrap</div>');

  	var buttons = [
		<div style={{ width: 101, float: 'left' }}><Dropdown values={values} selectedValue={'Theme'} onValueChanged={ this.onThemeChanged }></Dropdown></div>,
	  	<div className="orb-tlbr-btn orb-tlbr-btn-expandall"></div>,
	  	<div className="orb-tlbr-btn orb-tlbr-btn-collapseall"></div>
  	];

  	return <div>
  		{ buttons }
  		</div>;
  }
});