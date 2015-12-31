/* global module, require, react */
/*jshint eqnull: true*/

'use strict';

var React = typeof window === 'undefined' ? require('react') : window.React,
    DragManager = require('./orb.react.DragManager.jsx'),
    DropIndicator = require('./orb.react.DropIndicator.jsx'),    
    axe = require('../orb.axe'),
    dtid = 0;

module.exports = React.createClass({
	getInitialState: function () {
		this.dtid = ++dtid;
		return {
			isover: false
		};
	},
  	componentDidMount: function() {
  		DragManager.registerTarget(this, this.props.axetype, this.onDragOver, this.onDragEnd);
  	},
	componentWillUnmount : function() {
		DragManager.unregisterTarget(this);
	},
	onDragOver: function(callback) {
		if(this.isMounted()) {
			this.setState({
				isover: true
			}, callback);
		} else if(callback) {
			callback();
		}
	},
	onDragEnd: function(callback) {
		if(this.isMounted()) {
			this.setState({
				isover: false
			}, callback);
		} else if(callback) {
			callback();
		}
	},
	render: function() {	
		var self = this;

		var buttons = this.props.buttons.map(function(button, index) {			
			if(index < self.props.buttons.length - 1) {
				return [
					<td><DropIndicator isFirst={index === 0} position={index} axetype={self.props.axetype}></DropIndicator></td>,
					<td>{ button }</td>
				];
			} else {
				return [
					<td><DropIndicator isFirst={index === 0} position={index} axetype={self.props.axetype}></DropIndicator></td>,
					<td>{ button }</td>,
					<td><DropIndicator isLast={true} position={null} axetype={self.props.axetype}></DropIndicator></td>
				];
			}
		});

		var style = self.props.axetype === axe.Type.ROWS ? { position: 'absolute', left: 0, bottom: 11 } : null;

		return <div className={'drp-trgt' + (this.state.isover ? ' drp-trgt-over' : '') + (buttons.length === 0 ? ' drp-trgt-empty' : '')} style={style}>
			<table>
			<tbody>
				<tr>
					{buttons}
				</tr>
			</tbody>
			</table>
		</div>;
	}
});