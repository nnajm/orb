/** @jsx React.DOM */

/* global module, require, react */
/*jshint eqnull: true*/

'use strict';

var dtid = 0;

module.exports.DropTargetVertical = react.createClass({
	getInitialState: function () {
		this.dtid = ++dtid;
		return {
			isover: false
		};
	},
  	componentDidMount: function() {
  		dragManager.registerTarget(this, this.props.axetype, this.onDragOver, this.onDragEnd);
  	},
	componentWillUnmount : function() {
		dragManager.unregisterTarget(this);
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
		var DropIndicator = module.exports.DropIndicator;

		var buttons = this.props.buttons.map(function(button, index) {			
			var currButton = [
					<tr><td><DropIndicator isFirst={index === 0} position={index} axetype={self.props.axetype} isVertical={true}></DropIndicator></td></tr>,
					<tr><td>{ button }</td></tr>
				];

			if(index == self.props.buttons.length - 1) {
				currButton.push(
					<tr><td><DropIndicator isLast={true} position={null} axetype={self.props.axetype} isVertical={true}></DropIndicator></td></tr>
				);
			}

			return currButton;
		});

		return <div className={'drp-trgt-vertical' + (this.state.isover ? ' drp-trgt-over' : '') + (buttons.length === 0 ? ' drp-trgt-vertical-empty' : '')}>
			<table>
			<tbody>
				{buttons}
			</tbody>
			</table>
		</div>;
	}
});