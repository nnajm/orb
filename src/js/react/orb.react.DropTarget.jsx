/** @jsx React.DOM */

/* global module, require, react */
/*jshint eqnull: true*/

'use strict';

var dtid = 0;

module.exports.DropTarget = react.createClass({
	getInitialState: function () {
		this.dtid = ++dtid;
		// initial state, all zero.
		dragManager.registerTarget(this, this.props.axetype, this.onDragOver, this.onDragEnd);
		return {
			isover: false
		};
	},
	componentWillUnmount : function() {
		dragManager.unregisterTarget(this);
	},
	onDragOver: function(component) {
		this.setState({
			isover: true
		});
	},
	onDragEnd: function() {
		this.setState({
			isover: false
		});
	},
	render: function() {	
		var self = this;
		var DropIndicator = module.exports.DropIndicator;
		var buttons = this.props.buttons.map(function(button, index) {			
			if(index < self.props.buttons.length - 1) {
				return [
					<DropIndicator isFirst={index === 0} position={index} axetype={self.props.axetype}></DropIndicator>,
					button
				];
			} else {
				return [
					<DropIndicator isFirst={index === 0} position={index} axetype={self.props.axetype}></DropIndicator>,
					button,
					<DropIndicator isLast={true} position={null} axetype={self.props.axetype}></DropIndicator>
				];
			}
		});

		return <div className={'drp-trgt' + (this.state.isover ? ' drp-trgt-over' : '')}>
				{buttons}
			   </div>;
	}
});