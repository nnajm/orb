/* global module, require, react */
/*jshint eqnull: true*/

'use strict';

var React = typeof window === 'undefined' ? require('react') : window.React,
    DragManager = require('./orb.react.DragManager.jsx');

module.exports = React.createClass({
	displayName: 'DropIndicator',
	getInitialState: function () {
		DragManager.registerIndicator(this, this.props.axetype, this.props.position, this.onDragOver, this.onDragEnd);
		return {
			isover: false
		};
	},
	componentWillUnmount : function() {
		DragManager.unregisterIndicator(this);
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
		var classname = 'drp-indic' + (this.props.isVertical ? '-vertical' : '');

		if(this.props.isFirst) {
			classname += ' drp-indic-first';
		}

		if(this.props.isLast) {
			classname += ' drp-indic-last';
		}

		var style = {};
		if(this.state.isover) {
			classname += ' drp-indic-over';
		}

		return <div style={style} className={classname}></div>;
	}
});