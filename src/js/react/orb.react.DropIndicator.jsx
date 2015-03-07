/** @jsx React.DOM */

/* global module, require, react */
/*jshint eqnull: true*/

'use strict';


module.exports.DropIndicator = react.createClass({
	displayName: 'DropIndicator',
	getInitialState: function () {
		dragManager.registerIndicator(this, this.props.axetype, this.props.position, this.onDragOver, this.onDragEnd);
		return {
			isover: false
		};
	},
	componentWillUnmount : function() {
		dragManager.unregisterIndicator(this);
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
		var classname = 'drp-indic';

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