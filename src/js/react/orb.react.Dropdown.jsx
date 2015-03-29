/** @jsx React.DOM */

/* global module, react, React */
/*jshint eqnull: true*/

'use strict';

module.exports.Dropdown = react.createClass({
	openOrClose: function(e) {
		var valueNode = this.refs.valueElement.getDOMNode();
		var valuesListNode = this.refs.valuesList.getDOMNode();
		if(e.target === valueNode && valuesListNode.style.display === 'none') {
			valuesListNode.style.display = 'block';
		} else {
			valuesListNode.style.display = 'none';
		}
	},
	onMouseEnter: function() {
		var valueNode = this.refs.valueElement.getDOMNode();
		valueNode.className = "orb-tgl-btn-down";
		valueNode.style.backgroundPosition = 'right center';
	},
	onMouseLeave: function() {
		this.refs.valueElement.getDOMNode().className = "";
	},
	componentDidMount: function() {
		document.addEventListener('click', this.openOrClose);
	},
	componentWillUnmount : function() {
		document.removeEventListener('click', this.openOrClose);
	},
	selectValue: function(e) {
		var listNode = this.refs.valuesList.getDOMNode();
		var target = e.target;
		var isli = false;
		while(!isli && target != null) {
			if(target.parentNode == listNode) {
				isli = true;
				break;
			}
			target = target.parentNode;
		}

		if(isli) {
			var value = target.textContent;
			var valueElement = this.refs.valueElement.getDOMNode();
			if(valueElement.textContent != value) {
				valueElement.textContent = value;
				if(this.props.onValueChanged) {
					this.props.onValueChanged(value);
				}
			}
		}
	},
	render: function() {
		function createSelectValueFunc(value) {
			return function() {
				this.selectValue(value);
			};
		}

		var values = [];
		for(var i=0; i < this.props.values.length; i++) {
			values.push(<li key={'item' + i} dangerouslySetInnerHTML={{__html: this.props.values[i]}}></li>);
		}

		return <div className="orb-select">
				<div ref="valueElement" dangerouslySetInnerHTML={{__html: this.props.selectedValue}} onMouseEnter={this.onMouseEnter} onMouseLeave={this.onMouseLeave}></div>
				<ul ref="valuesList" style={{ display: 'none' }} onClick={ this.selectValue }>
					{values}
				</ul>
			</div>;
	}
});