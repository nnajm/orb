/** @jsx React.DOM */

/* global module, require, React */
/*jshint eqnull: true*/

'use strict';

var FilterPanel = module.exports.FilterPanel = react.createClass({
	destroy: function() {
		var container = this.getDOMNode().parentNode
		React.unmountComponentAtNode(container);
		container.parentNode.removeChild(container);
	},
	onMouseDown: function(e) {
		var container = this.getDOMNode().parentNode
		var target = e.target;
		while(target != null) {
			if(target == container) {
				return true;
			}
			target = target.parentNode;
		}

		this.destroy();
	},
	componentWillMount : function() {
		document.addEventListener('mousedown', this.onMouseDown);
		window.addEventListener('resize', this.destroy);
	},
	componentDidMount: function() {
		new filterManager(this, this.getDOMNode());
	},
	componentWillUnmount : function() {
		document.removeEventListener('mousedown', this.onMouseDown);
		window.removeEventListener('resize', this.destroy);
	},
	render: function () {
		var values = this.props.rootComp.props.data.getFieldValues(this.props.field);
		var checkboxes = [];

		function addCheckboxRow(value, text) {
			return checkboxes.push(<tr>
				<td className="filter-checkbox">
					<input type="checkbox" value={value} defaultChecked="checked"/>
				</td>
				<td className="filter-value" title={text || value}>{text || value}</td>
				</tr>);
		}

		addCheckboxRow('#All#', '(Show All)');
		if(values.containsBlank) {
			addCheckboxRow('#Blank#"', '(Blank)');
		}

		for(var i = 0; i < values.length; i++) {
			addCheckboxRow(values[i]);
		}

		return <div>
				<div className="filter-search-box">
					<input type="text" className="filter-search-box-value" placeholder="search"/>
					<div className="filter-search-box-regex">.*</div>
				</div>
				<div className="filter-values-table-container">
					<table className="filter-values-table">
					<tbody>
						{checkboxes}
					</tbody>
					</table>
				</div>
				<div className="filter-confirm-buttons">
					<input type="button" className="orb-button" value="Ok" style={{ float: 'right' }}/>
					<input type="button" className="orb-button" value="Cancel" style={{ float: 'right' }}/>
				</div>
			   </div>;
	}
});

function filterManager(reatComp, filterContainerElement, checkedValues) {

	var self = this;

	var ALL = '#All#';
	var NONE = '#None#';
	var INDETERMINATE = 'indeterminate';

	var checked = [];
	var allValues = [];
	var searchCheckedValues = [];
	var isSearchMode = false;
	var isRegexMode = false;

	var elems = {
		filterContainer: null,
		checkboxes: {},
		searchBox: null,
		allCheckbox: null,
		addCheckbox: null,
		enableRegexButton: null,
		okButton: null,
		cancelButton: null,
	};

	this.checkedValues = [];

	this.reset = function(newFilterContaineElement, newCheckedValues) {
		this.checkedValues = [];
		allValues = [];
		searchCheckedValues = [];
		isSearchMode = false;
		isRegexMode = false;

		elems.filterContainer = newFilterContaineElement;
		elems.checkboxes = {};
		elems.searchBox = elems.filterContainer.children[0].children[0];
		elems.okButton = elems.filterContainer.children[2].children[0];
		elems.cancelButton = elems.filterContainer.children[2].children[1];

		var rows = elems.filterContainer.children[1].children[0].rows;
		for(var i = 0; i < rows.length; i++) {
			var checkbox = rows[i].cells[0].children[0];
			elems.checkboxes[checkbox.value] = checkbox;
			allValues.push(checkbox.value);
		}

		elems.allCheckbox = elems.checkboxes[ALL];
		elems.addCheckbox = null;
		elems.enableRegexButton = elems.filterContainer.children[0].children[1];

		elems.filterContainer.addEventListener('click', self.valueChecked);
		elems.searchBox.addEventListener('keyup', self.searchChanged);
		elems.enableRegexButton.addEventListener('click', function() { 
			isRegexMode = !isRegexMode;
			elems.enableRegexButton.className = elems.enableRegexButton.className.replace('filter-search-box-regex-active', '');
			if(isRegexMode) {
				elems.enableRegexButton.className += ' filter-search-box-regex-active';
			}
			self.searchChanged();
		});
		elems.okButton.addEventListener('click', function() { reatComp.destroy(); });
		elems.cancelButton.addEventListener('click', function() { reatComp.destroy(); });

		self.updateCheckboxes(newCheckedValues);
	}

	this.valueChecked = function(e) {
		var target = e.target;
		if(target && target.type && target.type === 'checkbox') {
			self.updateCheckedValues(target == elems.allCheckbox);
			//log.textContent = JSON.stringify(self.checkedValues, null, 2) + '\n' + JSON.stringify(searchCheckedValues, null, 2);
		}
	}

	function escapeRegex(re) {
	    return re.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
	};

	this.searchChanged = function() {
		var search = (elems.searchBox.value || '').trim();
		isSearchMode = search != '';
		var searchRegex = isSearchMode ? new RegExp(isRegexMode ? search : escapeRegex(search), 'i') : undefined;
		var defaultDisplay = search ? 'none' : '';

		elems.allCheckbox.parentNode.parentNode.style.display = defaultDisplay;
		for(var i = 1; i < allValues.length; i++) {
			var val = allValues[i];
			var checkbox = elems.checkboxes[val];
			var visible = !isSearchMode || (val.search(searchRegex) >= 0);
			checkbox.parentNode.parentNode.style.display = visible ? '' : defaultDisplay;
			checkbox.checked = visible;
		}

		if(!isSearchMode) {
			searchCheckedValues = [];
			self.updateCheckboxes(self.checkedValues);
		} else {
			self.updateCheckedValues();
		}
	}

	this.updateCheckedValues = function(allChecked) {
		if(allChecked) {
			self.checkedValues = elems.allCheckbox.checked ? ALL : NONE;
			self.updateCheckboxes(elems.allCheckbox.checked, ALL);
		} else {
			var checkedArray = [];
			for(var i = 1; i < allValues.length; i++) {
				var val = allValues[i];
				var checkbox = elems.checkboxes[val];
				if(checkbox.checked) {
					checkedArray.push(val);
				}
			}

			if(isSearchMode) {
				searchCheckedValues = checkedArray;
			} else {
				self.checkedValues = checkedArray;
			}
			self.updateAllCheckbox();
		}
	}

	this.updateCheckboxes = function(checkedList, source) {
		var allchecked = utils.isArray(checkedList) === '[object Array]' ? null : (checkedList == null ? true : !!checkedList);
		for(var i = 1; i < allValues.length; i++) {
			var val = allValues[i];
			elems.checkboxes[val].checked = allchecked != null ? allchecked : checkedList.indexOf(val) >= 0;
		}

		if(source !== ALL) {
			self.checkedValues = checkedList || (allchecked ? ALL : NONE);
			self.updateAllCheckbox();
		}
	}

	this.updateAllCheckbox = function() {
		if(!isSearchMode) {
			var allchecked = null;
			for(var i = 1; i < allValues.length; i++) {
				var checkbox = elems.checkboxes[allValues[i]];
				if(allchecked == null) {
					allchecked = checkbox.checked;
				} else {
					if(allchecked !== checkbox.checked) {
						allchecked = INDETERMINATE;
						break;
					}
				}
			}

			if(allchecked === INDETERMINATE) {
				elems.allCheckbox.indeterminate = true;
			} else {
				elems.allCheckbox.indeterminate = false;
				elems.allCheckbox.checked = allchecked;
			}
		}
	}

	this.reset(filterContainerElement, checkedValues);
}
