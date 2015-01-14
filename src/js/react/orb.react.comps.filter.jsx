/** @jsx React.DOM */

/* global module, react, React */
/*jshint eqnull: true*/

'use strict';

var FilterPanel = module.exports.FilterPanel = react.createClass({
	destroy: function() {
		var container = this.getDOMNode().parentNode;
		React.unmountComponentAtNode(container);
		container.parentNode.removeChild(container);
	},
	onFilter: function(filterValues) {
		this.props.rootComp.props.data.applyFilter(this.props.field, filterValues);
		this.destroy();
	},
	onMouseDown: function(e) {
		var container = this.getDOMNode().parentNode;
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
		new FilterManager(this, this.getDOMNode());
	},
	componentWillUnmount : function() {
		document.removeEventListener('mousedown', this.onMouseDown);
		window.removeEventListener('resize', this.destroy);
	},
	render: function () {
		var values = this.props.rootComp.props.data.getFieldValues(this.props.field);
		var checkboxes = [];

		function addCheckboxRow(value, text) {
			return checkboxes.push(<tr key={value}>
				<td className="filter-checkbox">
					<input type="checkbox" value={value} defaultChecked="checked"/>
				</td>
				<td className="filter-value" title={text || value}>{text || value}</td>
				</tr>);
		}

		addCheckboxRow(configuration.FILTER_ALL, '(Show All)');
		if(values.containsBlank) {
			addCheckboxRow(configuration.FILTER_BLANK, '(Blank)');
		}

		for(var i = 0; i < values.length; i++) {
			addCheckboxRow(values[i]);
		}

		return <table className="filter-subcontainer">
		<tbody>
			<tr>
				<td className="search-box-column"><input type="text" placeholder="search"/></td>
				<td className="search-type-column" title="Enable/disable Regular expressions">.*</td>
			</tr>
			<tr>
				<td colSpan="2" className="filter-values-column">
					<table className="filter-values-table">
					<tbody>
						{checkboxes}
					</tbody>
					</table>
				</td>
			</tr>
			<tr className="bottom-row">
				<td className="confirm-buttons-column">
					<input type="button" className="orb-button" value="Ok" style={{ float: 'left' }}/>
					<input type="button" className="orb-button" value="Cancel" style={{ float: 'left' }}/>
				</td>
				<td className="resize-column">
					<div></div>
				</td>
			</tr>
		</tbody>
		</table>;
	}
});

function FilterManager(reatComp, filterContainerElement, checkedValues) {

	var self = this;

	var allValues = [];
	var searchCheckedValues = [];
	var isSearchMode = false;
	var isRegexMode = false;
	var lastSearchTerm = '';

	var elems = {
		filterContainer: null,
		checkboxes: {},
		searchBox: null,
		allCheckbox: null,
		addCheckbox: null,
		enableRegexButton: null,
		okButton: null,
		cancelButton: null,
		resizeGrip: null
	};

	this.checkedValues = [];

	this.reset = function(newFilterContaineElement, newCheckedValues) {
		this.checkedValues = [];
		allValues = [];
		searchCheckedValues = [];
		isSearchMode = false;
		isRegexMode = false;
		lastSearchTerm = '';

		elems.filterContainer = newFilterContaineElement;
		elems.checkboxes = {};
		elems.searchBox = elems.filterContainer.rows[0].cells[0].children[0];
		elems.okButton = elems.filterContainer.rows[2].cells[0].children[0];
		elems.cancelButton = elems.filterContainer.rows[2].cells[0].children[1];
		elems.resizeGrip = elems.filterContainer.rows[2].cells[1].children[0];

		var rows = elems.filterContainer.rows[1].cells[0].children[0].rows;
		for(var i = 0; i < rows.length; i++) {
			var checkbox = rows[i].cells[0].children[0];
			elems.checkboxes[checkbox.value] = checkbox;
			allValues.push(checkbox.value);
		}

		elems.allCheckbox = elems.checkboxes[configuration.FILTER_ALL];
		elems.addCheckbox = null;
		elems.enableRegexButton = elems.filterContainer.rows[0].cells[1];

		elems.filterContainer.addEventListener('click', self.valueChecked);
		elems.searchBox.addEventListener('keyup', self.searchChanged);
		elems.enableRegexButton.addEventListener('click', function() { 
			isRegexMode = !isRegexMode;
			elems.enableRegexButton.className = elems.enableRegexButton.className.replace('search-type-column-active', '');
			if(isRegexMode) {
				elems.enableRegexButton.className += ' search-type-column-active';
			}
			self.searchChanged('regexModeChanged');
		});
		elems.okButton.addEventListener('click', function() { reatComp.onFilter(isSearchMode ? searchCheckedValues : self.checkedValues); });
		elems.cancelButton.addEventListener('click', function() { reatComp.destroy(); });		

		var resizeMan = new ResizeManager(elems.filterContainer.parentNode, elems.filterContainer.rows[1].cells[0].children[0], elems.resizeGrip);

		elems.resizeGrip.addEventListener('mousedown', resizeMan.resizeMouseDown);
		document.addEventListener('mouseup', resizeMan.resizeMouseUp);
		document.addEventListener('mousemove', resizeMan.resizeMouseMove);

		self.updateCheckboxes(newCheckedValues);
	};

	function ResizeManager(outerContainerElem, valuesTableElem, resizeGripElem) {

		var minContainerWidth = 189;
		var minContainerHeight = 201;

		var mousedownpos = {
			x: 0, y: 0
		};
		var isMouseDown = false;

		this.resizeMouseDown = function(e) {
			// drag/sort with left mouse button
			if (e.button !== 0) return;

			isMouseDown = true;
			document.body.style.cursor = 'se-resize';

			mousedownpos.x = e.pageX;
			mousedownpos.y = e.pageY;

			// prevent event bubbling (to prevent text selection while dragging for example)
			e.stopPropagation();
			e.preventDefault();
		};

		this.resizeMouseUp = function() {
			isMouseDown = false;
			document.body.style.cursor = 'auto';
			return true;
		};

		this.resizeMouseMove = function(e) {
			// if the mouse is not down while moving, return (no drag)
			if (!isMouseDown) return;

			var resizeGripSize = resizeGripElem.getBoundingClientRect();
			var outerContainerSize = outerContainerElem.getBoundingClientRect();
		    var valuesTableSize = valuesTableElem.getBoundingClientRect();

		    var outerContainerWidth = outerContainerSize.right - outerContainerSize.left;
		    var outerContainerHeight = outerContainerSize.bottom - outerContainerSize.top;

			var offset = {
				x: outerContainerWidth <= minContainerWidth && e.pageX < resizeGripSize.left ? 0 : e.pageX - mousedownpos.x,
				y: outerContainerHeight <= minContainerHeight && e.pageY < resizeGripSize.top ? 0 : e.pageY - mousedownpos.y
			};

			var newContainerWidth = outerContainerWidth  + offset.x;
		    var newContainerHeight = outerContainerHeight  + offset.y;

			mousedownpos.x = e.pageX;
			mousedownpos.y = e.pageY;

			if(newContainerWidth >= minContainerWidth) {
				outerContainerElem.style.width = newContainerWidth + 'px';
			}

			if(newContainerHeight >= minContainerHeight) {
				outerContainerElem.style.height = newContainerHeight + 'px';
				valuesTableElem.style.height = (valuesTableSize.bottom - valuesTableSize.top + offset.y) + 'px';
			}

			e.stopPropagation();
			e.preventDefault();
		};
	}

	this.valueChecked = function(e) {
		var target = e.target;
		if(target && target.type && target.type === 'checkbox') {
			self.updateCheckedValues(target == elems.allCheckbox);
		}
	};

	function escapeRegex(re) {
	    return re.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
	}

	this.searchChanged = function(e) {
		var search = (elems.searchBox.value || '').trim();
		if((e === 'regexModeChanged' && search) || search != lastSearchTerm) {
			lastSearchTerm = search;
			isSearchMode = search !== '';
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
	};

	this.updateCheckedValues = function(allChecked) {
		if(allChecked) {
			self.checkedValues = elems.allCheckbox.checked ? configuration.FILTER_ALL : configuration.FILTER_NONE;
			self.updateCheckboxes(elems.allCheckbox.checked, configuration.FILTER_ALL);
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
		console.log(self.checkedValues + '\n' + searchCheckedValues);
	};

	this.updateCheckboxes = function(checkedList, source) {
		var allchecked = utils.isArray(checkedList) ? null : (checkedList == null ? true : !!checkedList);
		for(var i = 1; i < allValues.length; i++) {
			var val = allValues[i];
			elems.checkboxes[val].checked = allchecked != null ? allchecked : checkedList.indexOf(val) >= 0;
		}

		if(source !== configuration.FILTER_ALL) {
			self.checkedValues = checkedList || (allchecked ? configuration.FILTER_ALL : configuration.FILTER_NONE);
			self.updateAllCheckbox();
		}
	};

	var INDETERMINATE = 'indeterminate';

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
	};

	this.reset(filterContainerElement, checkedValues);
}
