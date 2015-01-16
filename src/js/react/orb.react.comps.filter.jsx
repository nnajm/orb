/** @jsx React.DOM */

/* global module, react, React */
/*jshint eqnull: true*/

'use strict';

module.exports.FilterPanel = react.createClass({
	pgridwidget: null,
	values: null,
	getInitialState: function() {
		this.pgridwidget = this.props.rootComp.props.data;
		return {};
	},
	destroy: function() {
		var container = this.getDOMNode().parentNode;
		React.unmountComponentAtNode(container);
		container.parentNode.removeChild(container);
	},
	onFilter: function(filterValues) {
		this.pgridwidget.applyFilter(this.props.field, filterValues);
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
	onMouseWheel: function(e) {
		var valuesTable = this.getDOMNode().rows[1].cells[0].children[0];		
		var target = e.target;
		while(target != null) {
			if(target == valuesTable) {
				if(valuesTable.scrollHeight <= valuesTable.clientHeight) {
					e.stopPropagation();
					e.preventDefault();
				}
				return;
			}
			target = target.parentNode;
		}

		this.destroy();
	},
	componentWillMount : function() {
		document.addEventListener('mousedown', this.onMouseDown);
		document.addEventListener('wheel', this.onMouseWheel);
		window.addEventListener('resize', this.destroy);
	},
	componentDidMount: function() {
		new FilterManager(this, this.getDOMNode(), this.pgridwidget.pgrid.getFieldFilter(this.props.field));
	},
	componentWillUnmount : function() {
		document.removeEventListener('mousedown', this.onMouseDown);
		document.removeEventListener('wheel', this.onMouseWheel);
		window.removeEventListener('resize', this.destroy);
	},
	render: function () {
		var checkboxes = [];
		this.values = this.pgridwidget.pgrid.getFieldValues(this.props.field);

		function addCheckboxRow(value, text) {
			return checkboxes.push(<tr key={value}>
				<td className="filter-checkbox">
					<input type="checkbox" value={value} defaultChecked="checked"/>
				</td>
				<td className="filter-value" title={text || value}>{text || value}</td>
				</tr>);
		}

		addCheckboxRow(filtering.ALL, '(Show All)');
		if(this.values.containsBlank) {
			addCheckboxRow(filtering.BLANK, '(Blank)');
		}

		for(var i = 0; i < this.values.length; i++) {
			addCheckboxRow(this.values[i]);
		}

		var buttonClass = 'orb-button' + (this.props.rootComp.props.data.pgrid.config.bootstrap ? ' btn btn-default btn-xs' : '');
		var pivotStyle = window.getComputedStyle(this.props.rootComp.getDOMNode(), null );
		var style = {
			fontFamily: pivotStyle.getPropertyValue('font-family'),
            fontSize: pivotStyle.getPropertyValue('font-size')
        };

		return <table className="filter-subcontainer" style={style}>
		<tbody>
			<tr>
				<td className="search-operator-column">
					<div className="orb-select">
						<div>{filtering.Operators.MATCH.name}</div>
						<ul>
							<li>{filtering.Operators.MATCH.name}</li>
							<li>{filtering.Operators.NOTMATCH.name}</li>
							<li>{filtering.Operators.EQ.name}</li>
							<li>{filtering.Operators.NEQ.name}</li>
							<li>{filtering.Operators.GT.name}</li>
							<li>{filtering.Operators.GTE.name}</li>
							<li>{filtering.Operators.LT.name}</li>
							<li>{filtering.Operators.LTE.name}</li>
						</ul>
					</div>
				</td>
				<td className="search-type-column" title="Enable/disable Regular expressions">.*</td>
				<td className="search-box-column"><input type="text" placeholder="search"/></td>
			</tr>
			<tr>
				<td colSpan="3" className="filter-values-column">
					<table className="filter-values-table">
					<tbody>
						{checkboxes}
					</tbody>
					</table>
				</td>
			</tr>
			<tr className="bottom-row">
				<td className="confirm-buttons-column" colSpan="2">
					<input type="button" className={buttonClass} value="Ok" style={{ float: 'left' }}/>
					<input type="button" className={buttonClass} value="Cancel" style={{ float: 'left' }}/>
				</td>
				<td className="resize-column">
					<div></div>
				</td>
			</tr>
		</tbody>
		</table>;
	}
});

function FilterManager(reactComp, filterContainerElement, checkedValues) {

	var self = this;
	var INDETERMINATE = 'indeterminate';

	var savedCheckedValues;
	var isSearchMode = false;
	var isRegexMode = false;
	var operator = filtering.Operators.MATCH;
	var lastSearchTerm = '';

	var elems = {
		filterContainer: null,
		checkboxes: {},
		searchBox: null,
		operatorBox: null,
		allCheckbox: null,
		addCheckbox: null,
		enableRegexButton: null,
		okButton: null,
		cancelButton: null,
		resizeGrip: null
	};

	this.reset = function(newFilterContaineElement, newCheckedValues) {
		isSearchMode = false;
		isRegexMode = false;
		lastSearchTerm = '';

		elems.filterContainer = newFilterContaineElement;
		elems.checkboxes = {};
		elems.searchBox = elems.filterContainer.rows[0].cells[2].children[0];
		elems.operatorBox = elems.filterContainer.rows[0].cells[0].children[0];
		elems.okButton = elems.filterContainer.rows[2].cells[0].children[0];
		elems.cancelButton = elems.filterContainer.rows[2].cells[0].children[1];
		elems.resizeGrip = elems.filterContainer.rows[2].cells[1].children[0];

		var rows = elems.filterContainer.rows[1].cells[0].children[0].rows;
		for(var i = 0; i < rows.length; i++) {
			var checkbox = rows[i].cells[0].children[0];
			elems.checkboxes[checkbox.value] = checkbox;
		}

		elems.allCheckbox = elems.checkboxes[filtering.ALL];
		elems.addCheckbox = null;
		elems.enableRegexButton = elems.filterContainer.rows[0].cells[1];
		self.toggleRegexpButton();

		elems.filterContainer.addEventListener('click', self.valueChecked);
		elems.searchBox.addEventListener('keyup', self.searchChanged);
		elems.okButton.addEventListener('click', function() { reactComp.onFilter(self.getCheckedValues()); });
		elems.cancelButton.addEventListener('click', function() { reactComp.destroy(); });		

		var dropdownManager = new DropdownManager(elems.operatorBox, function(newOperator) {
			if(operator.name !== newOperator) {
				operator = filtering.Operators.get(newOperator);
				self.toggleRegexpButton();
				self.searchChanged('operatorChanged');
			}
		});

		var resizeMan = new ResizeManager(elems.filterContainer.parentNode, elems.filterContainer.rows[1].cells[0].children[0], elems.resizeGrip);

		elems.resizeGrip.addEventListener('mousedown', resizeMan.resizeMouseDown);
		document.addEventListener('mouseup', resizeMan.resizeMouseUp);
		document.addEventListener('mousemove', resizeMan.resizeMouseMove);

		self.updateCheckboxes(newCheckedValues);
		self.updateAllCheckbox();
	};

	function ResizeManager(outerContainerElem, valuesTableElem, resizeGripElem) {

		var minContainerWidth = 301;
		var minContainerHeight = 223;

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

	function DropdownManager(dropdowElement, valueChangedCallback) {
		var valueElement = dropdowElement.children[0];
		var listElement = dropdowElement.children[1];
		valueElement.addEventListener('click', function(e) {
			if(listElement.style.display !== 'block') {
				listElement.style.display = 'block';
				e.preventDefault();
				e.stopPropagation();
			}
		});
		listElement.addEventListener('click', function(e) {
			if(e.target.parentNode == listElement) {
				if(valueElement.textContent != e.target.textContent) {
					valueChangedCallback(valueElement.textContent = e.target.textContent);
				}
			}
		});
		document.addEventListener('click', function(e) {
			listElement.style.display = 'none';
		});
	}

	this.toggleRegexpButton = function() {
		if(operator.regexpSupported) {
			elems.enableRegexButton.addEventListener('click', self.regexpActiveChanged);
			elems.enableRegexButton.className = elems.enableRegexButton.className.replace(/\s+search\-type\-column\-hidden/, '');
			
		} else {
			elems.enableRegexButton.removeEventListener('click', self.regexpActiveChanged);
			elems.enableRegexButton.className += ' search-type-column-hidden';
		}
	}

	this.regexpActiveChanged = function() { 
		isRegexMode = !isRegexMode;
		elems.enableRegexButton.className = elems.enableRegexButton.className.replace('search-type-column-active', '');
		if(isRegexMode) {
			elems.enableRegexButton.className += ' search-type-column-active';
		}
		self.searchChanged('regexModeChanged');
	};

	this.valueChecked = function(e) {
		var target = e.target;
		if(target && target.type && target.type === 'checkbox') {
			if(target == elems.allCheckbox) {
				self.updateCheckboxes(elems.allCheckbox.checked);
			} else {
				self.updateAllCheckbox();
			}
		}
	};

	this.searchChanged = function(e) {
		var search = (elems.searchBox.value || '').trim();
		if(e === 'operatorChanged' || (e === 'regexModeChanged' && search) || search != lastSearchTerm) {
			lastSearchTerm = search;
			
			var previousIsSearchMode = isSearchMode;
			isSearchMode = search !== '';
			if(isSearchMode && !previousIsSearchMode) {
				savedCheckedValues = self.getCheckedValues();
			}

			var searchTerm = operator.regexpSupported && isSearchMode ? new RegExp(isRegexMode ? search : utils.escapeRegex(search), 'i') : search;
			var defaultDisplay = search ? 'none' : '';

			elems.allCheckbox.parentNode.parentNode.style.display = defaultDisplay;
			for(var i = 0; i < reactComp.values.length; i++) {
				var val = reactComp.values[i];
				var checkbox = elems.checkboxes[val];
				if(utils.isString(val)) {
					val = val.toUpperCase();
					searchTerm = searchTerm.toUpperCase();
				}
				var visible = !isSearchMode || operator.func(val, searchTerm);
				checkbox.parentNode.parentNode.style.display = visible ? '' : defaultDisplay;
				checkbox.checked = visible;
			}

			if(!isSearchMode && previousIsSearchMode) {
				self.updateCheckboxes(savedCheckedValues);
			}

			self.updateAllCheckbox();
		}
	};

	this.getCheckedValues = function() {
		if(!isSearchMode && !elems.allCheckbox.indeterminate) {
			return elems.allCheckbox.checked ? filtering.ALL : filtering.NONE;
		} else {
			var checkedArray = [];
			for(var i = 0; i < reactComp.values.length; i++) {
				var val = reactComp.values[i];
				var checkbox = elems.checkboxes[val];
				if(checkbox.checked) {
					checkedArray.push(val);
				}
			}
			return checkedArray;
		}
	};

	this.updateCheckboxes = function(checkedList) {
		var allchecked = utils.isArray(checkedList) ?
			null :
			(checkedList == null || checkedList === filtering.ALL ?
				true :
				(checkedList === filtering.NONE ? 
					false :
					!!checkedList
				)
			);
		for(var i = 0; i < reactComp.values.length; i++) {
			var val = reactComp.values[i];
			elems.checkboxes[val].checked = allchecked != null ? allchecked : checkedList.indexOf(val) >= 0;
		}
	};

	this.updateAllCheckbox = function() {
		if(!isSearchMode) {
			var allchecked = null;
			for(var i = 0; i < reactComp.values.length; i++) {
				var checkbox = elems.checkboxes[reactComp.values[i]];
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
				elems.allCheckbox.checked = false;
			} else {
				elems.allCheckbox.indeterminate = false;
				elems.allCheckbox.checked = allchecked;
			}
		}
	};

	this.reset(filterContainerElement, checkedValues);
}
