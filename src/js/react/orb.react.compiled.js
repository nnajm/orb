/** @jsx React.DOM */

// Ensure orb.react namespace is created
orb.utils.ns('orb.react');

(function() {

var extraCol = 1;

orb.react.PivotTable = React.createClass({displayName: 'PivotTable',
  getInitialState: function() {
    orb.react.DragManager.init(this);
    return {};
  },
  sort: function(axetype, field) {
    this.props.data.sort(axetype, field);
    this.setProps(this.props);
  },
  moveButton: function(button, newAxeType, position) {
    this.props.data.moveField(button.props.field.name, button.props.axetype, newAxeType, position);
    this.setProps(this.props);
  },
  expandRow: function(cell) {
    cell.expanded = true;
    this.setProps({});
  },
  collapseRow: function(cell) {
    cell.subtotalHeader.expanded = false;
    this.setProps({});
  },
  render: function() {

    var self = this;

    var ptc = this.props.data;
    var PivotButton = orb.react.PivotButton;
    var PivotRow = orb.react.PivotRow;
    var DropTarget = orb.react.DropTarget;

    var fieldButtons = ptc.pgrid.config.availablefields().map(function(field, index) {
      return React.createElement(PivotButton, {key: field.name, 
                          field: field, 
                          axetype: null, 
                          position: index, 
                          rootComp: self}
             );
    });

    var dataButtons = ptc.pgrid.config.datafields.map(function(field, index) {
      return React.createElement(PivotButton, {key: field.name, 
                          field: field, 
                          axetype: orb.axe.Type.DATA, 
                          position: index, 
                          rootComp: self}
             );
    });

    var columnButtons = ptc.pgrid.config.columnfields.map(function(field, index) {
      return React.createElement(PivotButton, {key: field.name, 
                          field: field, 
                          axetype: orb.axe.Type.COLUMNS, 
                          position: index, 
                          rootComp: self}
             );
    });

    // get 'row buttons' row (also last row containing column headers)
    var rowButtons = orb.utils.findInArray(ptc.cells, function(row) {
      return row[0].template === 'cell-template-fieldbutton';
    });

    // build row buttons
    if(rowButtons !== undefined) {
      rowButtons = rowButtons.filter(function(buttonCell) {
        return buttonCell.template === 'cell-template-fieldbutton';
      }).map(function(buttonCell, index) {
          return React.createElement(PivotButton, {key: buttonCell.value.name, 
                              field: buttonCell.value, 
                              axetype: orb.axe.Type.ROWS, 
                              position: index, 
                              rootComp: self}
                 );
      });
    } else {
      rowButtons = [];
    }

    // build the cell that will contains 'row buttons'
    var rowButtonsCell = React.createElement("td", {className: "empty", colSpan: ptc.rowHeadersWidth + extraCol, rowSpan: "1"}, 
                          React.createElement(DropTarget, {data: rowButtons, axetype: orb.axe.Type.ROWS}
                          )
                         );

    var rows = ptc.cells.map(function(row, index) {
      if(index == ptc.columnHeadersHeight - 1) {
        return React.createElement(PivotRow, {key: index, 
                         row: row, 
                         rowButtonsCount: ptc.rowHeadersWidth, 
                         rowButtonsCell: rowButtonsCell, 
                         rootComp: self}
               );
      } else {
        return React.createElement(PivotRow, {key: index, 
                         row: row, 
                         rootComp: self}
               );
      };
    });

    var tblStyle = this.props.config.width ?  {width: this.props.config.width} : {};

    return (
    React.createElement("div", {className: "orb-container", style: tblStyle}, 
      React.createElement("table", {id: "tbl", className: "orb", style: {width: '100%'}}, 
        React.createElement("tbody", null, 
          React.createElement("tr", null, 
            React.createElement("td", {className: "available-fields field-group", colSpan: extraCol, rowSpan: "1"}, 
              React.createElement("div", {className: "field-group-caption"}, "Fields:")
            ), 
            React.createElement("td", {className: "available-fields", colSpan: ptc.totalWidth, rowSpan: "1"}, 
              React.createElement(DropTarget, {data: fieldButtons, axetype: null}
              )
            )
          ), 
          React.createElement("tr", null, 
            React.createElement("td", {className: "field-group", colSpan: extraCol, rowSpan: "1"}, 
              React.createElement("div", {className: "field-group-caption"}, "Data fields:")
            ), 
            React.createElement("td", {className: "empty", colSpan: ptc.totalWidth, rowSpan: "1"}, 
              React.createElement(DropTarget, {data: dataButtons, axetype: orb.axe.Type.DATA}
              )
            )
          ), 
          React.createElement("tr", null, 
            React.createElement("td", {className: "empty", colSpan: ptc.rowHeadersWidth + extraCol, rowSpan: "1"}), 
            React.createElement("td", {className: "empty", colSpan: ptc.columnHeadersWidth, rowSpan: "1"}, 
              React.createElement(DropTarget, {data: columnButtons, axetype: orb.axe.Type.COLUMNS}
              )
            )
          ), 
          rows
        )
      )
    )
    );
  }
});

orb.react.PivotRow = React.createClass({displayName: 'PivotRow',
  render: function() {
    var self = this;
    var PivotCell = orb.react.PivotCell;
    
    var lastCellIndex = this.props.row.length - 1;
    var cell0 = this.props.row[0];
    var cells;

    var rowstyle = {};

    if(this.props.rowButtonsCell !== undefined) {
      cells = this.props.row.slice(this.props.rowButtonsCount).map(function(cell, index) {
        var isrightmost = index === (lastCellIndex - self.props.rowButtonsCount);
        return React.createElement(PivotCell, {key: index, 
                          cell: cell, 
                          rightmost: isrightmost, 
                          leftmost: false, 
                          rootComp: self.props.rootComp}
               );
      });

      return (
        React.createElement("tr", null, 
          this.props.rowButtonsCell, 
          cells
        )
      );

    } else {

      if(cell0.template == 'cell-template-row-header' && cell0.visible && !cell0.visible()) {
        rowstyle.display = 'none';
      }

      cells = this.props.row.map(function(cell, index) {
        var isrightmost = index === lastCellIndex;
        var isleftmost = index === 0 && (
                           cell.type === orb.ui.HeaderType.EMPTY ||
                           cell.type === orb.ui.HeaderType.SUB_TOTAL || 
                           cell.type === orb.ui.HeaderType.GRAND_TOTAL || 
                           (cell.dim && (cell.dim.isRoot || cell.dim.parent.isRoot))
                         );

        return React.createElement(PivotCell, {key: index, 
                          cell: cell, 
                          rightmost: isrightmost, 
                          leftmost: isleftmost, 
                          rootComp: self.props.rootComp}
               );
      });

      return (
        React.createElement("tr", {style: rowstyle}, 
          cells
        )
      );
    }
  }
});

orb.react.PivotCell = React.createClass({displayName: 'PivotCell',
  expand: function() {
    this.props.rootComp.expandRow(this.props.cell);
  },
  collapse: function() {
    this.props.rootComp.collapseRow(this.props.cell);
  },
  render: function() {
    var cell = this.props.cell;
    var divcontent = [];
    var value;
    var vArrow = '\u25bc';
    var hArrow = '\u25b6';

    switch(cell.template) {
      case 'cell-template-row-header':
      case 'cell-template-column-header':
        if(cell.type === orb.ui.HeaderType.WRAPPER && cell.dim.field.subtotal.visible && cell.dim.field.subtotal.collapsible && cell.subtotalHeader.expanded) {
          divcontent.push(React.createElement("span", {key: "toggle-button", className: "toggle-button", onClick: this.collapse}, vArrow));
        } else if(cell.type === orb.ui.HeaderType.SUB_TOTAL && !cell.expanded){
          divcontent.push(React.createElement("span", {key: "toggle-button", className: "toggle-button", onClick: this.expand}, hArrow));
        }
        value = cell.value;
        break;
      case 'cell-template-dataheader':
        value = cell.value.caption;
        break;
      case 'cell-template-datavalue':
        value = cell.datafield && cell.datafield.formatfunc ? cell.datafield.formatfunc(cell.value) : cell.value;
        break;
      default:
        break;
    }

    divcontent.push(React.createElement("span", {key: "cell-value", style: {whiteSpace: 'nowrap'}}, value));

    var classname = cell.cssclass;
    var isHidden = !cell.visible();
    if(isHidden || this.props.rightmost || this.props.leftmost) {
      
      if(isHidden) {
        classname += ' cell-hidden';
      }

      if(this.props.rightmost && (cell.axetype !== orb.axe.Type.COLUMNS || cell.type === orb.ui.HeaderType.GRAND_TOTAL)) {
        classname += ' cell-rightmost';
      }

      if(this.props.leftmost) {
        classname += ' cell-leftmost';
        console.log('cell-leftmost: ' + cell.value);
      }
    }

    return React.createElement("td", {className: classname, 
               colSpan: cell.hspan() + (this.props.leftmost ? extraCol : 0), 
               rowSpan: cell.vspan()}, 
                React.createElement("div", null, 
                  divcontent
                )
           );
  }
});

})();
/** @jsx React.DOM */

// Ensure orb.react namespace is created
orb.utils.ns('orb.react');

(function() {

function forEach(list, func, defStop) {
	var ret;
	if(list != null) {
		for(var i = 0, l = list.length; i < l; i++) {
			ret = func(list[i], i);
			if(ret !== undefined && defStop === true) {
				break;
			}
		}
	}
	return ret;
}

orb.react.DragManager = (function() {
	
	var _pivotComp = null;
	var _dragElement = null;
	var _dragNode = null;
	var _dropTargets = [];
	var _dropIndicators = [];

	function doElementsOverlap(elem1Rect, elem2Rect) {
		return !(elem1Rect.right < elem2Rect.left || 
                elem1Rect.left > elem2Rect.right || 
                elem1Rect.bottom < elem2Rect.top || 
                elem1Rect.top > elem2Rect.bottom);
	}

	function signalDragOver(target) {
		if(target.onDragOver) {
			target.onDragOver(_dragElement);
			return true;
		}
		return false;
	}

	function signalDragEnd(target) {
		if(target.onDragEnd) {
			target.onDragEnd();
			return true;
		}
		return false;
	}

	function getDropTarget() {
		return forEach(_dropTargets, function(target) {
			if(target.component.state.isover) {
				return target;
			}
		}, true);
	}

	function getDropIndicator() {
		return forEach(_dropIndicators, function(indicator) {
			if(indicator.component.state.isover) {
				return indicator;
			}
		}, true);
	}

	var _initialized = false;

	return {
		init: function(pivotComp) {
			_initialized = true;
			_pivotComp = pivotComp;
		},
		dragElement: function(elem) {
			
			var prevDragElement = _dragElement;
			_dragElement = elem;
			if(_dragElement != prevDragElement) {
				if(elem == null) {

					// Drop Target
					var dropTarget = getDropTarget();
					// Drop Indicator
					var dropIndicator = getDropIndicator();

					if(dropTarget) {
						var position = dropIndicator != null ? dropIndicator.position : null;
						_pivotComp.moveButton(prevDragElement, dropTarget.component.props.axetype, position);
					}

					_dragNode = null;
					forEach(_dropTargets, function(target) {
						signalDragEnd(target);
					});

					forEach(_dropIndicators, function(indicator) {
						signalDragEnd(indicator);
					});

				} else {
					_dragNode = _dragElement.getDOMNode();
				}
			}
		},
		registerTarget: function(target, axetype, dragOverHandler, dargEndHandler) {
			_dropTargets.push({
				component: target,
				axetype: axetype,
				onDragOver: dragOverHandler,
				onDragEnd: dargEndHandler
			});
		},
		unregisterTarget: function(target) {
			var tindex;
			for(var i = 0; i < _dropTargets.length; i++) {
				if(_dropTargets[i].component == target) {
					tindex = i;
					break;
				}
			}
			if(tindex != null) {
				_dropTargets.splice(tindex, 1);
			};
		},
		registerIndicator: function(indicator, axetype, position, dragOverHandler, dargEndHandler) {
			_dropIndicators.push({
				component: indicator,
				axetype: axetype,
				position: position,
				onDragOver: dragOverHandler,
				onDragEnd: dargEndHandler
			});
		},
		unregisterIndicator: function(indicator) {
			var iindex;
			for(var i = 0; i < _dropIndicators.length; i++) {
				if(_dropIndicators[i].component == indicator) {
					iindex = i;
					break;
				}
			}
			if(iindex != null) {
				_dropIndicators.splice(iindex, 1);
			};
		},
		elementMoved: function() {
			if(_dragElement != null) {
				var dragNodeRect = _dragNode.getBoundingClientRect();
				var foundTarget;

				forEach(_dropTargets, function(target) {
					if(!foundTarget) {
						var tnodeRect = target.component.getDOMNode().getBoundingClientRect();
						var isOverlap = doElementsOverlap(dragNodeRect, tnodeRect);
						if(isOverlap && signalDragOver(target)) {
							foundTarget = target;
							return true;
						} else {
							signalDragEnd(target);
						}
					}
				}, true);

				var foundIndicator;

				if(foundTarget) {
					forEach(_dropIndicators, function(indicator, index) {
						if(!foundIndicator) {
							var elementOwnIndicator = indicator.component.props.axetype === _dragElement.props.axetype
												&& indicator.component.props.position === _dragElement.props.position;

							var targetIndicator = indicator.component.props.axetype === foundTarget.component.props.axetype;
							if(targetIndicator && !elementOwnIndicator) {	
								var tnodeRect = indicator.component.getDOMNode().getBoundingClientRect();
								var isOverlap = doElementsOverlap(dragNodeRect, tnodeRect);
								if(isOverlap && signalDragOver(indicator)) {
									foundIndicator = indicator;
									return;
								}
							}
						}

						signalDragEnd(indicator);
					});

					if(!foundIndicator) {
						var axeIndicators = _dropIndicators.filter(function(indicator) {
							return indicator.component.props.axetype === foundTarget.component.props.axetype;
						});
						if(axeIndicators.length > 0) {
							signalDragOver(axeIndicators[axeIndicators.length - 1]);
						}
					}
				} else {
					forEach(_dropIndicators, function(indicator, index) {
						signalDragEnd(indicator);
					});
				}
			}
		}
	};
}());

var dtid = 0;

orb.react.DropTarget = React.createClass({displayName: 'DropTarget',
	getInitialState: function () {
		this.dtid = ++dtid;
		// initial state, all zero.
		orb.react.DragManager.registerTarget(this, this.props.axetype, this.onDragOver, this.onDragEnd);
		return {
			isover: false
		};
	},
	componentWillUnmount : function() {
		orb.react.DragManager.unregisterTarget(this);
	},
	onDragOver: function(component) {
		this.setState({
			isover: true
		})
	},
	onDragEnd: function() {
		this.setState({
			isover: false
		})
	},
	render: function() {	
		var self = this;
		var DropIndicator = orb.react.DropIndicator;
		var buttons = this.props.data.map(function(button, index) {			
			if(index < self.props.data.length - 1) {
				return [
					React.createElement(DropIndicator, {isFirst: index === 0, position: index, axetype: self.props.axetype}),
					button
				];
			} else {
				return [
					React.createElement(DropIndicator, {isFirst: index === 0, position: index, axetype: self.props.axetype}),
					button,
					React.createElement(DropIndicator, {isLast: true, position: null, axetype: self.props.axetype})
				];
			}
		});

		return React.createElement("div", {className: 'drop-target' + (this.state.isover ? ' drag-over' : '')}, 
				buttons
			   );
	}
});

function getOffset(element) {
	if(element != null) {
	    var rect = element.getBoundingClientRect();
	    return { x: rect.left, y: rect.top };
	}
    return { x: 0, y: 0 };
}

function getSize(element) {
	if(element != null) {
	    var rect = element.getBoundingClientRect();
	    return { width: rect.right - rect.left, height: rect.bottom - rect.top};
	}
    return { x: 0, y: 0 };
}

orb.react.DropIndicator = React.createClass({
	displayName: 'DropIndicator',
	getInitialState: function () {
		orb.react.DragManager.registerIndicator(this, this.props.axetype, this.props.position, this.onDragOver, this.onDragEnd);
		return {
			isover: false
		};
	},
	componentWillUnmount : function() {
		orb.react.DragManager.unregisterIndicator(this);
	},
	onDragOver: function(component) {
		this.setState({
			isover: true,
			width: component.getDOMNode().style.width
		})
	},
	onDragEnd: function() {
		this.setState({
			isover: false,
			width: null
		})
	},
	render: function() {
		var classname = 'drop-indicator';

		if(this.props.isFirst) {
			classname += ' drop-indicator-first';
		}

		if(this.props.isLast) {
			classname += ' drop-indicator-last';
		}

		var style = {};
		if(this.state.isover) {
			classname += ' drop-indicator-drag-over';
		}

		return React.createElement("div", {style: style, className: classname});
	}
});

var pbid = 0;

orb.react.PivotButton = React.createClass({
	displayName: 'PivotButton',
	getInitialState: function () {
		this.pbid = ++pbid;

		// initial state, all zero.
		return {
			pos: { x: 0, y: 0 },
			startpos: { x: 0, y: 0 },
			mousedown: false,
			dragging: false
		};
	},
	onMouseDown: function(e) {
		// drag/sort with left mouse button
		if (e.button !== 0) return;

		var thispos = getOffset(this.getDOMNode());
		
		// inform mousedown, save start pos
		this.setState({
			mousedown: true,
			mouseoffset: {
				x: thispos.x - e.pageX,
				y: thispos.y - e.pageY,
			},
			startpos: {
				x: e.pageX,
				y: e.pageY
			}
		});
		// prevent event bubbling (to prevent text selection while dragging for example)
		e.stopPropagation();
		e.preventDefault();
	},
	componentDidUpdate: function () {
		if (!this.state.mousedown) {
			// mouse not down, don't care about mouse up/move events.
			orb.react.DragManager.dragElement(null);
			document.removeEventListener('mousemove', this.onMouseMove)
			document.removeEventListener('mouseup', this.onMouseUp)
		} else if (this.state.mousedown) {
			// mouse down, interested by mouse up/move events.
			orb.react.DragManager.dragElement(this);
			document.addEventListener('mousemove', this.onMouseMove)
			document.addEventListener('mouseup', this.onMouseUp)
		}
	},
	componentWillUnmount : function() {
		document.removeEventListener('mousemove', this.onMouseMove)
		document.removeEventListener('mouseup', this.onMouseUp)
	},
	onMouseUp: function() {
		var wasdragging = this.state.dragging;

		this.setState({
			mousedown: false,
			dragging: false,
			size: null,
			pos: {
				x: 0,
				y: 0
			}
		});

		// if button was not dragged, proceed as a click
		if(!wasdragging) {
			this.props.rootComp.sort(this.props.axetype, this.props.field)
		}
	},
	onMouseMove: function (e) {
		console.log('PivotButton[' + this.pbid + '].onMouseMove');

		// if the mouse is not down while moving, return (no drag)
		if (!this.state.mousedown) return

		var size = null;
		if(!this.state.dragging) {
			size = getSize(this.getDOMNode());
		} else {
			size = this.state.size;
		}

		var newpos = {
			x: e.pageX + this.state.mouseoffset.x,
			y: e.pageY + this.state.mouseoffset.y
		};

		this.setState({
			dragging: true,
			size: size,
			pos: newpos
		});

		orb.react.DragManager.elementMoved();

		e.stopPropagation();
		e.preventDefault();
	},
	render: function() {
		var self = this;
		var divstyle = {
			left: self.state.pos.x + 'px',
			top: self.state.pos.y + 'px',
			position: self.state.dragging ? 'absolute' : ''
		};

		if(self.state.size) {
			divstyle.width = self.state.size.width + 'px';
		}

		var DropIndicator = orb.react.DropIndicator;
		var sortIndicator = self.props.field.sort.order === 'asc' ? 
		' \u25B3' :
		(self.props.field.sort.order === 'desc' ?
			' \u25BD' :
			'' );

		return React.createElement("div", {key: self.props.field.name, 
		            className: "field-button", 
		            onMouseDown: this.onMouseDown, 
		            style: divstyle}, 
		            	self.props.field.caption, 
		            	React.createElement("span", null, sortIndicator)
		        );
	}
});

})();