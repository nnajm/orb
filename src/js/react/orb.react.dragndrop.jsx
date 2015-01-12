/** @jsx React.DOM */

/* global module, require, react */
/*jshint eqnull: true*/

'use strict';

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

var dragManager = module.exports.DragManager = (function() {
	
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
			}
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
			}
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
							var elementOwnIndicator = indicator.component.props.axetype === _dragElement.props.axetype &&
													  indicator.component.props.position === _dragElement.props.position;

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
		var buttons = this.props.data.map(function(button, index) {			
			if(index < self.props.data.length - 1) {
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

		return <div className={'drop-target' + (this.state.isover ? ' drop-target-drag-over' : '')}>
				{buttons}
			   </div>;
	}
});

function getOffset(element) {
	if(element != null) {
	    var rect = element.getBoundingClientRect();
	    return { x: rect.left + 0, y: rect.top + 0 };
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
	onDragOver: function(component) {
		this.setState({
			isover: true,
			width: component.getDOMNode().style.width
		});
	},
	onDragEnd: function() {
		this.setState({
			isover: false,
			width: null
		});
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

		return <div style={style} className={classname}></div>;
	}
});

var pbid = 0;

module.exports.PivotButton = react.createClass({
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
	onFilterMouseDown: function(e) {
		// left mouse button only
		if (e.button !== 0) return;

		var filterButton = this.getDOMNode().childNodes[0].rows[0].cells[2].childNodes[0];
		var filterButtonPos = getOffset(filterButton);
		var filterContainer = document.createElement('div');

        var filterPanelFactory = React.createFactory(FilterPanel);
        var filterPanel = filterPanelFactory({
            field: this.props.field.name,
            rootComp: this.props.rootComp
        });

        filterContainer.className = 'orb-theme orb filter-container';
        filterContainer.style.top = filterButtonPos.y + 'px';
        filterContainer.style.left = filterButtonPos.x + 'px';
        document.body.appendChild(filterContainer);

        React.render(filterPanel, filterContainer);

		// prevent event bubbling (to prevent text selection while dragging for example)
		e.stopPropagation();
		e.preventDefault();
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
			dragManager.dragElement(null);
			document.removeEventListener('mousemove', this.onMouseMove);
			document.removeEventListener('mouseup', this.onMouseUp);
		} else if (this.state.mousedown) {
			// mouse down, interested by mouse up/move events.
			dragManager.dragElement(this);
			document.addEventListener('mousemove', this.onMouseMove);
			document.addEventListener('mouseup', this.onMouseUp);
		}
	},
	componentWillUnmount : function() {
		document.removeEventListener('mousemove', this.onMouseMove);
		document.removeEventListener('mouseup', this.onMouseUp);
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
			this.props.rootComp.sort(this.props.axetype, this.props.field);
		}

		return true;
	},
	onMouseMove: function (e) {
		// if the mouse is not down while moving, return (no drag)
		if (!this.state.mousedown) return;

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

		dragManager.elementMoved();

		e.stopPropagation();
		e.preventDefault();
	},
	render: function() {
		var self = this;
		var divstyle = {
			left: self.state.pos.x + 'px',
			top: self.state.pos.y + 'px',
			position: self.state.dragging ? 'fixed' : ''
		};

		if(self.state.size) {
			divstyle.width = self.state.size.width + 'px';
		}

		var sortIndicator = self.props.field.sort.order === 'asc' ? 
			' \u2191' :
			(self.props.field.sort.order === 'desc' ?
				' \u2193' :
				'' );

		return <div key={self.props.field.name} 
		            className={'field-button' + (this.props.rootComp.props.config.bootstrap ? ' btn btn-default' : '')}
		            onMouseDown={this.onMouseDown}
		            style={divstyle}>
		            <table>
		            	<tbody>
		            		<tr>
		            			<td style={{padding: 0 }}>{self.props.field.caption}</td>
		            			<td style={{padding: 0, width: 8 }}>{sortIndicator}</td>
		            			<td style={{padding: 0, verticalAlign: 'top' }}>
		            				<div className={self.state.dragging ? '' : 'filter-button'} onMouseDown={self.state.dragging ? null : this.onFilterMouseDown}></div>
		            			</td>
		            		</tr>
		            	</tbody>
		            </table>
		        </div>;
	}
});

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
	componentWillUnmount : function() {
		document.removeEventListener('mousedown', this.onMouseDown);
		window.removeEventListener('resize', this.destroy);
	},
	render: function () {
		var values = this.props.rootComp.props.data.getFieldValues(this.props.field);
		var checkboxes = [];
		for(var i = 0; i < values.length; i++) {
			checkboxes.push(<tr><td className="filter-checkbox"><input type="checkbox" value={values[i]} defaultChecked="checked"/></td><td className="filter-value" title={values[i]}>{values[i]}</td></tr>);
		}
		if(values.containsBlank) {
			checkboxes.push(<tr><td className="filter-checkbox"><input type="checkbox" value="#Blank#" defaultChecked="checked"/></td><td className="filter-value" title={values[i]}>(Blank)</td></tr>);
		}

		var style = {
		/*	position: 'absolute',
			top: 0,
			left: 0*/
		};
		return <div><div className="filter-values-table-container">
					<table className="filter-values-table"><tbody>{checkboxes}</tbody></table>
				</div>
			<div className="filter-confirm-buttons">
				<input type="button" value="Ok" style={{ float: 'right' }}/>
				<input type="button" value="Cancel" style={{ float: 'right' }}/>
			</div></div>;
	}
});