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

orb.react.DropTarget = React.createClass({
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

		return <div className={'drop-target' + (this.state.isover ? ' drag-over' : '')}>
				{buttons}
			   </div>;
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

		return <div style={style} className={classname}></div>;
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

		return <div key={self.props.field.name} 
		            className='field-button'
		            onMouseDown={this.onMouseDown}
		            style={divstyle}>
		            	{self.props.field.caption}
		            	<span>{sortIndicator}</span>
		        </div>;
	}
});

})();