/* global module, require, react */
/*jshint eqnull: true*/

'use strict';

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
		return reactUtils.forEach(_dropTargets, function(target) {
			if(target.component.state.isover) {
				return target;
			}
		}, true);
	}

	function getDropIndicator() {
		return reactUtils.forEach(_dropIndicators, function(indicator) {
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
					reactUtils.forEach(_dropTargets, function(target) {
						signalDragEnd(target);
					});

					reactUtils.forEach(_dropIndicators, function(indicator) {
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

				reactUtils.forEach(_dropTargets, function(target) {
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
					reactUtils.forEach(_dropIndicators, function(indicator, index) {
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
					reactUtils.forEach(_dropIndicators, function(indicator, index) {
						signalDragEnd(indicator);
					});
				}
			}
		}
	};
}());