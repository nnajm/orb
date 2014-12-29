/** @jsx React.DOM */

/* global module, require, React */

'use strict';

var react = typeof window === 'undefined' ? require('react') : window.React;
var utils = require('../orb.utils');
var axe = require('../orb.axe');
var uiheaders = require('../orb.ui.header');

var pivotId = 1;
var extraCol = 1;
var comps = module.exports;

module.exports.PivotTable = react.createClass({
    getInitialState: function() {
        comps.DragManager.init(this);
        return {};
    },
    id: pivotId++,
    sort: function(axetype, field) {
        this.props.data.sort(axetype, field);
        this.setProps(this.props);
    },
    moveButton: function(button, newAxeType, position) {
        this.props.data.moveField(button.props.field.name, button.props.axetype, newAxeType, position);
        this.setProps(this.props);
    },
    expandRow: function(cell) {
        cell.expand();
        this.setProps({});
    },
    collapseRow: function(cell) {
        cell.subtotalHeader.collapse();
        this.setProps({});
    },
    render: function() {

        var self = this;

        var ptc = this.props.data;
        var PivotButton = comps.PivotButton;
        var PivotRow = comps.PivotRow;
        var DropTarget = comps.DropTarget;

        var fieldButtons = ptc.pgrid.config.availablefields().map(function(field, index) {
            return React.createElement(PivotButton, {
                key: field.name,
                field: field,
                axetype: null,
                position: index,
                rootComp: self
            });
        });

        var dataButtons = ptc.pgrid.config.dataFields.map(function(field, index) {
            return React.createElement(PivotButton, {
                key: field.name,
                field: field,
                axetype: axe.Type.DATA,
                position: index,
                rootComp: self
            });
        });

        var columnButtons = ptc.pgrid.config.columnFields.map(function(field, index) {
            return React.createElement(PivotButton, {
                key: field.name,
                field: field,
                axetype: axe.Type.COLUMNS,
                position: index,
                rootComp: self
            });
        });

        // get 'row buttons' row (also last row containing column headers)
        var rowButtons = utils.findInArray(ptc.cells, function(row) {
            return row[0].template === 'cell-template-fieldbutton';
        });

        // build row buttons
        if (rowButtons !== undefined) {
            rowButtons = rowButtons.filter(function(buttonCell) {
                return buttonCell.template === 'cell-template-fieldbutton';
            }).map(function(buttonCell, index) {
                return React.createElement(PivotButton, {
                    key: buttonCell.value.name,
                    field: buttonCell.value,
                    axetype: axe.Type.ROWS,
                    position: index,
                    rootComp: self
                });
            });
        } else {
            rowButtons = [];
        }

        // build the cell that will contains 'row buttons'
        var rowButtonsCell = React.createElement("td", {
                className: "empty",
                colSpan: ptc.rowHeadersWidth + extraCol,
                rowSpan: "1"
            },
            React.createElement(DropTarget, {
                data: rowButtons,
                axetype: axe.Type.ROWS
            })
        );

        var rows = ptc.cells.map(function(row, index) {
            if (index == ptc.columnHeadersHeight - 1) {
                return React.createElement(PivotRow, {
                    key: index,
                    row: row,
                    rowButtonsCount: ptc.rowHeadersWidth,
                    rowButtonsCell: rowButtonsCell,
                    rootComp: self
                });
            } else {
                return React.createElement(PivotRow, {
                    key: index,
                    row: row,
                    rootComp: self
                });
            }
        });

        var tblStyle = {};
        if (this.props.config.width) {
            tblStyle.width = this.props.config.width;
        }
        if (this.props.config.height) {
            tblStyle.height = this.props.config.height;
        }

        return (
            React.createElement("div", {
                    className: "orb-container",
                    style: tblStyle
                },
                React.createElement("table", {
                        id: "{'tbl' + self.id}",
                        className: "orb",
                        style: {
                            width: '100%'
                        }
                    },
                    React.createElement("tbody", null,
                        React.createElement("tr", null,
                            React.createElement("td", {
                                    className: "available-fields field-group",
                                    colSpan: extraCol,
                                    rowSpan: "1"
                                },
                                React.createElement("div", {
                                    className: "field-group-caption"
                                }, "Fields")
                            ),
                            React.createElement("td", {
                                    className: "available-fields",
                                    colSpan: ptc.totalWidth,
                                    rowSpan: "1"
                                },
                                React.createElement(DropTarget, {
                                    data: fieldButtons,
                                    axetype: null
                                })
                            )
                        ),
                        React.createElement("tr", null,
                            React.createElement("td", {
                                    className: "field-group",
                                    colSpan: extraCol,
                                    rowSpan: "1"
                                },
                                React.createElement("div", {
                                    className: "field-group-caption"
                                }, "Data")
                            ),
                            React.createElement("td", {
                                    className: "empty",
                                    colSpan: ptc.totalWidth,
                                    rowSpan: "1"
                                },
                                React.createElement(DropTarget, {
                                    data: dataButtons,
                                    axetype: axe.Type.DATA
                                })
                            )
                        ),
                        React.createElement("tr", null,
                            React.createElement("td", {
                                className: "empty",
                                colSpan: ptc.rowHeadersWidth + extraCol,
                                rowSpan: "1"
                            }),
                            React.createElement("td", {
                                    className: "empty",
                                    colSpan: ptc.columnHeadersWidth,
                                    rowSpan: "1"
                                },
                                React.createElement(DropTarget, {
                                    data: columnButtons,
                                    axetype: axe.Type.COLUMNS
                                })
                            )
                        ),
                        rows
                    )
                ),
                React.createElement("div", {
                    className: "orb-overlay orb-overlay-hidden",
                    id: 'drilldialog' + self.id
                })
            )
        );
    }
});

module.exports.PivotRow = react.createClass({
    render: function() {
        var self = this;
        var PivotCell = comps.PivotCell;

        var lastCellIndex = this.props.row.length - 1;
        var cell0 = this.props.row[0];
        var cells;

        var rowstyle = {};

        if (this.props.rowButtonsCell !== undefined) {
            cells = this.props.row.slice(this.props.rowButtonsCount).map(function(cell, index) {
                var isrightmost = index === (lastCellIndex - self.props.rowButtonsCount);
                return React.createElement(PivotCell, {
                    key: index,
                    cell: cell,
                    rightmost: isrightmost,
                    leftmost: false,
                    rootComp: self.props.rootComp
                });
            });

            return (
                React.createElement("tr", null,
                    this.props.rowButtonsCell,
                    cells
                )
            );

        } else {

            if (cell0.template == 'cell-template-row-header' && cell0.visible && !cell0.visible()) {
                rowstyle.display = 'none';
            }

            cells = this.props.row.map(function(cell, index) {
                var isrightmost = index === lastCellIndex;
                var isleftmost = index === 0 && (
                    cell.type === uiheaders.HeaderType.EMPTY ||
                    (cell.type === uiheaders.HeaderType.SUB_TOTAL && cell.dim.parent.isRoot) ||
                    cell.type === uiheaders.HeaderType.GRAND_TOTAL ||
                    (cell.dim && (cell.dim.isRoot || cell.dim.parent.isRoot))
                );

                return React.createElement(PivotCell, {
                    key: index,
                    cell: cell,
                    rightmost: isrightmost,
                    leftmost: isleftmost,
                    rootComp: self.props.rootComp
                });
            });

            return (
                React.createElement("tr", {
                        style: rowstyle
                    },
                    cells
                )
            );
        }
    }
});

module.exports.PivotCell = react.createClass({
    expand: function() {
        this.props.rootComp.expandRow(this.props.cell);
    },
    collapse: function() {
        this.props.rootComp.collapseRow(this.props.cell);
    },
    render: function() {
        var self = this;
        var cell = this.props.cell;
        var divcontent = [];
        var value;
        var vArrow = '\u25bc';
        var hArrow = '\u25b6';
        var cellClick;

        switch (cell.template) {
            case 'cell-template-row-header':
            case 'cell-template-column-header':
                if (cell.type === uiheaders.HeaderType.WRAPPER && cell.dim.field.subTotal.visible && cell.dim.field.subTotal.collapsible && cell.subtotalHeader.expanded) {
                    divcontent.push(React.createElement("span", {
                        key: "toggle-button",
                        className: "toggle-button toggle-button-down",
                        onClick: this.collapse
                    }));
                } else if (cell.type === uiheaders.HeaderType.SUB_TOTAL && !cell.expanded) {
                    divcontent.push(React.createElement("span", {
                        key: "toggle-button",
                        className: "toggle-button toggle-button-right",
                        onClick: this.expand
                    }));
                }
                value = cell.value;
                break;
            case 'cell-template-dataheader':
                value = cell.value.caption;
                break;
            case 'cell-template-datavalue':
                value = (cell.datafield && cell.datafield.formatFunc) ? cell.datafield.formatFunc()(cell.value) : cell.value;
                cellClick = function() {
                    self.props.rootComp.props.data.drilldown(cell, self.props.rootComp.id);
                }
                break;
            default:
                break;
        }

        divcontent.push(React.createElement("span", {
            key: "cell-value",
            style: {
                whiteSpace: 'nowrap'
            }
        }, value));

        var classname = cell.cssclass;
        var isHidden = !cell.visible();
        if (isHidden || this.props.rightmost || this.props.leftmost) {

            if (isHidden) {
                classname += ' cell-hidden';
            }

            if (this.props.rightmost && (cell.axetype !== axe.Type.COLUMNS || cell.type === uiheaders.HeaderType.GRAND_TOTAL)) {
                classname += ' cell-rightmost';
            }

            if (this.props.leftmost) {
                classname += ' cell-leftmost';
            }
        }

        if (cell.template === 'cell-template-column-header' || cell.template === 'cell-template-dataheader') {
            classname += ' centered';
        }

        return React.createElement("td", {
                className: classname,
                onDoubleClick: cellClick,
                colSpan: cell.hspan() + (this.props.leftmost ? extraCol : 0),
                rowSpan: cell.vspan()
            },
            React.createElement("div", null,
                divcontent
            )
        );
    }
});

module.exports.Grid = react.createClass({
    render: function() {
        var data = this.props.data;
        var headers = this.props.headers;

        var rows = [];

        if (headers && headers.length > 0) {
            var headerRow = [];
            for (var h = 0; h < headers.length; h++) {
                headerRow.push(React.createElement("th", null, headers[h]));
            }
            rows.push(React.createElement("tr", null, headerRow));
        }

        if (data && data.length > 0) {
            for (var i = 0; i < data.length; i++) {
                var row = [];
                for (var j = 0; j < data.length; j++) {
                    row.push(React.createElement("td", null, data[i][j]));
                }
                rows.push(React.createElement("tr", null, row));
            }
        }

        return React.createElement("table", null,
            React.createElement("tbody", null,
                rows
            )
        );
    }
});

module.exports.Dialog = react.createClass({
    overlayElement: null,
    componentDidMount: function() {
        this.overlayElement = document.getElementById('drilldialog' + this.props.pivotId);
        this.overlayElement.className = 'orb-overlay orb-overlay-visible';
        this.overlayElement.addEventListener('click', this.close);

        var dialogElement = this.overlayElement.children[0];

        var screenWidth = Math.max(document.documentElement.clientWidth, window.innerWidth || 0)
        var screenHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0)

        dialogElement.style.top = (screenHeight > dialogElement.offsetHeight ? (screenHeight - dialogElement.offsetHeight) / 2 : 0) + 'px';
        dialogElement.style.left = (screenWidth > dialogElement.offsetWidth ? (screenWidth - dialogElement.offsetWidth) / 2 : 0) + 'px';
    },
    close: function() {
        if (this.overlayElement) {
            this.overlayElement.removeEventListener('click', this.close);
            React.unmountComponentAtNode(this.overlayElement);
            this.overlayElement.className = 'orb-overlay orb-overlay-hidden';
        }
    },
    render: function() {
        var Grid = comps.Grid;
        return React.createElement("div", {
                className: "orb-dialog"
            },
            React.createElement("div", {
                    className: "orb-dialog-body"
                },
                React.createElement(Grid, {
                    headers: this.props.headers,
                    data: this.props.data
                })
            )
        );
    }
});
/** @jsx React.DOM */

/* global module, require, react */
/*jshint eqnull: true*/

'use strict';

function forEach(list, func, defStop) {
    var ret;
    if (list != null) {
        for (var i = 0, l = list.length; i < l; i++) {
            ret = func(list[i], i);
            if (ret !== undefined && defStop === true) {
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
        if (target.onDragOver) {
            target.onDragOver(_dragElement);
            return true;
        }
        return false;
    }

    function signalDragEnd(target) {
        if (target.onDragEnd) {
            target.onDragEnd();
            return true;
        }
        return false;
    }

    function getDropTarget() {
        return forEach(_dropTargets, function(target) {
            if (target.component.state.isover) {
                return target;
            }
        }, true);
    }

    function getDropIndicator() {
        return forEach(_dropIndicators, function(indicator) {
            if (indicator.component.state.isover) {
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
            if (_dragElement != prevDragElement) {
                if (elem == null) {

                    // Drop Target
                    var dropTarget = getDropTarget();
                    // Drop Indicator
                    var dropIndicator = getDropIndicator();

                    if (dropTarget) {
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
            for (var i = 0; i < _dropTargets.length; i++) {
                if (_dropTargets[i].component == target) {
                    tindex = i;
                    break;
                }
            }
            if (tindex != null) {
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
            for (var i = 0; i < _dropIndicators.length; i++) {
                if (_dropIndicators[i].component == indicator) {
                    iindex = i;
                    break;
                }
            }
            if (iindex != null) {
                _dropIndicators.splice(iindex, 1);
            }
        },
        elementMoved: function() {
            if (_dragElement != null) {
                var dragNodeRect = _dragNode.getBoundingClientRect();
                var foundTarget;

                forEach(_dropTargets, function(target) {
                    if (!foundTarget) {
                        var tnodeRect = target.component.getDOMNode().getBoundingClientRect();
                        var isOverlap = doElementsOverlap(dragNodeRect, tnodeRect);
                        if (isOverlap && signalDragOver(target)) {
                            foundTarget = target;
                            return true;
                        } else {
                            signalDragEnd(target);
                        }
                    }
                }, true);

                var foundIndicator;

                if (foundTarget) {
                    forEach(_dropIndicators, function(indicator, index) {
                        if (!foundIndicator) {
                            var elementOwnIndicator = indicator.component.props.axetype === _dragElement.props.axetype &&
                                indicator.component.props.position === _dragElement.props.position;

                            var targetIndicator = indicator.component.props.axetype === foundTarget.component.props.axetype;
                            if (targetIndicator && !elementOwnIndicator) {
                                var tnodeRect = indicator.component.getDOMNode().getBoundingClientRect();
                                var isOverlap = doElementsOverlap(dragNodeRect, tnodeRect);
                                if (isOverlap && signalDragOver(indicator)) {
                                    foundIndicator = indicator;
                                    return;
                                }
                            }
                        }

                        signalDragEnd(indicator);
                    });

                    if (!foundIndicator) {
                        var axeIndicators = _dropIndicators.filter(function(indicator) {
                            return indicator.component.props.axetype === foundTarget.component.props.axetype;
                        });
                        if (axeIndicators.length > 0) {
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
    getInitialState: function() {
        this.dtid = ++dtid;
        // initial state, all zero.
        dragManager.registerTarget(this, this.props.axetype, this.onDragOver, this.onDragEnd);
        return {
            isover: false
        };
    },
    componentWillUnmount: function() {
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
            if (index < self.props.data.length - 1) {
                return [
                    React.createElement(DropIndicator, {
                        isFirst: index === 0,
                        position: index,
                        axetype: self.props.axetype
                    }),
                    button
                ];
            } else {
                return [
                    React.createElement(DropIndicator, {
                        isFirst: index === 0,
                        position: index,
                        axetype: self.props.axetype
                    }),
                    button,
                    React.createElement(DropIndicator, {
                        isLast: true,
                        position: null,
                        axetype: self.props.axetype
                    })
                ];
            }
        });

        return React.createElement("div", {
                className: 'drop-target' + (this.state.isover ? ' drag-over' : '')
            },
            buttons
        );
    }
});

function getOffset(element) {
    if (element != null) {
        var rect = element.getBoundingClientRect();
        return {
            x: rect.left + 0,
            y: rect.top + 0
        };
    }
    return {
        x: 0,
        y: 0
    };
}

function getSize(element) {
    if (element != null) {
        var rect = element.getBoundingClientRect();
        return {
            width: rect.right - rect.left,
            height: rect.bottom - rect.top
        };
    }
    return {
        x: 0,
        y: 0
    };
}

module.exports.DropIndicator = react.createClass({
    displayName: 'DropIndicator',
    getInitialState: function() {
        dragManager.registerIndicator(this, this.props.axetype, this.props.position, this.onDragOver, this.onDragEnd);
        return {
            isover: false
        };
    },
    componentWillUnmount: function() {
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

        if (this.props.isFirst) {
            classname += ' drop-indicator-first';
        }

        if (this.props.isLast) {
            classname += ' drop-indicator-last';
        }

        var style = {};
        if (this.state.isover) {
            classname += ' drop-indicator-drag-over';
        }

        return React.createElement("div", {
            style: style,
            className: classname
        });
    }
});

var pbid = 0;

module.exports.PivotButton = react.createClass({
    displayName: 'PivotButton',
    getInitialState: function() {
        this.pbid = ++pbid;

        // initial state, all zero.
        return {
            pos: {
                x: 0,
                y: 0
            },
            startpos: {
                x: 0,
                y: 0
            },
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
    componentDidUpdate: function() {
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
    componentWillUnmount: function() {
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
        if (!wasdragging) {
            this.props.rootComp.sort(this.props.axetype, this.props.field);
        }
    },
    onMouseMove: function(e) {
        // if the mouse is not down while moving, return (no drag)
        if (!this.state.mousedown) return;

        var size = null;
        if (!this.state.dragging) {
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

        if (self.state.size) {
            divstyle.width = self.state.size.width + 'px';
        }

        var DropIndicator = module.exports.DropIndicator;
        var sortIndicator = self.props.field.sort.order === 'asc' ?
            ' \u2191' :
            (self.props.field.sort.order === 'desc' ?
                ' \u2193' :
                '');

        return React.createElement("div", {
                key: self.props.field.name,
                className: "field-button",
                onMouseDown: this.onMouseDown,
                style: divstyle
            },
            self.props.field.caption,
            React.createElement("span", null, sortIndicator)
        );
    }
});
