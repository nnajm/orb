/** @jsx React.DOM */

/* global module, require, React */

'use strict';

var react = typeof window === 'undefined' ? require('react') : window.React;
var utils = require('../orb.utils');
var axe = require('../orb.axe');
var uiheaders = require('../orb.ui.header');
var filtering = require('../orb.filtering');
var reactUtils = require('./orb.react.utils');

var extraCol = 1;
var comps = module.exports;

/** @jsx React.DOM */

/* global module, require, React */

'use strict';

var pivotId = 1;
var themeChangeCallbacks = {};

module.exports.PivotTable = react.createClass({
    id: pivotId++,
    pgrid: null,
    pgridwidget: null,
    getInitialState: function() {
        comps.DragManager.init(this);

        themeChangeCallbacks[this.id] = [];
        this.registerThemeChanged(this.updateClasses);

        this.pgridwidget = this.props.pgridwidget;
        this.pgrid = this.pgridwidget.pgrid;
        return {};
    },
    sort: function(axetype, field) {
        this.pgridwidget.sort(axetype, field);
        this.setProps({});
    },
    moveButton: function(button, newAxeType, position) {
        this.pgridwidget.moveField(button.props.field.name, button.props.axetype, newAxeType, position);
        this.setProps({});
    },
    expandRow: function(cell) {
        cell.expand();
        this.setProps({});
    },
    collapseRow: function(cell) {
        cell.subtotalHeader.collapse();
        this.setProps({});
    },
    applyFilter: function(fieldname, operator, term, staticValue, excludeStatic) {
        this.pgridwidget.applyFilter(fieldname, operator, term, staticValue, excludeStatic);
        this.setProps({});
    },
    registerThemeChanged: function(compCallback) {
        if (compCallback) {
            themeChangeCallbacks[this.id].push(compCallback);
        }
    },
    unregisterThemeChanged: function(compCallback) {
        var i;
        if (compCallback && (i = themeChangeCallbacks[this.id].indexOf(compCallback) >= 0)) {
            themeChangeCallbacks[this.id].splice(i, 1);
        }
    },
    changeTheme: function(newTheme) {
        if (this.pgridwidget.pgrid.config.setTheme(newTheme)) {
            // notify self/sub-components of the theme change
            for (var i = 0; i < themeChangeCallbacks[this.id].length; i++) {
                themeChangeCallbacks[this.id][i]();
            }
        }
    },
    updateClasses: function() {
        var thisnode = this.getDOMNode();
        var classes = this.pgridwidget.pgrid.config.theme.getPivotClasses();
        thisnode.className = classes.container;
        thisnode.children[0].className = classes.table;
    },
    render: function() {

        var self = this;

        var config = this.pgridwidget.pgrid.config;
        var PivotButton = comps.PivotButton;
        var PivotRow = comps.PivotRow;
        var DropTarget = comps.DropTarget;
        var Toolbar = comps.Toolbar;

        var fieldButtons = config.availablefields().map(function(field, index) {
            return React.createElement(PivotButton, {
                key: field.name,
                field: field,
                axetype: null,
                position: index,
                pivotTableComp: self
            });
        });

        var dataButtons = config.dataFields.map(function(field, index) {
            return React.createElement(PivotButton, {
                key: field.name,
                field: field,
                axetype: axe.Type.DATA,
                position: index,
                pivotTableComp: self
            });
        });

        var columnButtons = config.columnFields.map(function(field, index) {
            return React.createElement(PivotButton, {
                key: field.name,
                field: field,
                axetype: axe.Type.COLUMNS,
                position: index,
                pivotTableComp: self
            });
        });

        // get 'row buttons' row (also last row containing column headers)
        var rowButtons = utils.findInArray(this.pgridwidget.cells, function(row) {
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
                    pivotTableComp: self
                });
            });
        } else {
            rowButtons = [];
        }

        // build the cell that will contains 'row buttons'
        var rowButtonsCell = React.createElement("td", {
                className: "empty",
                colSpan: this.pgridwidget.layout.rowHeaders.width + extraCol,
                rowSpan: "1"
            },
            React.createElement(DropTarget, {
                buttons: rowButtons,
                axetype: axe.Type.ROWS
            })
        );

        var rows = this.pgridwidget.cells.map(function(row, index) {
            if (index == self.pgridwidget.layout.columnHeaders.height - 1) {
                return React.createElement(PivotRow, {
                    key: index,
                    row: row,
                    topmost: index === 0,
                    rowButtonsCount: self.pgridwidget.layout.rowHeaders.width,
                    rowButtonsCell: rowButtonsCell,
                    pivotTableComp: self
                });
            } else {
                return React.createElement(PivotRow, {
                    key: index,
                    topmost: index === 0,
                    row: row,
                    pivotTableComp: self
                });
            }
        });

        var classes = config.theme.getPivotClasses();

        var tblStyle = {};
        if (config.width) {
            tblStyle.width = config.width;
        }
        if (config.height) {
            tblStyle.height = config.height;
        }

        return (
            React.createElement("div", {
                    className: classes.container,
                    style: tblStyle
                },
                React.createElement("table", {
                        id: "{'tbl' + self.id}",
                        className: classes.table,
                        style: {
                            width: '100%'
                        }
                    },
                    React.createElement("tbody", null,
                        React.createElement("tr", null,
                            React.createElement("td", {
                                    className: "orb-toolbar",
                                    colSpan: this.pgridwidget.layout.pivotTable.width + extraCol
                                },
                                React.createElement(Toolbar, {
                                    pivotTableComp: self
                                })
                            )
                        ),
                        React.createElement("tr", null,
                            React.createElement("td", {
                                    className: "flds-grp-cap av-flds text-muted",
                                    colSpan: extraCol,
                                    rowSpan: "1"
                                },
                                React.createElement("div", null, "Fields")
                            ),
                            React.createElement("td", {
                                    className: "av-flds",
                                    colSpan: this.pgridwidget.layout.pivotTable.width,
                                    rowSpan: "1"
                                },
                                React.createElement(DropTarget, {
                                    buttons: fieldButtons,
                                    axetype: null
                                })
                            )
                        ),
                        React.createElement("tr", null,
                            React.createElement("td", {
                                    className: "flds-grp-cap text-muted",
                                    colSpan: extraCol,
                                    rowSpan: "1"
                                },
                                React.createElement("div", null, "Data")
                            ),
                            React.createElement("td", {
                                    className: "empty",
                                    colSpan: this.pgridwidget.layout.pivotTable.width,
                                    rowSpan: "1"
                                },
                                React.createElement(DropTarget, {
                                    buttons: dataButtons,
                                    axetype: axe.Type.DATA
                                })
                            )
                        ),
                        React.createElement("tr", null,
                            React.createElement("td", {
                                className: "empty",
                                colSpan: this.pgridwidget.layout.rowHeaders.width + extraCol,
                                rowSpan: "1"
                            }),
                            React.createElement("td", {
                                    className: "empty",
                                    colSpan: this.pgridwidget.layout.columnHeaders.width,
                                    rowSpan: "1"
                                },
                                React.createElement(DropTarget, {
                                    buttons: columnButtons,
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
/** @jsx React.DOM */

/* global module, require, React */

'use strict';


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
                var isleftmostHeader = index === 0;
                return React.createElement(PivotCell, {
                    key: index,
                    cell: cell,
                    topmost: self.props.topmost,
                    rightmost: isrightmost,
                    leftmostheader: isleftmostHeader,
                    pivotTableComp: self.props.pivotTableComp
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
                    (cell.type === uiheaders.HeaderType.GRAND_TOTAL) ||
                    (cell.dim && (cell.dim.isRoot || cell.dim.parent.isRoot))
                );
                var isleftmostHeader = cell.template === 'cell-template-column-header' && index === 1;
                var isleftmostDataValue = cell.template === 'cell-template-datavalue' && cell.visible() && (self.props.row[index - 1].template !== 'cell-template-datavalue' || !self.props.row[index - 1].visible());

                return React.createElement(PivotCell, {
                    key: index,
                    cell: cell,
                    topmost: self.props.topmost,
                    leftmostheader: isleftmostHeader,
                    leftmostdatavalue: isleftmostDataValue,
                    rightmost: isrightmost,
                    leftmost: isleftmost,
                    pivotTableComp: self.props.pivotTableComp
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
/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.PivotCell = react.createClass({
    expand: function() {
        this.props.pivotTableComp.expandRow(this.props.cell);
    },
    collapse: function() {
        this.props.pivotTableComp.collapseRow(this.props.cell);
    },
    render: function() {
        var self = this;
        var cell = this.props.cell;
        var divcontent = [];
        var value;
        var cellClick;
        var headerPushed = false;

        switch (cell.template) {
            case 'cell-template-row-header':
            case 'cell-template-column-header':
                var isWrapper = cell.type === uiheaders.HeaderType.WRAPPER && cell.dim.field.subTotal.visible && cell.dim.field.subTotal.collapsible && cell.subtotalHeader.expanded;
                var isSubtotal = cell.type === uiheaders.HeaderType.SUB_TOTAL && !cell.expanded;
                if (isWrapper || isSubtotal) {
                    headerPushed = true;

                    divcontent.push(React.createElement("table", {
                            key: "header-value"
                        },
                        React.createElement("tbody", null,
                            React.createElement("tr", null, React.createElement("td", {
                                    className: "tgl-btn"
                                }, React.createElement("div", {
                                    className: 'tgl-btn-' + (isWrapper ? 'down' : 'right'),
                                    onClick: (isWrapper ? this.collapse : this.expand)
                                })),
                                React.createElement("td", {
                                    className: "hdr-val"
                                }, React.createElement("div", null, cell.value)))
                        )));
                }
                value = cell.value;
                break;
            case 'cell-template-dataheader':
                value = cell.value.caption;
                break;
            case 'cell-template-datavalue':
                value = (cell.datafield && cell.datafield.formatFunc) ? cell.datafield.formatFunc()(cell.value) : cell.value;
                cellClick = function() {
                    self.props.pivotTableComp.pgridwidget.drilldown(cell, self.props.pivotTableComp.id);
                }
                break;
            default:
                break;
        }

        if (!headerPushed) {
            divcontent.push(React.createElement("div", {
                key: "cell-value",
                className: cell.template !== 'cell-template-datavalue' ? 'hdr-val' : ''
            }, React.createElement("div", null, value)));
        }

        return React.createElement("td", {
                className: getClassname(this.props),
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

function getClassname(compProps) {
        var cell = compProps.cell;
        var classname = cell.cssclass;
        var isHidden = !cell.visible();
        var isEmpty = cell.template === 'cell-template-empty';

        if (isHidden) {
            classname += ' cell-hidden';
        }

        if (compProps.leftmostheader || compProps.leftmostdatavalue || (compProps.leftmost && !isEmpty)) {
            classname += ' cell-leftmost';
        }

        if (compProps.topmost && !isEmpty) {
            classname += ' cell-topmost';
        }

        if (compProps.rightmost && (cell.axetype !== axe.Type.COLUMNS || cell.type === uiheaders.HeaderType.GRAND_TOTAL)) {
            classname += ' cell-rightmost';
        }

        if (cell.template === 'cell-template-column-header' || cell.template === 'cell-template-dataheader') {
            classname += ' cntr';
        }

        return classname;
    }
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
        return reactUtils.forEach(_dropTargets, function(target) {
            if (target.component.state.isover) {
                return target;
            }
        }, true);
    }

    function getDropIndicator() {
        return reactUtils.forEach(_dropIndicators, function(indicator) {
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

                reactUtils.forEach(_dropTargets, function(target) {
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
                    reactUtils.forEach(_dropIndicators, function(indicator, index) {
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
                    reactUtils.forEach(_dropIndicators, function(indicator, index) {
                        signalDragEnd(indicator);
                    });
                }
            }
        }
    };
}());
/** @jsx React.DOM */

/* global module, require, react */
/*jshint eqnull: true*/

'use strict';


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
        var classname = 'drp-indic';

        if (this.props.isFirst) {
            classname += ' drp-indic-first';
        }

        if (this.props.isLast) {
            classname += ' drp-indic-last';
        }

        var style = {};
        if (this.state.isover) {
            classname += ' drp-indic-over';
        }

        return React.createElement("div", {
            style: style,
            className: classname
        });
    }
});
/** @jsx React.DOM */

/* global module, require, react */
/*jshint eqnull: true*/

'use strict';

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
        var buttons = this.props.buttons.map(function(button, index) {
            if (index < self.props.buttons.length - 1) {
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
                className: 'drp-trgt' + (this.state.isover ? ' drp-trgt-over' : '')
            },
            buttons
        );
    }
});
/** @jsx React.DOM */

/* global module, require, react */
/*jshint eqnull: true*/

'use strict';

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
    onFilterMouseDown: function(e) {
        // left mouse button only
        if (e.button !== 0) return;

        var filterButton = this.getDOMNode().childNodes[0].rows[0].cells[2].childNodes[0];
        var filterButtonPos = reactUtils.getOffset(filterButton);
        var filterContainer = document.createElement('div');

        var filterPanelFactory = React.createFactory(comps.FilterPanel);
        var filterPanel = filterPanelFactory({
            field: this.props.field.name,
            pivotTableComp: this.props.pivotTableComp
        });

        filterContainer.className = this.props.pivotTableComp.pgrid.config.theme.getFilterClasses().container;
        filterContainer.style.top = filterButtonPos.y + 'px';
        filterContainer.style.left = filterButtonPos.x + 'px';
        document.body.appendChild(filterContainer);

        React.render(filterPanel, filterContainer);

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
    componentDidMount: function() {
        this.props.pivotTableComp.registerThemeChanged(this.updateClasses);
    },
    componentWillUnmount: function() {
        this.props.pivotTableComp.unregisterThemeChanged(this.updateClasses);
        document.removeEventListener('mousemove', this.onMouseMove);
        document.removeEventListener('mouseup', this.onMouseUp);
    },
    onMouseDown: function(e) {
        // drag/sort with left mouse button
        if (e.button !== 0) return;

        var thispos = reactUtils.getOffset(this.getDOMNode());

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
            this.props.pivotTableComp.sort(this.props.axetype, this.props.field);
        }

        return true;
    },
    onMouseMove: function(e) {
        // if the mouse is not down while moving, return (no drag)
        if (!this.state.mousedown) return;

        var size = null;
        if (!this.state.dragging) {
            size = reactUtils.getSize(this.getDOMNode());
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
    updateClasses: function() {
        this.getDOMNode().className = this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().pivotButton;
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

        var sortIndicator = self.props.field.sort.order === 'asc' ?
            ' \u2191' :
            (self.props.field.sort.order === 'desc' ?
                ' \u2193' :
                '');

        var filterClass = (self.state.dragging ? '' : 'fltr-btn') + (this.props.pivotTableComp.pgrid.isFieldFiltered(this.props.field.name) ? ' fltr-btn-active' : '');

        return React.createElement("div", {
                key: self.props.field.name,
                className: this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().pivotButton,
                onMouseDown: this.onMouseDown,
                style: divstyle
            },
            React.createElement("table", null,
                React.createElement("tbody", null,
                    React.createElement("tr", null,
                        React.createElement("td", {
                            style: {
                                padding: 0
                            }
                        }, self.props.field.caption),
                        React.createElement("td", {
                            style: {
                                padding: 0,
                                width: 13
                            }
                        }, sortIndicator),
                        React.createElement("td", {
                                style: {
                                    padding: 0,
                                    verticalAlign: 'top'
                                }
                            },
                            React.createElement("div", {
                                className: filterClass,
                                onMouseDown: self.state.dragging ? null : this.onFilterMouseDown
                            })
                        )
                    )
                )
            )
        );
    }
});
/** @jsx React.DOM */

/* global module, react, React */
/*jshint eqnull: true*/

'use strict';

module.exports.FilterPanel = react.createClass({
    pgridwidget: null,
    values: null,
    filterManager: null,
    getInitialState: function() {
        this.pgridwidget = this.props.pivotTableComp.pgridwidget;
        return {};
    },
    destroy: function() {
        var container = this.getDOMNode().parentNode;
        React.unmountComponentAtNode(container);
        container.parentNode.removeChild(container);
    },
    onFilter: function(operator, term, staticValue, excludeStatic) {
        this.props.pivotTableComp.applyFilter(this.props.field, operator, term, staticValue, excludeStatic);
        this.destroy();
    },
    onMouseDown: function(e) {
        var container = this.getDOMNode().parentNode;
        var target = e.target;
        while (target != null) {
            if (target == container) {
                return true;
            }
            target = target.parentNode;
        }

        this.destroy();
    },
    onMouseWheel: function(e) {
        var valuesTable = this.getDOMNode().rows[1].cells[0].children[0];
        var target = e.target;
        while (target != null) {
            if (target == valuesTable) {
                if (valuesTable.scrollHeight <= valuesTable.clientHeight) {
                    e.stopPropagation();
                    e.preventDefault();
                }
                return;
            }
            target = target.parentNode;
        }

        this.destroy();
    },
    componentWillMount: function() {
        document.addEventListener('mousedown', this.onMouseDown);
        document.addEventListener('wheel', this.onMouseWheel);
        window.addEventListener('resize', this.destroy);
    },
    componentDidMount: function() {
        this.filterManager.init(this.getDOMNode());
    },
    componentWillUnmount: function() {
        document.removeEventListener('mousedown', this.onMouseDown);
        document.removeEventListener('wheel', this.onMouseWheel);
        window.removeEventListener('resize', this.destroy);
    },
    render: function() {
        var Dropdown = comps.Dropdown;
        var checkboxes = [];

        this.filterManager = new FilterManager(this, this.pgridwidget.pgrid.getFieldFilter(this.props.field));
        this.values = this.pgridwidget.pgrid.getFieldValues(this.props.field);

        function addCheckboxRow(value, text) {
            return checkboxes.push(React.createElement("tr", {
                    key: value
                },
                React.createElement("td", {
                        className: "fltr-chkbox"
                    },
                    React.createElement("input", {
                        type: "checkbox",
                        value: value,
                        defaultChecked: "checked"
                    })
                ),
                React.createElement("td", {
                    className: "fltr-val",
                    title: text || value
                }, text || value)
            ));
        }

        addCheckboxRow(filtering.ALL, '(Show All)');
        if (this.values.containsBlank) {
            addCheckboxRow(filtering.BLANK, '(Blank)');
        }

        for (var i = 0; i < this.values.length; i++) {
            addCheckboxRow(this.values[i]);
        }

        var buttonClass = this.props.pivotTableComp.pgrid.config.theme.getButtonClasses().orbButton;
        var pivotStyle = window.getComputedStyle(this.props.pivotTableComp.getDOMNode(), null);
        var style = {
            fontFamily: pivotStyle.getPropertyValue('font-family'),
            fontSize: pivotStyle.getPropertyValue('font-size')
        };

        var currentFilter = this.pgridwidget.pgrid.getFieldFilter(this.props.field);

        return React.createElement("table", {
                className: "fltr-scntnr",
                style: style
            },
            React.createElement("tbody", null,
                React.createElement("tr", null,
                    React.createElement("td", {
                            className: "srchop-col"
                        },
                        React.createElement(Dropdown, {
                            values: [
                                filtering.Operators.MATCH.name,
                                filtering.Operators.NOTMATCH.name,
                                filtering.Operators.EQ.name,
                                filtering.Operators.NEQ.name,
                                filtering.Operators.GT.name,
                                filtering.Operators.GTE.name,
                                filtering.Operators.LT.name,
                                filtering.Operators.LTE.name
                            ],
                            selectedValue: currentFilter && currentFilter.operator ? currentFilter.operator.name : filtering.Operators.MATCH.name,
                            onValueChanged: this.filterManager.onOperatorChanged
                        })
                    ),
                    React.createElement("td", {
                        className: "srchtyp-col",
                        title: "Enable/disable Regular expressions"
                    }, ".*"),
                    React.createElement("td", {
                        className: "srchbox-col"
                    }, React.createElement("input", {
                        type: "text",
                        placeholder: "search"
                    }))
                ),
                React.createElement("tr", null,
                    React.createElement("td", {
                            colSpan: "3",
                            className: "fltr-vals-col"
                        },
                        React.createElement("table", {
                                className: "fltr-vals-tbl"
                            },
                            React.createElement("tbody", null,
                                checkboxes
                            )
                        )
                    )
                ),
                React.createElement("tr", {
                        className: "bottom-row"
                    },
                    React.createElement("td", {
                            className: "cnfrm-btn-col",
                            colSpan: "2"
                        },
                        React.createElement("input", {
                            type: "button",
                            className: buttonClass,
                            value: "Ok",
                            style: {
                                float: 'left'
                            }
                        }),
                        React.createElement("input", {
                            type: "button",
                            className: buttonClass,
                            value: "Cancel",
                            style: {
                                float: 'left'
                            }
                        })
                    ),
                    React.createElement("td", {
                            className: "resize-col"
                        },
                        React.createElement("div", null)
                    )
                )
            )
        );
    }
});

function FilterManager(reactComp, initialFilterObject) {

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

    var resizeManager;

    this.init = function(filterContainerElement) {

        elems.filterContainer = filterContainerElement;
        elems.checkboxes = {};
        elems.searchBox = elems.filterContainer.rows[0].cells[2].children[0];
        elems.operatorBox = elems.filterContainer.rows[0].cells[0].children[0];
        elems.okButton = elems.filterContainer.rows[2].cells[0].children[0];
        elems.cancelButton = elems.filterContainer.rows[2].cells[0].children[1];
        elems.resizeGrip = elems.filterContainer.rows[2].cells[1].children[0];

        var rows = elems.filterContainer.rows[1].cells[0].children[0].rows;
        for (var i = 0; i < rows.length; i++) {
            var checkbox = rows[i].cells[0].children[0];
            elems.checkboxes[checkbox.value] = checkbox;
        }

        elems.allCheckbox = elems.checkboxes[filtering.ALL];
        elems.addCheckbox = null;
        elems.enableRegexButton = elems.filterContainer.rows[0].cells[1];

        resizeManager = new ResizeManager(elems.filterContainer.parentNode, elems.filterContainer.rows[1].cells[0].children[0], elems.resizeGrip);

        applyInitialFilterObject();
        addEventListeners();
    };

    this.onOperatorChanged = function(newOperator) {
        if (operator.name !== newOperator) {
            operator = filtering.Operators.get(newOperator);
            self.toggleRegexpButtonVisibility();
            self.searchChanged('operatorChanged');
        }
    };

    function checkboxVisible(checkbox, isVisible) {
        if (isVisible != null) {
            checkbox.parentNode.parentNode.style.display = isVisible ? '' : 'none';
        } else {
            return checkbox.parentNode.parentNode.style.display != 'none';
        }
    }

    function applyInitialFilterObject() {
        if (initialFilterObject) {
            var staticInfos = {
                values: initialFilterObject.staticValue,
                toExclude: initialFilterObject.excludeStatic
            };

            if (initialFilterObject.term) {
                isSearchMode = true;

                operator = initialFilterObject.operator;
                self.toggleRegexpButtonVisibility();

                if (initialFilterObject.regexpMode) {
                    isRegexMode = true;
                    self.toggleRegexpButtonState();
                    lastSearchTerm = initialFilterObject.term.source;
                } else {
                    lastSearchTerm = initialFilterObject.term;
                }

                elems.searchBox.value = lastSearchTerm;

                self.applyFilterTerm(initialFilterObject.operator, initialFilterObject.term);
            } else {
                savedCheckedValues = staticInfos;
            }

            self.updateCheckboxes(staticInfos);
            self.updateAllCheckbox();
        }
    }

    function addEventListeners() {
        self.toggleRegexpButtonVisibility();

        elems.filterContainer.addEventListener('click', self.valueChecked);
        elems.searchBox.addEventListener('keyup', self.searchChanged);

        elems.okButton.addEventListener('click', function() {
            var checkedObj = self.getCheckedValues();
            reactComp.onFilter(operator.name, operator.regexpSupported && isSearchMode && isRegexMode ? new RegExp(lastSearchTerm, 'i') : lastSearchTerm, checkedObj.values, checkedObj.toExclude);
        });
        elems.cancelButton.addEventListener('click', function() {
            reactComp.destroy();
        });
    }

    function ResizeManager(outerContainerElem, valuesTableElem, resizeGripElem) {

        var minContainerWidth = 301;
        var minContainerHeight = 223;

        var mousedownpos = {
            x: 0,
            y: 0
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

            var newContainerWidth = outerContainerWidth + offset.x;
            var newContainerHeight = outerContainerHeight + offset.y;

            mousedownpos.x = e.pageX;
            mousedownpos.y = e.pageY;

            if (newContainerWidth >= minContainerWidth) {
                outerContainerElem.style.width = newContainerWidth + 'px';
            }

            if (newContainerHeight >= minContainerHeight) {
                outerContainerElem.style.height = newContainerHeight + 'px';
                valuesTableElem.style.height = (valuesTableSize.bottom - valuesTableSize.top + offset.y) + 'px';
            }

            e.stopPropagation();
            e.preventDefault();
        };

        resizeGripElem.addEventListener('mousedown', this.resizeMouseDown);
        document.addEventListener('mouseup', this.resizeMouseUp);
        document.addEventListener('mousemove', this.resizeMouseMove);
    }

    this.toggleRegexpButtonVisibility = function() {
        if (operator.regexpSupported) {
            elems.enableRegexButton.addEventListener('click', self.regexpActiveChanged);
            elems.enableRegexButton.className = elems.enableRegexButton.className.replace(/\s+srchtyp\-col\-hidden/, '');

        } else {
            elems.enableRegexButton.removeEventListener('click', self.regexpActiveChanged);
            elems.enableRegexButton.className += ' srchtyp-col-hidden';
        }
    }

    this.toggleRegexpButtonState = function() {
        elems.enableRegexButton.className = elems.enableRegexButton.className.replace('srchtyp-col-active', '');
        if (isRegexMode) {
            elems.enableRegexButton.className += ' srchtyp-col-active';
        }
    }

    this.regexpActiveChanged = function() {
        isRegexMode = !isRegexMode;
        self.toggleRegexpButtonState();
        self.searchChanged('regexModeChanged');
    };

    this.valueChecked = function(e) {
        var target = e.target;
        if (target && target.type && target.type === 'checkbox') {
            if (target == elems.allCheckbox) {
                self.updateCheckboxes({
                    values: elems.allCheckbox.checked
                });
            } else {
                self.updateAllCheckbox();
            }
        }
    };

    this.applyFilterTerm = function(operator, term) {
        var defaultVisible = term ? false : true;
        var opterm = operator.regexpSupported && isSearchMode ? (isRegexMode ? term : utils.escapeRegex(term)) : term;
        checkboxVisible(elems.allCheckbox, defaultVisible);
        for (var i = 0; i < reactComp.values.length; i++) {
            var val = reactComp.values[i];
            var checkbox = elems.checkboxes[val];
            var visible = !isSearchMode || operator.func(val, opterm);
            checkboxVisible(checkbox, visible);
            checkbox.checked = visible;
        }
    }

    this.searchChanged = function(e) {
        var search = (elems.searchBox.value || '').trim();
        if (e === 'operatorChanged' || (e === 'regexModeChanged' && search) || search != lastSearchTerm) {
            lastSearchTerm = search;

            var previousIsSearchMode = isSearchMode;
            isSearchMode = search !== '';

            if (isSearchMode && !previousIsSearchMode) {
                savedCheckedValues = self.getCheckedValues();
            }

            //var searchTerm = operator.regexpSupported && isSearchMode ? new RegExp(isRegexMode ? search : utils.escapeRegex(search), 'i') : search;
            if (e !== 'operatorChanged' || isSearchMode) {
                self.applyFilterTerm(operator, search);
            }

            if (!isSearchMode && previousIsSearchMode) {
                self.updateCheckboxes(savedCheckedValues);
            }

            self.updateAllCheckbox();
        }
    };

    this.getCheckedValues = function() {
        if (!isSearchMode && !elems.allCheckbox.indeterminate) {
            return {
                values: elems.allCheckbox.checked ? filtering.ALL : filtering.NONE,
                toExclude: false
            };
        } else {
            var staticValue;
            var i,
                val,
                checkbox;
            var valuesCount = 0,
                checkedCount = 0;

            for (i = 0; i < reactComp.values.length; i++) {
                val = reactComp.values[i];
                checkbox = elems.checkboxes[val];
                if (checkboxVisible(checkbox)) {
                    valuesCount++;
                    if (checkbox.checked) {
                        checkedCount++;
                    }
                }
            }

            if (checkedCount == 0) {
                staticValue = filtering.NONE;
            } else if (checkedCount == valuesCount) {
                staticValue = filtering.ALL;
            } else {
                staticValue = [];
                var excludeUnchecked = checkedCount > (valuesCount / 2 + 1);

                for (i = 0; i < reactComp.values.length; i++) {
                    val = reactComp.values[i];
                    checkbox = elems.checkboxes[val];
                    if (checkboxVisible(checkbox)) {
                        if ((!excludeUnchecked && checkbox.checked) || (excludeUnchecked && !checkbox.checked)) {
                            staticValue.push(val);
                        }
                    }
                }
            }
            return {
                values: staticValue,
                toExclude: excludeUnchecked
            };
        }
    };

    this.updateCheckboxes = function(checkedList) {
        var values = checkedList ? checkedList.values : null;
        var allchecked = utils.isArray(values) ?
            null :
            (values == null || values === filtering.ALL ?
                true :
                (values === filtering.NONE ?
                    false :
                    !!values
                )
            );
        for (var i = 0; i < reactComp.values.length; i++) {
            var val = reactComp.values[i];
            var checkbox = elems.checkboxes[val];
            if (checkboxVisible(checkbox)) {
                if (allchecked != null) {
                    checkbox.checked = allchecked;
                } else {
                    var valInList = values.indexOf(val) >= 0;
                    checkbox.checked = checkedList.toExclude ? !valInList : valInList;
                }
            }
        }
    };

    this.updateAllCheckbox = function() {
        if (!isSearchMode) {
            var allchecked = null;
            for (var i = 0; i < reactComp.values.length; i++) {
                var checkbox = elems.checkboxes[reactComp.values[i]];
                if (allchecked == null) {
                    allchecked = checkbox.checked;
                } else {
                    if (allchecked !== checkbox.checked) {
                        allchecked = INDETERMINATE;
                        break;
                    }
                }
            }

            if (allchecked === INDETERMINATE) {
                elems.allCheckbox.indeterminate = true;
                elems.allCheckbox.checked = false;
            } else {
                elems.allCheckbox.indeterminate = false;
                elems.allCheckbox.checked = allchecked;
            }
        }
    };
}

/** @jsx React.DOM */

/* global module, react, React */
/*jshint eqnull: true*/

'use strict';

module.exports.Dropdown = react.createClass({
    openOrClose: function(e) {
        var valueNode = this.refs.valueElement.getDOMNode();
        var valuesListNode = this.refs.valuesList.getDOMNode();
        if (e.target === valueNode && valuesListNode.style.display === 'none') {
            valuesListNode.style.display = 'block';
        } else {
            valuesListNode.style.display = 'none';
        }
        e.stopPropagation();
        e.preventDefault();
    },
    componentDidMount: function() {
        document.addEventListener('click', this.openOrClose);
    },
    componentWillUnmount: function() {
        document.removeEventListener('click', this.openOrClose);
    },
    selectValue: function(e) {
        var listNode = this.refs.valuesList.getDOMNode();
        var target = e.target;
        var isli = false;
        while (!isli && target != null) {
            if (target.parentNode == listNode) {
                isli = true;
                break;
            }
            target = target.parentNode;
        }

        if (isli) {
            var value = target.textContent;
            var valueElement = this.refs.valueElement.getDOMNode();
            if (valueElement.textContent != value) {
                valueElement.textContent = value;
                if (this.props.onValueChanged) {
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
        for (var i = 0; i < this.props.values.length; i++) {
            values.push(React.createElement("li", {
                dangerouslySetInnerHTML: {
                    __html: this.props.values[i]
                }
            }))
        }

        return React.createElement("div", {
                className: "orb-select"
            },
            React.createElement("div", {
                ref: "valueElement",
                dangerouslySetInnerHTML: {
                    __html: this.props.selectedValue
                }
            }),
            React.createElement("ul", {
                    ref: "valuesList",
                    style: {
                        display: 'none'
                    },
                    onClick: this.selectValue
                },
                values
            )
        );
    }
});
/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.Grid = react.createClass({
    render: function() {
        var data = this.props.data;
        var headers = this.props.headers;
        var tableClasses = this.props.theme.getGridClasses();

        var rows = [];

        if (headers && headers.length > 0) {
            var headerRow = [];
            for (var h = 0; h < headers.length; h++) {
                headerRow.push(React.createElement("th", {
                    key: 'h' + h
                }, headers[h]));
            }
            rows.push(React.createElement("tr", {
                key: 'h'
            }, headerRow));
        }

        if (data && data.length > 0) {
            for (var i = 0; i < data.length; i++) {
                var row = [];
                for (var j = 0; j < data[i].length; j++) {
                    row.push(React.createElement("td", {
                        key: i + '' + j
                    }, data[i][j]));
                }
                rows.push(React.createElement("tr", {
                    key: i
                }, row));
            }
        }

        return React.createElement("table", {
                className: tableClasses.table
            },
            React.createElement("tbody", null,
                rows
            )
        );
    }
});
/** @jsx React.DOM */

/* global module, require, React */

'use strict';

function createOverlay() {
    var overlayElement = document.createElement('div');
    overlayElement.className = 'orb-overlay orb-overlay-hidden';
    document.body.appendChild(overlayElement);
    return overlayElement;
}

var Dialog = module.exports.Dialog = react.createClass({
    statics: {
        create: function() {
            var dialogFactory = React.createFactory(Dialog);
            var overlay = createOverlay();

            return {
                show: function(props) {
                    React.render(dialogFactory(props), overlay);
                }
            }
        }
    },
    overlayElement: null,
    setOverlayClass: function(visible) {
        this.overlayElement.className = this.props.theme.getDialogClasses(visible).overlay;
    },
    componentDidMount: function() {
        this.overlayElement = this.getDOMNode().parentNode;
        this.setOverlayClass(true);
        this.overlayElement.addEventListener('click', this.close);

        var dialogElement = this.overlayElement.children[0];
        var dialogBodyElement = dialogElement.children[0].children[1];

        var screenWidth = Math.max(document.documentElement.clientWidth, window.innerWidth || 0)
        var screenHeight = Math.max(document.documentElement.clientHeight, window.innerHeight || 0)
        var maxHeight = 2 * screenHeight / 3;
        maxHeight = maxHeight < 301 ? 301 : maxHeight;
        var dWidth = dialogElement.offsetWidth + (dialogElement.offsetHeight > maxHeight ? 11 : 0);
        var dHeight = dialogElement.offsetHeight > maxHeight ? maxHeight : dialogElement.offsetHeight;

        dialogElement.style.top = (screenHeight > dHeight ? (screenHeight - dHeight) / 2 : 0) + 'px';
        dialogElement.style.left = (screenWidth > dWidth ? (screenWidth - dWidth) / 2 : 0) + 'px';
        dialogElement.style.height = dHeight + 'px';
        dialogBodyElement.style.width = dWidth + 'px';
        dialogBodyElement.style.height = (dHeight - 45) + 'px';
    },
    close: function(e) {
        if (e.target == this.overlayElement || e.target.className === 'button-close') {
            this.overlayElement.removeEventListener('click', this.close);
            React.unmountComponentAtNode(this.overlayElement);
            this.setOverlayClass(false);
        }
    },
    render: function() {
        if (this.props.comp) {
            var comp = React.createElement(this.props.comp.type, this.props.comp.props);
            var classes = this.props.theme.getDialogClasses();

            return React.createElement("div", {
                    className: classes.dialog,
                    style: this.props.style || {}
                },
                React.createElement("div", {
                        className: classes.content
                    },
                    React.createElement("div", {
                        className: classes.header
                    }, React.createElement("div", {
                        className: "button-close",
                        onClick: this.close
                    }), React.createElement("div", {
                        className: classes.title
                    }, this.props.title)),
                    React.createElement("div", {
                            className: classes.body
                        },
                        comp
                    )
                )
            );
        }
    }
});
/** @jsx React.DOM */

/* global module, require, React */

'use strict';

module.exports.Toolbar = react.createClass({
    onThemeChanged: function(newTheme) {
        this.props.pivotTableComp.changeTheme(newTheme);
    },
    render: function() {

        var Dropdown = comps.Dropdown;

        var themeColors = require('../orb.themes').themes;
        var values = [];
        for (var color in themeColors) {
            values.push('<div style="float: left; width: 16px; height: 16px; margin-right: 3px; border: 1px dashed lightgray; background-color: ' + themeColors[color] + '"></div><div style="float: left;">' + color + '</div>');
        }
        values.push('<div style="float: left; width: 16px; height: 16px; margin-right: 3px; border: 1px dashed lightgray;"></div><div style="float: left;">bootstrap</div>');

        var buttons = [
            React.createElement("div", {
                style: {
                    width: 101,
                    float: 'left'
                }
            }, React.createElement(Dropdown, {
                values: values,
                selectedValue: 'Theme',
                onValueChanged: this.onThemeChanged
            })),
            React.createElement("div", {
                className: "orb-tlbr-btn orb-tlbr-btn-expandall"
            }),
            React.createElement("div", {
                className: "orb-tlbr-btn orb-tlbr-btn-collapseall"
            })
        ];

        return React.createElement("div", null,
            buttons
        );
    }
});
