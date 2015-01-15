/** @jsx React.DOM */

/* global module, require, React */

'use strict';

var react = typeof window === 'undefined' ? require('react') : window.React;
var utils = require('../orb.utils');
var axe = require('../orb.axe');
var uiheaders = require('../orb.ui.header');
var configuration = require('../orb.config').config;

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
                    topmost: index === 0,
                    rowButtonsCount: ptc.rowHeadersWidth,
                    rowButtonsCell: rowButtonsCell,
                    rootComp: self
                });
            } else {
                return React.createElement(PivotRow, {
                    key: index,
                    topmost: index === 0,
                    row: row,
                    rootComp: self
                });
            }
        });

        var useBootstrap = this.props.config.bootstrap;
        var containerClass = "orb-container" + (useBootstrap ? "" : " orb-theme");
        var orbtableClass = "orb" + (useBootstrap ? " table" : "");

        var tblStyle = {};
        if (this.props.config.width) {
            tblStyle.width = this.props.config.width;
        }
        if (this.props.config.height) {
            tblStyle.height = this.props.config.height;
        }


        return (
            React.createElement("div", {
                    className: containerClass,
                    style: tblStyle
                },
                React.createElement("table", {
                        id: "{'tbl' + self.id}",
                        className: orbtableClass,
                        style: {
                            width: '100%'
                        }
                    },
                    React.createElement("tbody", null,
                        React.createElement("tr", null,
                            React.createElement("td", {
                                    className: "fields-group-caption available-fields text-muted",
                                    colSpan: extraCol,
                                    rowSpan: "1"
                                },
                                React.createElement("div", null, "Fields")
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
                                    className: "fields-group-caption text-muted",
                                    colSpan: extraCol,
                                    rowSpan: "1"
                                },
                                React.createElement("div", null, "Data")
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
                var isleftmostHeader = index === 0;
                return React.createElement(PivotCell, {
                    key: index,
                    cell: cell,
                    topmost: self.props.topmost,
                    rightmost: isrightmost,
                    leftmostheader: isleftmostHeader,
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
                                    className: "toggle-button"
                                }, React.createElement("div", {
                                    className: 'toggle-button-' + (isWrapper ? 'down' : 'right'),
                                    onClick: (isWrapper ? this.collapse : this.expand)
                                })),
                                React.createElement("td", {
                                    className: "header-value"
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
                    self.props.rootComp.props.data.drilldown(cell, self.props.rootComp.id);
                }
                break;
            default:
                break;
        }

        if (!headerPushed) {
            divcontent.push(React.createElement("div", {
                key: "cell-value",
                className: cell.template !== 'cell-template-datavalue' ? 'header-value' : ''
            }, React.createElement("div", null, value)));
        }

        var classname = cell.cssclass;
        var isHidden = !cell.visible();

        if (isHidden) {
            classname += ' cell-hidden';
        }

        if (this.props.topmost && cell.template !== 'cell-template-empty') {
            classname += ' cell-topmost';
        }

        if (this.props.rightmost && (cell.axetype !== axe.Type.COLUMNS || cell.type === uiheaders.HeaderType.GRAND_TOTAL)) {
            classname += ' cell-rightmost';
        }

        if ((this.props.leftmost && cell.template !== 'cell-template-empty') || this.props.leftmostheader || this.props.leftmostdatavalue) {
            classname += ' cell-leftmost';
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
        var tableClass = this.props.bootstrap ? "table table-striped table-condensed" : "orb-table";

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
                for (var j = 0; j < data[i].length; j++) {
                    row.push(React.createElement("td", null, data[i][j]));
                }
                rows.push(React.createElement("tr", null, row));
            }
        }

        return React.createElement("table", {
                className: tableClass
            },
            React.createElement("tbody", null,
                rows
            )
        );
    }
});

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
            var dialog = dialogFactory({});
            var overlay = createOverlay();

            return {
                show: function(props) {
                    dialog.props = props;
                    React.render(dialog, overlay);
                }
            }
        }
    },
    overlayElement: null,
    componentDidMount: function() {
        this.overlayElement = this.getDOMNode().parentNode;
        this.overlayElement.className = "orb-overlay orb-overlay-visible" + (this.props.bootstrap ? " modal" : " orb-theme");
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
            this.overlayElement.className = "orb-overlay orb-overlay-hidden" + (this.props.bootstrap ? " modal" : " orb-theme");
        }
    },
    render: function() {
        var comp = React.createElement(this.props.comp.type, this.props.comp.props);
        var useBootstrap = this.props.bootstrap;
        var dialogClass = "orb-dialog" + (useBootstrap ? " modal-dialog" : "");
        var contentClass = useBootstrap ? "modal-content" : "";
        var headerClass = "orb-dialog-header" + (useBootstrap ? " modal-header" : "");
        var titleClass = useBootstrap ? "modal-title" : "";
        var bodyClass = "orb-dialog-body" + (useBootstrap ? " modal-body" : "");

        return React.createElement("div", {
                className: dialogClass,
                style: this.props.style || {}
            },
            React.createElement("div", {
                    className: contentClass
                },
                React.createElement("div", {
                    className: headerClass
                }, React.createElement("div", {
                    className: "button-close",
                    onClick: this.close
                }), React.createElement("div", {
                    className: titleClass
                }, this.props.title)),
                React.createElement("div", {
                        className: bodyClass
                    },
                    comp
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
        new FilterManager(this, this.getDOMNode(), this.pgridwidget.pgrid.getFieldFilter(this.props.field));
    },
    componentWillUnmount: function() {
        document.removeEventListener('mousedown', this.onMouseDown);
        document.removeEventListener('wheel', this.onMouseWheel);
        window.removeEventListener('resize', this.destroy);
    },
    render: function() {
        var values = this.pgridwidget.pgrid.getFieldValues(this.props.field);
        var checkboxes = [];

        function addCheckboxRow(value, text) {
            return checkboxes.push(React.createElement("tr", {
                    key: value
                },
                React.createElement("td", {
                        className: "filter-checkbox"
                    },
                    React.createElement("input", {
                        type: "checkbox",
                        value: value,
                        defaultChecked: "checked"
                    })
                ),
                React.createElement("td", {
                    className: "filter-value",
                    title: text || value
                }, text || value)
            ));
        }

        addCheckboxRow(configuration.FILTER.ALL, '(Show All)');
        if (values.containsBlank) {
            addCheckboxRow(configuration.FILTER.BLANK, '(Blank)');
        }

        for (var i = 0; i < values.length; i++) {
            addCheckboxRow(values[i]);
        }

        var buttonClass = 'orb-button' + (this.props.rootComp.props.data.pgrid.config.bootstrap ? ' btn btn-default btn-xs' : '');
        var pivotStyle = window.getComputedStyle(this.props.rootComp.getDOMNode(), null);
        var style = {
            fontFamily: pivotStyle.getPropertyValue('font-family'),
            fontSize: pivotStyle.getPropertyValue('font-size')
        };

        return React.createElement("table", {
                className: "filter-subcontainer",
                style: style
            },
            React.createElement("tbody", null,
                React.createElement("tr", null,
                    React.createElement("td", {
                            className: "search-operator-column"
                        },
                        React.createElement("div", {
                                className: "orb-select"
                            },
                            React.createElement("div", null, configuration.FILTER.Operators.IN),
                            React.createElement("ul", null,
                                React.createElement("li", null, configuration.FILTER.Operators.IN),
                                React.createElement("li", null, configuration.FILTER.Operators.NOTIN),
                                React.createElement("li", null, configuration.FILTER.Operators.EQ),
                                React.createElement("li", null, configuration.FILTER.Operators.NEQ),
                                React.createElement("li", null, configuration.FILTER.Operators.GT),
                                React.createElement("li", null, configuration.FILTER.Operators.GTE),
                                React.createElement("li", null, configuration.FILTER.Operators.LT),
                                React.createElement("li", null, configuration.FILTER.Operators.LTE)
                            )
                        )
                    ),
                    React.createElement("td", {
                        className: "search-box-column"
                    }, React.createElement("input", {
                        type: "text",
                        placeholder: "search"
                    })),
                    React.createElement("td", {
                        className: "search-type-column",
                        title: "Enable/disable Regular expressions"
                    }, ".*")
                ),
                React.createElement("tr", null,
                    React.createElement("td", {
                            colSpan: "3",
                            className: "filter-values-column"
                        },
                        React.createElement("table", {
                                className: "filter-values-table"
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
                            className: "confirm-buttons-column",
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
                            className: "resize-column"
                        },
                        React.createElement("div", null)
                    )
                )
            )
        );
    }
});

function FilterManager(reatComp, filterContainerElement, checkedValues) {

    var self = this;

    var allValues = [];
    var searchCheckedValues = [];
    var isSearchMode = false;
    var isRegexMode = false;
    var operator = '=';
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
        elems.searchBox = elems.filterContainer.rows[0].cells[1].children[0];
        elems.operatorBox = elems.filterContainer.rows[0].cells[0].children[0];
        elems.okButton = elems.filterContainer.rows[2].cells[0].children[0];
        elems.cancelButton = elems.filterContainer.rows[2].cells[0].children[1];
        elems.resizeGrip = elems.filterContainer.rows[2].cells[1].children[0];

        var rows = elems.filterContainer.rows[1].cells[0].children[0].rows;
        for (var i = 0; i < rows.length; i++) {
            var checkbox = rows[i].cells[0].children[0];
            elems.checkboxes[checkbox.value] = checkbox;
            allValues.push(checkbox.value);
        }

        elems.allCheckbox = elems.checkboxes[configuration.FILTER.ALL];
        elems.addCheckbox = null;
        elems.enableRegexButton = elems.filterContainer.rows[0].cells[2];

        elems.filterContainer.addEventListener('click', self.valueChecked);
        elems.searchBox.addEventListener('keyup', self.searchChanged);
        elems.enableRegexButton.addEventListener('click', function() {
            isRegexMode = !isRegexMode;
            elems.enableRegexButton.className = elems.enableRegexButton.className.replace('search-type-column-active', '');
            if (isRegexMode) {
                elems.enableRegexButton.className += ' search-type-column-active';
            }
            self.searchChanged('regexModeChanged');
        });
        elems.okButton.addEventListener('click', function() {
            reatComp.onFilter(isSearchMode ? searchCheckedValues : self.checkedValues);
        });
        elems.cancelButton.addEventListener('click', function() {
            reatComp.destroy();
        });

        var dropdownManager = new DropdownManager(elems.operatorBox, function(oldOperator, newOperator) {
            self.searchChanged('operatorChanged');
        });

        var resizeMan = new ResizeManager(elems.filterContainer.parentNode, elems.filterContainer.rows[1].cells[0].children[0], elems.resizeGrip);

        elems.resizeGrip.addEventListener('mousedown', resizeMan.resizeMouseDown);
        document.addEventListener('mouseup', resizeMan.resizeMouseUp);
        document.addEventListener('mousemove', resizeMan.resizeMouseMove);

        self.updateCheckboxes(newCheckedValues);
    };

    function ResizeManager(outerContainerElem, valuesTableElem, resizeGripElem) {

        var minContainerWidth = 215;
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
    }

    function DropdownManager(dropdowElement, valueChangedCallback) {
        var valueElement = dropdowElement.children[0];
        var listElement = dropdowElement.children[1];
        valueElement.addEventListener('click', function(e) {
            if (listElement.style.display !== 'block') {
                listElement.style.display = 'block';
                e.preventDefault();
                e.stopPropagation();
            }
        });
        listElement.addEventListener('click', function(e) {
            if (e.target.parentNode == listElement) {
                if (valueElement.textContent != e.target.textContent) {
                    valueElement.textContent = e.target.textContent;
                    valueChangedCallback(valueElement.textContent, e.target.textContent);
                }
            }
        });
        document.addEventListener('click', function(e) {
            listElement.style.display = 'none';
        });
    }

    this.valueChecked = function(e) {
        var target = e.target;
        if (target && target.type && target.type === 'checkbox') {
            self.updateCheckedValues(target == elems.allCheckbox);
        }
    };

    function escapeRegex(re) {
        return re.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
    }

    this.searchChanged = function(e, options) {
        var search = (elems.searchBox.value || '').trim();
        if ((e === 'regexModeChanged' && search) || search != lastSearchTerm) {
            lastSearchTerm = search;
            isSearchMode = search !== '';
            var searchRegex = isSearchMode ? new RegExp(isRegexMode ? search : escapeRegex(search), 'i') : undefined;
            var defaultDisplay = search ? 'none' : '';

            elems.allCheckbox.parentNode.parentNode.style.display = defaultDisplay;
            for (var i = 1; i < allValues.length; i++) {
                var val = allValues[i];
                var checkbox = elems.checkboxes[val];
                var visible = !isSearchMode || (val.search(searchRegex) >= 0);
                checkbox.parentNode.parentNode.style.display = visible ? '' : defaultDisplay;
                checkbox.checked = visible;
            }

            if (!isSearchMode) {
                searchCheckedValues = [];
                self.updateCheckboxes(self.checkedValues);
            } else {
                self.updateCheckedValues();
            }
        }
    };

    this.updateCheckedValues = function(allChecked) {
        if (allChecked) {
            self.checkedValues = elems.allCheckbox.checked ? configuration.FILTER.ALL : configuration.FILTER.NONE;
            self.updateCheckboxes(elems.allCheckbox.checked, configuration.FILTER.ALL);
        } else {
            var checkedArray = [];
            for (var i = 1; i < allValues.length; i++) {
                var val = allValues[i];
                var checkbox = elems.checkboxes[val];
                if (checkbox.checked) {
                    checkedArray.push(val);
                }
            }

            if (isSearchMode) {
                searchCheckedValues = checkedArray;
            } else {
                self.checkedValues = checkedArray;
            }
            self.updateAllCheckbox();
        }
        console.log(self.checkedValues + '\n' + searchCheckedValues);
    };

    this.updateCheckboxes = function(checkedList, source) {
        var allchecked = utils.isArray(checkedList) ?
            null :
            (checkedList == null || checkedList === configuration.FILTER.ALL ?
                true :
                (checkedList === configuration.FILTER.NONE ?
                    false :
                    !!checkedList
                )
            );
        for (var i = 1; i < allValues.length; i++) {
            var val = allValues[i];
            elems.checkboxes[val].checked = allchecked != null ? allchecked : checkedList.indexOf(val) >= 0;
        }

        if (source !== configuration.FILTER.ALL) {
            self.checkedValues = checkedList || (allchecked ? configuration.FILTER.ALL : configuration.FILTER.NONE);
            self.updateAllCheckbox();
        }
    };

    var INDETERMINATE = 'indeterminate';

    this.updateAllCheckbox = function() {
        if (!isSearchMode) {
            var allchecked = null;
            for (var i = 1; i < allValues.length; i++) {
                var checkbox = elems.checkboxes[allValues[i]];
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
            } else {
                elems.allCheckbox.indeterminate = false;
                elems.allCheckbox.checked = allchecked;
            }
        }
    };

    this.reset(filterContainerElement, checkedValues);
}

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
                className: 'drop-target' + (this.state.isover ? ' drop-target-drag-over' : '')
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
    onFilterMouseDown: function(e) {
        // left mouse button only
        if (e.button !== 0) return;

        var filterButton = this.getDOMNode().childNodes[0].rows[0].cells[2].childNodes[0];
        var filterButtonPos = getOffset(filterButton);
        var filterContainer = document.createElement('div');

        var filterPanelFactory = React.createFactory(comps.FilterPanel);
        var filterPanel = filterPanelFactory({
            field: this.props.field.name,
            rootComp: this.props.rootComp
        });

        filterContainer.className = (this.props.rootComp.props.data.pgrid.config.bootstrap ? '' : 'orb-theme') + ' orb filter-container';
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

        return true;
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

        var sortIndicator = self.props.field.sort.order === 'asc' ?
            ' \u2191' :
            (self.props.field.sort.order === 'desc' ?
                ' \u2193' :
                '');

        var filterClass = (self.state.dragging ? '' : 'filter-button') + (this.props.rootComp.props.data.pgrid.isFieldFiltered(this.props.field.name) ? ' filter-button-active' : '');

        return React.createElement("div", {
                key: self.props.field.name,
                className: 'field-button' + (this.props.rootComp.props.config.bootstrap ? ' btn btn-default' : ''),
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
                                width: 8
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
