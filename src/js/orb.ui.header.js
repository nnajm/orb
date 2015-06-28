/**
 * @fileOverview Pivot Grid rows viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/


var axe = require('./orb.axe');
var state = new (require('./orb.state'))();

var HeaderType = module.exports.HeaderType = {
    EMPTY: 1,
    DATA_HEADER: 2,
    DATA_VALUE: 3,
    FIELD_BUTTON: 4,
    INNER: 5,
    WRAPPER: 6,
    SUB_TOTAL: 7,
    GRAND_TOTAL: 8,
    getHeaderClass: function(headerType, axetype) {
        var cssclass = axetype === axe.Type.ROWS ? 'header-row' : (axetype === axe.Type.COLUMNS ? 'header-col' : '');
        switch (headerType) {
            case HeaderType.EMPTY:
            case HeaderType.FIELD_BUTTON:
                cssclass = 'empty';
                break;
            case HeaderType.INNER:
                cssclass = 'header ' + cssclass;
                break;
            case HeaderType.WRAPPER:
                cssclass = 'header ' + cssclass;
                break;
            case HeaderType.SUB_TOTAL:
                cssclass = 'header header-st ' + cssclass;
                break;
            case HeaderType.GRAND_TOTAL:
                cssclass = 'header header-gt ' + cssclass;
                break;
        }

        return cssclass;
    },
    getCellClass: function(rowHeaderType, colHeaderType) {
        var cssclass = '';
        switch (rowHeaderType) {
            case HeaderType.GRAND_TOTAL:
                cssclass = 'cell-gt';
                break;
            case HeaderType.SUB_TOTAL:
                if (colHeaderType === HeaderType.GRAND_TOTAL) {
                    cssclass = 'cell-gt';
                } else {
                    cssclass = 'cell-st';
                }
                break;
            default:
                if (colHeaderType === HeaderType.GRAND_TOTAL) {
                    cssclass = 'cell-gt';
                } else if (colHeaderType === HeaderType.SUB_TOTAL) {
                    cssclass = 'cell-st';
                } else {
                    cssclass = '';
                }
        }
        return cssclass;
    }
};

function CellBase(options) {
    /**
     * axe type (COLUMNS, ROWS, DATA, ...)
     * @type {orb.axe.Type}
     */
    this.axetype = options.axetype;
    /**
     * cell type (EMPTY, DATA_VALUE, FIELD_BUTTON, INNER, WRAPPER, SUB_TOTAL, GRAND_TOTAL, ...)
     * @type {HeaderType}
     */
    this.type = options.type;
    /**
     * header cell template
     * @type {String}
     */
    this.template = options.template;
    /**
     * header cell value
     * @type {Object}
     */
    this.value = options.value;
    /**
     * is header cell expanded
     * @type {Boolean}
     */
    this.expanded = true;
    /**
     * header cell css class(es)
     * @type {String}
     */
    this.cssclass = options.cssclass;
    /**
     * header cell width
     * @type {Number}
     */
    this.hspan = options.hspan || function() {
        return 1;
    };
    /**
     * gets header cell's height
     * @return {Number}
     */
    this.vspan = options.vspan || function() {
        return 1;
    };
    /**
     * gets wether header cell is visible
     * @return {Boolean}
     */
    this.visible = options.isvisible || function() {
        return true;
    };

    this.key = this.axetype + this.type + this.value;
    this.getState = function() { return state.get(this.key); };
    this.setState = function(newState) { state.set(this.key, newState); };
}

/**
 * Creates a new instance of a row header.
 * @class
 * @memberOf orb.ui
 * @param  {orb.ui.rowHeader} parent - parent header.
 * @param  {orb.dimension} dim - related dimension values container.
 * @param  {HeaderType} type - header type (INNER, WRAPPER, SUB_TOTAL, GRAND_TOTAL).
 * @param  {orb.ui.rowHeader} totalHeader - sub total or grand total related header.
 */
module.exports.header = function(axetype, headerType, dim, parent, datafieldscount, subtotalHeader) {

    var self = this;

    var hspan;
    var vspan;
    var value;

    var isRowsAxe = axetype === axe.Type.ROWS;
    headerType = headerType || (dim.depth === 1 ? HeaderType.INNER : HeaderType.WRAPPER);

    switch (headerType) {
        case HeaderType.GRAND_TOTAL:
            value = 'Grand Total';
            hspan = isRowsAxe ? dim.depth - 1 || 1 : datafieldscount;
            vspan = isRowsAxe ? datafieldscount : dim.depth - 1 || 1;
            break;
        case HeaderType.SUB_TOTAL:
            value = dim.value;
            hspan = isRowsAxe ? dim.depth : datafieldscount;
            vspan = isRowsAxe ? datafieldscount : dim.depth;
            break;
        default:
            value = dim.value;
            hspan = isRowsAxe ? 1 : null;
            vspan = isRowsAxe ? null : 1;
            break;
    }

    CellBase.call(this, {
        axetype: axetype,
        type: headerType,
        template: isRowsAxe ? 'cell-template-row-header' : 'cell-template-column-header',
        value: value,
        cssclass: HeaderType.getHeaderClass(headerType, axetype),
        hspan: hspan != null ? function() {
            return hspan;
        } : calcSpan,
        vspan: vspan != null ? function() {
            return vspan;
        } : calcSpan,
        isvisible: isParentExpanded
    });

    this.subtotalHeader = subtotalHeader;
    this.parent = parent;
    this.subheaders = [];
    this.dim = dim;
    this.expanded = this.getState() ? this.getState().expanded : (headerType !== HeaderType.SUB_TOTAL || !dim.field.subTotal.collapsed);

    this.expand = function() {
        self.expanded = true;
        this.setState({
            expanded: self.expanded
        });
    };
    this.collapse = function() {
        self.expanded = false;
        this.setState({
            expanded: self.expanded
        });
    };

    if (parent != null) {
        parent.subheaders.push(this);
    }

    function isParentExpanded() {
        if (self.type === HeaderType.SUB_TOTAL) {
            var hparent = self.parent;
            while (hparent != null) {
                if (hparent.subtotalHeader && !hparent.subtotalHeader.expanded) {
                    return false;
                }
                hparent = hparent.parent;
            }
            return true;
        } else {

            var isexpanded = self.dim.isRoot || self.dim.isLeaf || !self.dim.field.subTotal.visible || self.subtotalHeader.expanded;
            if (!isexpanded) {
                return false;
            }

            var par = self.parent;
            while (par != null && (!par.dim.field.subTotal.visible || (par.subtotalHeader != null && par.subtotalHeader.expanded))) {
                par = par.parent;
            }
            return par == null || par.subtotalHeader == null ? isexpanded : par.subtotalHeader.expanded;
        }
    }

    function calcSpan(ignoreVisibility) {
        var tspan = 0;
        var subSpan;
        var addone = false;

        if (isRowsAxe || ignoreVisibility || self.visible()) {
            if (!self.dim.isLeaf) {
                // subdimvals 'own' properties are the set of values for this dimension
                if(self.subheaders.length > 0) {
                    for (var i = 0; i < self.subheaders.length; i++) {
                        var subheader = self.subheaders[i];
                        // if its not an array
                        if (!subheader.dim.isLeaf) {
                            subSpan = isRowsAxe ? subheader.vspan() : subheader.hspan();
                            tspan += subSpan;
                            if (i === 0 && (subSpan === 0)) {
                                addone = true;
                            }
                        } else {
                            tspan += datafieldscount;
                        }
                    }
                } else {
                    tspan += datafieldscount;
                }
            } else {
                return datafieldscount;
            }
            return tspan + (addone ? 1 : 0);
        }
        return tspan;
    }
};

module.exports.dataHeader = function(datafield, parent) {

    CellBase.call(this, {
        axetype: null,
        type: HeaderType.DATA_HEADER,
        template: 'cell-template-dataheader',
        value: datafield,
        cssclass: HeaderType.getHeaderClass(parent.type, parent.axetype),
        isvisible: parent.visible
    });

    this.parent = parent;
};

module.exports.dataCell = function(pgrid, isvisible, rowinfo, colinfo) {

    this.rowDimension = rowinfo.type === HeaderType.DATA_HEADER ? rowinfo.parent.dim : rowinfo.dim;
    this.columnDimension = colinfo.type === HeaderType.DATA_HEADER ? colinfo.parent.dim : colinfo.dim;
    this.rowType = rowinfo.type === HeaderType.DATA_HEADER ? rowinfo.parent.type : rowinfo.type;
    this.colType = colinfo.type === HeaderType.DATA_HEADER ? colinfo.parent.type : colinfo.type;

    this.datafield = pgrid.config.dataFieldsCount > 1 ?
        (pgrid.config.dataHeadersLocation === 'rows' ?
            rowinfo.value :
            colinfo.value) :
        pgrid.config.dataFields[0];


    CellBase.call(this, {
        axetype: null,
        type: HeaderType.DATA_VALUE,
        template: 'cell-template-datavalue',
        value: pgrid.getData(this.datafield ? this.datafield.name : null, this.rowDimension, this.columnDimension),
        cssclass: 'cell ' + HeaderType.getCellClass(this.rowType, this.colType),
        isvisible: isvisible
    });
};

module.exports.buttonCell = function(field) {

    CellBase.call(this, {
        axetype: null,
        type: HeaderType.FIELD_BUTTON,
        template: 'cell-template-fieldbutton',
        value: field,
        cssclass: HeaderType.getHeaderClass(HeaderType.FIELD_BUTTON)
    });
};

module.exports.emptyCell = function(hspan, vspan) {

    CellBase.call(this, {
        axetype: null,
        type: HeaderType.EMPTY,
        template: 'cell-template-empty',
        value: null,
        cssclass: HeaderType.getHeaderClass(HeaderType.EMPTY),
        hspan: function() {
            return hspan;
        },
        vspan: function() {
            return vspan;
        },
    });
};
