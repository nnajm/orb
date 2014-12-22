/**
 * @fileOverview Pivot Grid columns viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

var axe = require('./orb.axe');
var uiheaders = require('./orb.ui.header');

/**
 * Creates a new instance of columns ui properties.
 * @class
 * @memberOf orb.ui
 * @param  {orb.axe} columnsAxe - axe containing all columns dimensions.
 */
module.exports = function(columnsAxe) {

    var self = this;

    /**
     * Column dimensions axe
     * @type {orb.axe}
     */
    this.axe = columnsAxe;

    /**
     * Columns render properties
     * @type {Array}
     */
    this.uiInfos = null;

    this.leafsHeaders = null;

    var _multidatafields;
    var _datafieldscount;

    this.build = function() {

        _datafieldscount = self.axe.pgrid.config.dataHeadersLocation === 'columns' ? self.axe.pgrid.config.dataFieldsCount : 1;
        _multidatafields = self.axe.pgrid.config.dataHeadersLocation === 'columns' && _datafieldscount > 1;

        self.uiInfos = [];

        if (self.axe != null) {
            // Fill columns layout infos
            for (var depth = self.axe.root.depth; depth > 1; depth--) {
                self.uiInfos.push([]);
                getUiInfo(depth, self.uiInfos);
            }

            if (self.axe.pgrid.config.grandTotal.columnsvisible) {
                // add grandtotal header
                (self.uiInfos[0] = self.uiInfos[0] || []).push(new uiheaders.header(axe.Type.COLUMNS, uiheaders.HeaderType.GRAND_TOTAL, self.axe.root, null, _datafieldscount));
            }

            if (self.uiInfos.length === 0) {
                self.uiInfos.push([new uiheaders.header(axe.Type.COLUMNS, uiheaders.HeaderType.INNER, self.axe.root, null, _datafieldscount)]);
            }

            // generate leafs headers
            generateLeafsHeaders();
        }
    };

    function generateLeafsHeaders() {

        var leafsHeaders = [];

        function pushsubtotal(pheader) {
            if (pheader && pheader.dim.field.subTotal.visible) {
                leafsHeaders.push(pheader.subtotalHeader);
            }
        }

        if (self.uiInfos.length > 0) {
            // last headers row
            var infos = self.uiInfos[self.uiInfos.length - 1];
            var header = infos[0];

            var currparent,
                prevpar = header.parent;

            for (var i = 0; i < infos.length; i++) {
                header = infos[i];
                currparent = header.parent;
                // if current header parent is different than previous header parent,
                // add previous parent
                if (currparent != prevpar) {
                    pushsubtotal(prevpar);
                    if (currparent != null) {
                        // walk up parent hierarchy and add grand parents if different 
                        // than current header grand parents
                        var grandpar = currparent.parent;
                        var prevgrandpar = prevpar ? prevpar.parent : null;
                        while (grandpar != prevgrandpar && prevgrandpar != null) {
                            pushsubtotal(prevgrandpar);
                            grandpar = grandpar ? grandpar.parent : null;
                            prevgrandpar = prevgrandpar ? prevgrandpar.parent : null;
                        }
                    }
                    // update previous parent variable
                    prevpar = currparent;
                }
                // push current header
                leafsHeaders.push(infos[i]);

                // if it's the last header, add all of its parents up to the top
                if (i === infos.length - 1) {
                    while (prevpar != null) {
                        pushsubtotal(prevpar);
                        prevpar = prevpar.parent;
                    }
                }
            }
            // grandtotal is visible for columns and if there is more than one dimension in this axe
            if (self.axe.pgrid.config.grandTotal.columnsvisible && self.axe.dimensionsCount > 1) {
                // push also grand total header
                leafsHeaders.push(self.uiInfos[0][self.uiInfos[0].length - 1]);
            }
        }

        // add data headers if more than 1 data field and they willbe the leaf headers
        if (_multidatafields) {
            self.leafsHeaders = [];
            for (var leafIndex = 0; leafIndex < leafsHeaders.length; leafIndex++) {
                for (var datafieldindex = 0; datafieldindex < _datafieldscount; datafieldindex++) {
                    self.leafsHeaders.push(new uiheaders.dataHeader(self.axe.pgrid.config.dataFields[datafieldindex], leafsHeaders[leafIndex]));
                }
            }
            self.uiInfos.push(self.leafsHeaders);
        } else {
            self.leafsHeaders = leafsHeaders;
        }
    }

    this.build();

    /**
     * Fills the infos array given in argument with the dimension layout infos as column.
     * @param  {orb.dimension}  dimension - the dimension to get ui info for
     * @param  {int}  depth - the depth of the dimension that it's subdimensions will be returned
     * @param  {object}  infos - array to fill with ui dimension info
     */
    function getUiInfo(depth, uiInfos) {

        var infos = uiInfos[uiInfos.length - 1];
        var parents = self.axe.root.depth === depth ? [null] :
            uiInfos[self.axe.root.depth - depth - 1].filter(function(p) {
                return p.type !== uiheaders.HeaderType.SUB_TOTAL;
            });

        for (var pi = 0; pi < parents.length; pi++) {

            var parent = parents[pi];
            var parentDim = parent == null ? self.axe.root : parent.dim;

            for (var di = 0; di < parentDim.values.length; di++) {

                var subvalue = parentDim.values[di];
                var subdim = parentDim.subdimvals[subvalue];

                var subtotalHeader;
                if (!subdim.isLeaf && subdim.field.subTotal.visible) {
                    subtotalHeader = new uiheaders.header(axe.Type.COLUMNS, uiheaders.HeaderType.SUB_TOTAL, subdim, parent, _datafieldscount);
                } else {
                    subtotalHeader = null;
                }

                var header = new uiheaders.header(axe.Type.COLUMNS, null, subdim, parent, _datafieldscount, subtotalHeader);
                infos.push(header);

                if (!subdim.isLeaf && subdim.field.subTotal.visible) {
                    infos.push(subtotalHeader);
                }
            }
        }
    }
};
