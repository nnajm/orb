/**
 * @fileOverview Pivot Grid rows viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

var axe = require('./orb.axe');
var axeUi = require('./orb.ui.axe');
var uiheaders = require('./orb.ui.header');

/**
 * Creates a new instance of rows ui properties.
 * @class
 * @memberOf orb.ui
 * @param  {orb.axe} rowsAxe - axe containing all rows dimensions.
 */
module.exports = function(rowsAxe) {

    var self = this;

    axeUi.call(self, rowsAxe);

    this.build = function() {
        var headers = [];
        var grandtotalHeader;

        if (self.axe != null) {
            if(self.axe.root.values.length > 0 || self.axe.pgrid.config.grandTotal.rowsvisible) {
                headers.push([]);

                // Fill Rows layout infos
                getUiInfo(headers, self.axe.root);

                if (self.axe.pgrid.config.grandTotal.rowsvisible) {
                    var lastrow = headers[headers.length - 1];
                    grandtotalHeader = new uiheaders.header(axe.Type.ROWS, uiheaders.HeaderType.GRAND_TOTAL, self.axe.root, null, self.dataFieldsCount());
                    if (lastrow.length === 0) {
                        lastrow.push(grandtotalHeader);
                    } else {
                        headers.push([grandtotalHeader]);
                    }
                }
            }

            if (headers.length === 0) {
                headers.push([grandtotalHeader = new uiheaders.header(axe.Type.ROWS, uiheaders.HeaderType.INNER, self.axe.root, null, self.dataFieldsCount())]);
            }

            if(grandtotalHeader) {
                // add grand-total data headers if more than 1 data field and they will be the leaf headers
                addDataHeaders(headers, grandtotalHeader);
            }
        }
        self.headers = headers;
    };

    this.build();

    function addDataHeaders(infos, parent) {
        if (self.isMultiDataFields()) {
            var lastInfosArray = infos[infos.length - 1];
            for (var datafieldindex = 0; datafieldindex < self.dataFieldsCount(); datafieldindex++) {
                lastInfosArray.push(new uiheaders.dataHeader(self.axe.pgrid.config.dataFields[datafieldindex], parent));
                if (datafieldindex < self.dataFieldsCount() - 1) {
                    infos.push((lastInfosArray = []));
                }
            }
        }
    }

    /**
     * Fills the infos array given in argument with the dimension layout infos as row.
     * @param  {orb.dimension}  dimension - the dimension to get ui info for
     * @param  {object}  infos - array to fill with ui dimension info
     */
    function getUiInfo(infos, dimension) {
        if (dimension.values.length > 0) {

            var infosMaxIndex = infos.length - 1;
            var lastInfosArray = infos[infosMaxIndex];
            var parent = lastInfosArray.length > 0 ? lastInfosArray[lastInfosArray.length - 1] : null;

            for (var valIndex = 0; valIndex < dimension.values.length; valIndex++) {
                var subvalue = dimension.values[valIndex];
                var subdim = dimension.subdimvals[subvalue];

                var subTotalHeader;
                if (!subdim.isLeaf && subdim.field.subTotal.visible) {
                    subTotalHeader = new uiheaders.header(axe.Type.ROWS, uiheaders.HeaderType.SUB_TOTAL, subdim, parent, self.dataFieldsCount());
                } else {
                    subTotalHeader = null;
                }

                var newHeader = new uiheaders.header(axe.Type.ROWS, null, subdim, parent, self.dataFieldsCount(), subTotalHeader);

                if (valIndex > 0) {
                    infos.push((lastInfosArray = []));
                }

                lastInfosArray.push(newHeader);

                if (!subdim.isLeaf) {
                    getUiInfo(infos, subdim);
                    if (subdim.field.subTotal.visible) {
                        infos.push([subTotalHeader]);

                        // add sub-total data headers if more than 1 data field and they will be the leaf headers
                        addDataHeaders(infos, subTotalHeader);
                    }
                } else {
                    // add data headers if more than 1 data field and they will be the leaf headers
                    addDataHeaders(infos, newHeader);
                }
            }
        }
    }
};
