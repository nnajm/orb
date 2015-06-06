/**
 * @fileOverview Pivot Grid rows/columns viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

var axe = require('./orb.axe');
var uiheaders = require('./orb.ui.header');

/**
 * Creates a new instance of rows/columns ui properties.
 * @class
 * @memberOf orb.ui
 * @param  {orb.axe} axe - axe containing all dimensions.
 */
module.exports = function(axeModel) {

    var self = this;

    /**
     * Dimensions axe
     * @type {orb.axe}
     */
    this.axe = axeModel;

    /**
     * Headers render properties
     * @type {Array}
     */
    this.headers = [];

    this.dataFieldsCount = function() {
        return (self.axe.pgrid.config.dataHeadersLocation === 'columns' && self.axe.type === axe.Type.COLUMNS) ||
               (self.axe.pgrid.config.dataHeadersLocation === 'rows' && self.axe.type === axe.Type.ROWS) ?
                     self.axe.pgrid.config.dataFieldsCount :
                     1;
    };

    this.isMultiDataFields = function() {
        return self.dataFieldsCount() > 1;
    };

    this.toggleFieldExpansion = function(field, newState) {
        var toToggle = [];
        var allExpanded = true;
        var hIndex;

        for(var i = 0; i < this.headers.length; i++) {
            for(hIndex = 0; hIndex < this.headers[i].length; hIndex++) {
                var header = this.headers[i][hIndex];
                if(header.type === uiheaders.HeaderType.SUB_TOTAL && (field == null || header.dim.field.name == field.name)) {
                    toToggle.push(header);
                    allExpanded = allExpanded && header.expanded;
                }
            }
        }

        if(newState !== undefined) {
            allExpanded = !newState;
        }

        if(toToggle.length > 0) {
            for(hIndex = 0; hIndex < toToggle.length; hIndex++) {
                if(allExpanded) {
                    toToggle[hIndex].collapse();
                } else {
                    toToggle[hIndex].expand();
                }
            }
            return true;
        }

        return false;
    };    
};
