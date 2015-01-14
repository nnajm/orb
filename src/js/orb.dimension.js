/**
 * @fileOverview Pivot Grid dimension viewmodel
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module */
/*jshint eqnull: true*/

/**
 * Creates a new container for a row/column dimension values.<br/>
 * This object will have all informations related to a dimension: its values, depth, width and subdimensions.
 * @class
 * @memberOf orb
 * @param  {orb.dimension} parent - parent dimension
 * @param  {array} fields - array describing the fields used for an axe dimenisons
 * @param  {int} fieldindex - index of this dimension field in fields array
 * @param  {Boolean} isRoot - whether or not this is the root dimension for a given axe (row/column)
 */
module.exports = function(id, parent, value, field, depth, isRoot, isLeaf) {

    var self = this;
    /**
     * unique id within parent orb.axe instance.
     * @type {Number}
     */
    this.id = id;
    /**
     * parent subdimension
     * @type {orb.dimension}
     */
    this.parent = parent;
    /**
     * This instance dimension value
     * @type {object}
     */
    this.value = value;
    /**
     * Whether or not this is the root dimension for a given axe (row/column)
     * @type {Boolean}
     */
    this.isRoot = isRoot;
    /**
     * Whether or not this is the leaf (deepest) dimension for a given axe (row/column)
     * @type {Boolean}
     */
    this.isLeaf = isLeaf;
    /**
     * Dimension's data field
     * @type {Array}
     */
    this.field = field;
    /**
     * Dimension's depth (to the deepest sub-dimension)
     * @type {Number}
     */
    this.depth = depth;
    /**
     * Dimension's set of all values
     * @type {Array}
     */
    this.values = [];
    /**
     * Direct descendant subdimensions dictionary
     * @type {Object}
     */
    this.subdimvals = {};

    this.rowIndexes = null;

    this.getRowIndexes = function(result) {
        if (self.rowIndexes == null) {
            self.rowIndexes = [];
            for (var i = 0; i < self.values.length; i++) {
                self.subdimvals[self.values[i]].getRowIndexes(self.rowIndexes);
            }
        }
        if (result != null) {
            for (var j = 0; j < self.rowIndexes.length; j++) {
                result.push(self.rowIndexes[j]);
            }
            return result;
        } else {
            return self.rowIndexes;
        }
    };
};
