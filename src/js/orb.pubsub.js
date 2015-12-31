/**
 * @fileOverview Publish/Subscribe pattern implementation
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

'use strict';

/* global module, require */
/*jshint eqnull: true*/

var utils = require('./orb.utils');

/**
 * Creates a new instance of pubsub.
 * @class
 * @memberOf orb
 */
module.exports = function() {
    var _topics = {};
    
    this.subscribe = function(topic, callback) {
        if(utils.isString(topic) && utils.isFunction(callback)) {
            _topics[topic] = _topics[topic] || [];
            _topics[topic].push(callback);
        }
    };
    
    this.publish = function(topic /*, callback arguments */) {
        if(utils.isString(topic)) {
            utils.forEach(_topics[topic], function(callback) {
                callback.apply(null, [topic].concat(Array.prototype.slice.call(arguments, 1)));
            }); 
        }
    };
};