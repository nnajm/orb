/**
 * @fileOverview Utility functions
 * @author Najmeddine Nouri <najmno@gmail.com>
 */

module.exports = {
    /**
     * Creates a namespcae hierarchy if not exists
     * @param  {string} identifier - namespace identifier
     * @return {object}
     */
    ns: function(identifier, parent) {
        var parts = identifier.split('.');
        var i = 0;
        parent = parent || window;
        while (i < parts.length) {
            parent[parts[i]] = parent[parts[i]] || {};
            parent = parent[parts[i]];
            i++;
        }
        return parent;
    },
    /**
     * Returns an array of object own properties
     * @param  {Object} obj
     * @return {Array}
     */
    ownProperties: function(obj) {
        var arr = [];
        for (var prop in obj) {
            if (obj.hasOwnProperty(prop)) {
                arr.push(prop);
            }
        }
        return arr;
    },
    /**
     * Returns whether or not obj is a javascript array.
     * @param  {object}  obj
     * @return {Boolean}
     */
    isArray: function(obj) {
        return Object.prototype.toString.apply(obj) === '[object Array]';
    },
    /**
     * Returns whether or not obj is a number
     * @param  {object}  obj
     * @return {Boolean}
     */
    isNumber: function(obj) {
        return Object.prototype.toString.apply(obj) === '[object Number]';
    },
    /**
     * Returns whether or not obj is a Date object.
     * @param  {object}  obj
     * @return {Boolean}
     */
    isDate: function(obj) {
        return Object.prototype.toString.apply(obj) === '[object Date]';
    },
    /**
     * Returns whether or not obj is a string
     * @param  {object}  obj
     * @return {Boolean}
     */
    isString: function(obj) {
        return Object.prototype.toString.apply(obj) === '[object String]';
    },
    /**
     * Returns whether or not obj is a regular expression object
     * @param  {object}  obj
     * @return {Boolean}
     */
    isRegExp: function(obj) {
        return Object.prototype.toString.apply(obj) === '[object RegExp]';
    },
    /**
     * Escapes all RegExp special characters.
     */
    escapeRegex: function(re) {
        return re.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&');
    },
    /**
     * Returns the first element in the array that satisfies the given predicate
     * @param  {Array} array     the array to search
     * @param  {function} predicate Function to apply to each element until it returns true
     * @return {Object}           The first object in the array that satisfies the predicate or undefined.
     */
    findInArray: function(array, predicate) {
        if (this.isArray(array) && predicate) {
            for (var i = 0; i < array.length; i++) {
                var item = array[i];
                if (predicate(item)) {
                    return item;
                }
            }
        }
        return undefined;
    },
    /**
     * Returns a JSON string represenation of an object
     * @param {object} obj
     * @return {string}
     */
    jsonStringify: function(obj, censorKeywords) {
        function censor(key, value) {
            return censorKeywords && censorKeywords.indexOf(key) > -1 ? undefined : value;
        }
        return JSON.stringify(obj, censor, 2);
    }
};
