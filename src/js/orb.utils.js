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


// from: https://github.com/davidchambers/Base64.js

(function(object) {
    var chars = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';

    function InvalidCharacterError(message) {
        this.message = message;
    }
    InvalidCharacterError.prototype = new Error();
    InvalidCharacterError.prototype.name = 'InvalidCharacterError';
    // encoder
    // [https://gist.github.com/999166] by [https://github.com/nignag]
    object.btoa = global && global.btoa ? function(str) { return global.btoa(str); } :
         function(input) {
            var str = String(input);
            for (
                // initialize result and counter
                var block, charCode, idx = 0, map = chars, output = '';
                // if the next str index does not exist:
                // change the mapping table to "="
                // check if d has no fractional digits
                str.charAt(idx | 0) || (map = '=', idx % 1);
                // "8 - idx % 1 * 8" generates the sequence 2, 4, 6, 8
                output += map.charAt(63 & block >> 8 - idx % 1 * 8)
            ) {
                charCode = str.charCodeAt(idx += 3 / 4);
                if (charCode > 0xFF) {
                    throw new InvalidCharacterError("'btoa' failed: The string to be encoded contains characters outside of the Latin1 range.");
                }
                block = block << 8 | charCode;
            }
            return output;
        };

    // decoder
    // [https://gist.github.com/1020396] by [https://github.com/atk]
    object.atob = global && global.atob ? function(str) { return global.atob(str); } :
        function(input) {
            var str = String(input).replace(/=+$/, '');
            if (str.length % 4 == 1) {
                throw new InvalidCharacterError("'atob' failed: The string to be decoded is not correctly encoded.");
            }
            for (
                // initialize result and counters
                var bc = 0, bs, buffer, idx = 0, output = '';
                // get next character
                (buffer = str.charAt(idx++));
                // character found in table? initialize bit storage and add its ascii value;
                ~buffer && (bs = bc % 4 ? bs * 64 + buffer : buffer,
                    // and if not first of each 4 characters,
                    // convert the first 8 bits to one ascii character
                    bc++ % 4) ? output += String.fromCharCode(255 & bs >> (-2 * bc & 6)) : 0
            ) {
                // try to find character in table (0-63, not found => -1)
                buffer = chars.indexOf(buffer);
            }
            return output;
        };
}(module.exports));
