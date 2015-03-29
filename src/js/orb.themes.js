/* global module */
/*jshint eqnull: true*/

'use strict';

module.exports = (function() {

    var currentTheme = 'blue';
    var themeManager = {};

    function isBootstrap() {
        return currentTheme === 'bootstrap';
    }

    themeManager.themes = {
        red: '#C72C48',
        blue: '#5bc0de',
        green: '#3fb618',
        orange: '#df691a',
        flower: '#A74AC7',
        gray: '#808080',
        black: '#000000',
        white: '#FFFFFF'
    };

    themeManager.current = function(newTheme) {
        if (newTheme) {
            currentTheme = themeManager.validateTheme(newTheme);
        }

        return currentTheme;
    };

    themeManager.validateTheme = function(themeName) {
        themeName = (themeName || '').toString().trim();
        if (!themeManager.themes[themeName] && themeName !== 'bootstrap') {
            return 'blue';
        } else {
            return themeName;
        }
    };

    themeManager.getPivotClasses = function() {
        return {
            container: 'orb-container orb-' + currentTheme,
            table: 'orb' + (isBootstrap() ? ' table' : '')
        };
    };

    themeManager.getButtonClasses = function() {
        return {
            pivotButton: 'fld-btn' + (isBootstrap() ? ' btn btn-default btn-xs' : ''),
            orbButton: 'orb-btn' + (isBootstrap() ? ' btn btn-default btn-xs' : ''),
            scrollBar: isBootstrap() ? ' btn btn-default btn-xs' : ''
        };
    };

    themeManager.getFilterClasses = function() {
        return {
            container: 'orb-' + currentTheme + ' orb fltr-cntnr'
        };
    };

    themeManager.getGridClasses = function() {
        return {
            table: isBootstrap() ? 'table table-condensed' : 'orb-table'
        };
    };

    themeManager.getDialogClasses = function(visible) {
        var classes = {
            overlay: 'orb-overlay orb-overlay-' + (visible ? 'visible' : 'hidden') + ' orb-' + currentTheme,
            dialog: 'orb-dialog',
            content: '',
            header: 'orb-dialog-header',
            title: '',
            body: 'orb-dialog-body'
        };

        if (isBootstrap()) {
            classes.overlay += ' modal';
            classes.dialog += ' modal-dialog';
            classes.content = 'modal-content';
            classes.header += ' modal-header';
            classes.title = 'modal-title';
            classes.body += ' modal-body';
        }
        return classes;
    };

    var utils = themeManager.utils = {
        hexToRgb: function(hex) {
            var result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
            return result ? {
                r: parseInt(result[1], 16),
                g: parseInt(result[2], 16),
                b: parseInt(result[3], 16)
            } : null;
        },
        rgbaToHex: function(rgba) {
            var matches = rgba.match(/rgba\((\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+(?:\.\d+)?)\s*\)/);
            if(matches) {
                var alpah = parseFloat(matches[4]);
                return '#' +
                    utils.applyAlphaAndToHex(matches[1], alpah) +
                    utils.applyAlphaAndToHex(matches[2], alpah) +
                    utils.applyAlphaAndToHex(matches[3], alpah);
            }
            return null;
        },
        applyAlphaAndToHex: function(value, alpha) {
           return (Math.floor(alpha*parseInt(value) + (1-alpha)*255) + 256).toString(16).substr(1,2);
        },
        fadeoutColor: function(color, alpha) {
            color = utils.hexToRgb(color);
            return '#' +
               utils.applyAlphaAndToHex(color.r, alpha) +
               utils.applyAlphaAndToHex(color.g, alpha) +
               utils.applyAlphaAndToHex(color.b, alpha);
        }
     };

    return themeManager;
}());
