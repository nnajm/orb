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
        blue: '#268BD2',
        green: '#3A9D23',
        orange: '#f7840d',
        flower: '#A74AC7',
        gray: '#808080',
        white: '#FFFFFF',
        black: '#000000'
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
            table: isBootstrap() ? 'table table-striped table-condensed' : 'orb-table'
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

    return themeManager;
}());
