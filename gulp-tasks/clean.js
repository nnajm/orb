/* global require */

'use strict';

module.exports = function () {
    
    var del = require('del'),
        vars = require('./variables');
    
    return del([       
        vars.dist.folder + '*.js',
        vars.dist.folder + '*.css',
        vars.dist.folder + '*.map',
        vars.dist.versionedFolder + '**',
        vars.website.jsFolder + '*.js',
        vars.website.jsFolder + '*.map',
        vars.website.cssFolder + '*.css'
    ], { force: true });
};