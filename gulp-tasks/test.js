/* global require */

'use strict';

// This is required to transpile jsx files on the fly
require('babel-register')({
    presets: ['es2015', 'react', 'stage-0']
});

module.exports = function () {
    
    var gulp = require('gulp'),
        jasmine = require('gulp-jasmine');
    
    return gulp.src('./test/spec/orb.query.js')
        .pipe(jasmine({
            verbose: true
        }));
};