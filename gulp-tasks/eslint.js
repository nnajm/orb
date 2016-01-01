/* global require */

'use strict';

module.exports = function () {
    
    var gulp = require('gulp'),
        eslint = require('gulp-eslint');
    
    return gulp
        .src(['./src/js/**/*.js', './src/js/**/*.jsx'])
        .pipe(eslint())
        .pipe(eslint.format());
};
