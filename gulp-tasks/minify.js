/* global require */

'use strict';

module.exports = function () {
    
    var gulp = require('gulp'),
        rename = require('gulp-rename'),
        uglify = require('gulp-uglify'),
        sourcemaps = require('gulp-sourcemaps'),
        header = require('gulp-header'),

        vars = require('./variables');

    return gulp.src(vars.dist.folder + vars.lib.name + '.js')
        .pipe(sourcemaps.init({ loadMaps: true }))
        // Add transformation tasks to the pipeline here.
        .pipe(uglify({ output: { ascii_only: true } }))
        .pipe(header(vars.banner, { pkg: vars.pkg, years: vars.years }))

        // to dist folder
        .pipe(rename(vars.lib.name + '.min.js'))
        .pipe(sourcemaps.write('./'))
        .pipe(gulp.dest(vars.dist.folder))

        // to website folder
        .pipe(gulp.dest(vars.website.jsFolder))

        // to versioned folder
        .pipe(sourcemaps.init({ loadMaps: true }))
        .pipe(rename(vars.lib.versionedName  + '.min.js'))
        .pipe(sourcemaps.write('./'))
        .pipe(gulp.dest(vars.dist.versionedFolder));
};