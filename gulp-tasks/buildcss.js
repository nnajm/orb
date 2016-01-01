/* global require, Buffer */

'use strict';

module.exports = function () {
    
    var gulp = require('gulp'),
        replace = require('gulp-replace'),
        rename = require('gulp-rename'),
        tap = require('gulp-tap'),
        header = require('gulp-header'),
        concat = require('gulp-concat'),
        less = require('gulp-less'),
        cssnano = require('gulp-cssnano'),

        vars = require('./variables'),

        customless = require('../src/css/parselessvars'),    
        themecss = customless(require('fs').readFileSync('./src/css/orb.theme.less', 'utf8'), require('../src/css/theme.default.json'));
    
    return gulp.src(['./src/css/orb.less', './src/css/orb.bootstrap.less'])
        .pipe(concat('orb.less'))        
        // remove comments
        .pipe(replace(/\/\*[\s\S]+?\*\//gm, ''))
        .pipe(less())
        // prepend less vars
        .pipe(tap(function (file) {
            file.contents = Buffer.concat([
                file.contents,
                new Buffer(themecss)
            ]);
        }))
        // add banner
        .pipe(header(vars.banner, { pkg: vars.pkg, years: vars.years }))
	
        // to dist folder
        .pipe(rename(vars.lib.name + '.css'))
        .pipe(gulp.dest(vars.dist.folder))

        // to website folder
        .pipe(gulp.dest(vars.website.cssFolder))

        // to versioned folder
        .pipe(rename(vars.lib.versionedName + '.css'))
        .pipe(gulp.dest(vars.dist.versionedFolder))

        // minify
        .pipe(cssnano())

        // to dist folder
        .pipe(rename(vars.lib.name + '.min.css'))
        .pipe(gulp.dest(vars.dist.folder))

        // to website folder
        .pipe(gulp.dest(vars.website.cssFolder))

        // to versioned folder
        .pipe(rename(vars.lib.versionedName + '.min.css'))
        .pipe(gulp.dest(vars.dist.versionedFolder));
};
