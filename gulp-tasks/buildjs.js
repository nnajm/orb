/* global require */

'use strict';

// This is required to transpile jsx files on the fly
require('babel-register')({
    presets: ['es2015', 'react', 'stage-0']
});

module.exports = function () {

    var gulp = require('gulp'),
        browserify = require('browserify'),
        derequire = require('gulp-derequire'),
        replace = require('gulp-replace'),
        rename = require('gulp-rename'),
        source = require('vinyl-source-stream'),
        buffer = require('vinyl-buffer'),
        beautify = require('gulp-jsbeautifier'),
        header = require('gulp-header'),
        babelify = require('babelify'),

        vars = require('./variables');
    
    var bundler = browserify({
        entries: ['./src/js/orb.js'],
        debug: false,
        standalone: 'orb'
    }).external('react')
        .external('react-dom');

    var bundle = function () {
        return bundler
            .transform(babelify, {
                presets: ['es2015', 'react', 'stage-0'],
                plugins: ['transform-runtime']
            })
            .bundle()
            .pipe(source(vars.lib.name + '.js'))
            .pipe(derequire())
            .pipe(buffer())
            .pipe(replace(/\/\*[\s\S]+?\*\//gm, ''))
            .pipe(replace(/('use strict'|"use strict");?/gm, ''))
            .pipe(replace(/[\n]{2,}/gm, '\n\n'))
            .pipe(beautify({ indent_size: 2 }))
            .pipe(header(vars.banner + vars.jshintOptions + '\'use strict\';\n', { pkg: vars.pkg, years: vars.years }))

            // to dist folder
            .pipe(gulp.dest(vars.dist.folder))

            // to website folder
            .pipe(gulp.dest(vars.website.jsFolder))

            // to versioned folder
            .pipe(rename(vars.lib.versionedName + '.js'))
            .pipe(gulp.dest(vars.dist.versionedFolder));
    };

    return bundle();
};