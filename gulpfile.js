'use strict';

var gulp = require('gulp');
var browserify = require('browserify');
var derequire = require('gulp-derequire');
var replace = require('gulp-replace');
var rename = require('gulp-rename');
var source = require('vinyl-source-stream');
var buffer = require('vinyl-buffer');
var uglify = require('gulp-uglify');
var sourcemaps = require('gulp-sourcemaps');
var beautify = require('gulp-jsbeautifier');
var header = require('gulp-header');
var concat = require('gulp-concat');
var react = require('gulp-react');
var less = require('gulp-less');
var cleancss = require("gulp-minify-css");

var pkg = require('./package.json');
var year = new Date().getFullYear();                  
var years = '2014' + (year > 2014 ? '-' + year : '');
var banner = 
          '/**\n' +
          ' * <%= pkg.name %> v<%= pkg.version %>, <%= pkg.description %>.\n' +
          ' *\n' +
          ' * Copyright (c) <%= years %> <%= pkg.author %>.\n' +
          ' *\n' +
          ' * @version v<%= pkg.version %>\n' +
          ' * @link <%= pkg.homepage %>\n' +
          ' * @license <%= pkg.license %>\n' +
          ' */\n\n';

var getBundleName = function () {
  return 'orb-' + pkg.version;
};

gulp.task('less', function () {
	gulp.src('./src/css/orb.css')
	.pipe(replace(/\/\*[\s\S]+?\*\//gm, ''))
	.pipe(less({
		plugins: []//cleancss]
	}))
  .pipe(rename(getBundleName() + '.css'))
  .pipe(header(banner, { pkg : pkg, years: years } ))
  .pipe(gulp.dest('./dist/'))
  .pipe(gulp.dest('../orb-gh-pages/css/orb/'))
  .pipe(rename(getBundleName() + '.min.css'))
  .pipe(cleancss())
  .pipe(gulp.dest('./dist/'))
  .pipe(gulp.dest('../orb-gh-pages/css/orb/'));
});

gulp.task('react', function() {

	gulp.src(['./src/js/react/orb.react.components.jsx', './src/js/react/orb.react.dragndrop.jsx'])
	.pipe(concat('orb.react.compiled.js'))
  //.pipe(replace('var React = require(\'react\');', ''))
	.pipe(react())
  .pipe(beautify({indent_size: 2}))
  .pipe(gulp.dest('./src/js/react/'));
});

gulp.task('debug', ['react'], function() {

  var bundler = browserify({
    entries: ['./src/js/orb.js'],
    debug: false,
    standalone: 'orb'
  }).ignore('react');

  var bundle = function() {
    return bundler
    .bundle()
    .pipe(source(getBundleName() + '.js'))
    .pipe(derequire())
    .pipe(buffer())
    .pipe(replace(/\/\*[\s\S]+?\*\//gm, ''))
    .pipe(replace(/('use strict'|"use strict");?/gm, ''))
    .pipe(replace(/[\n]{2,}/gm, '\n\n'))
    .pipe(beautify({indent_size: 2}))
    .pipe(header(banner + '\'use strict\';\n', { pkg : pkg, years: years } ))
    .pipe(gulp.dest('./dist/'))
    .pipe(gulp.dest('../orb-gh-pages/js/orb/'));
  };

  return bundle();
});

gulp.task('minify', ['debug'], function() {

  gulp.src('./dist/' + getBundleName() + '.js')
  .pipe(sourcemaps.init({loadMaps: true}))
      // Add transformation tasks to the pipeline here.
      .pipe(uglify({output: {ascii_only: true}}))
      .pipe(header(banner, { pkg : pkg, years: years } ))
      .pipe(rename(getBundleName() + '.min.js'))
      .pipe(sourcemaps.write('./'))
      .pipe(gulp.dest('./dist/'))
      .pipe(gulp.dest('../orb-gh-pages/js/orb/'));
    });

gulp.task('default', ['less', 'react', 'debug', 'minify']);