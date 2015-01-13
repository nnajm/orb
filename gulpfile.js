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
var jasmine = require('gulp-jasmine');

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

var namelatest = 'orb';
var namever = namelatest + '-' + pkg.version;
var distlatest  = './dist/';
var distver = distlatest + 'v' + pkg.version + '/';
var distwebsite = '../orb-gh-pages/';
var distwebsitejs = distwebsite + 'js/orb/';
var distwebsitecss = distwebsite + 'css/orb/';

function parseLessVars(obj, ret, prefix) {
	prefix = prefix || '';
	for(var prop in obj) {
		if(typeof obj[prop] === 'object') {
			ret = parseLessVars(obj[prop], ret, prefix + prop + '-');
		} else {
			if(obj[prop]) {
				ret += '@' + prefix + prop + ': ' + obj[prop] + ';\n';
			}
		}
	}
	return ret;
}

gulp.task('less', function () {
	gulp.src(['./src/css/orb.css', './src/css/orb.theme.less', './src/css/orb.bootstrap.less'])
	.pipe(concat('orb.less'))
	// remove comments
	.pipe(replace(/\/\*[\s\S]+?\*\//gm, ''))
	// prepend less variables
	.pipe(header(parseLessVars(require('./src/css/theme.default.json'), '')))
	.pipe(less())
	// add banner
	.pipe(header(banner, { pkg : pkg, years: years } ))

	// to latest folder
	.pipe(rename(namelatest + '.css'))
	.pipe(gulp.dest(distlatest))

	// to website folder
	.pipe(gulp.dest(distwebsitecss))

	// to versioned folder
	.pipe(rename(namever + '.css'))
	.pipe(gulp.dest(distver))

	// minify
	.pipe(cleancss())

	// to latest folder
	.pipe(rename(namelatest + '.min.css'))
	.pipe(gulp.dest(distlatest))

	// to website folder
	.pipe(gulp.dest(distwebsitecss))

	// to versioned folder
	.pipe(rename(namever + '.min.css'))
	.pipe(gulp.dest(distver));

});

gulp.task('react', function() {

	gulp.src(['./src/js/react/orb.react.components.jsx', './src/js/react/orb.react.comps.filter.jsx', './src/js/react/orb.react.dragndrop.jsx'])
	.pipe(concat('orb.react.compiled.js'))
	.pipe(react())
	.pipe(beautify({indent_size: 2}))
	.pipe(gulp.dest('./src/js/react/'));
});

gulp.task('test', ['react'], function () {

    return gulp.src('test/spec/orb.query.js')
        .pipe(jasmine({
        	verbose: true
        }));
});

gulp.task('debug', ['test'], function() {

  var bundler = browserify({
    entries: ['./src/js/orb.js'],
    debug: false,
    standalone: 'orb'
  }).exclude('react');

  var bundle = function() {
    return bundler
    .bundle()
    .pipe(source(namelatest + '.js'))
    .pipe(derequire())
    .pipe(buffer())
    .pipe(replace(/\/\*[\s\S]+?\*\//gm, ''))
    .pipe(replace(/('use strict'|"use strict");?/gm, ''))
    .pipe(replace(/[\n]{2,}/gm, '\n\n'))
    .pipe(beautify({indent_size: 2}))
    .pipe(header(banner + '\'use strict\';\n', { pkg : pkg, years: years } ))

    // to latest folder
    .pipe(gulp.dest(distlatest))

    // to website folder
    .pipe(gulp.dest(distwebsitejs))

	// to versioned folder
	.pipe(rename(namever + '.js'))
	.pipe(gulp.dest(distver));
  };

  return bundle();
});

gulp.task('minify', ['debug'], function() {

	gulp.src(distlatest + namelatest + '.js')
	.pipe(sourcemaps.init({loadMaps: true}))
	// Add transformation tasks to the pipeline here.
	.pipe(uglify({output: {ascii_only: true}}))
	.pipe(header(banner, { pkg : pkg, years: years } ))

	// to latest folder
	.pipe(rename(namelatest + '.min.js'))
	.pipe(sourcemaps.write('./'))
	.pipe(gulp.dest(distlatest))

	// to website folder
	.pipe(gulp.dest(distwebsitejs))

	// to versioned folder
	.pipe(sourcemaps.init({loadMaps: true}))
	.pipe(rename(namever + '.min.js'))
	.pipe(sourcemaps.write('./'))
	.pipe(gulp.dest(distver));
});

gulp.task('default', ['less', 'react', 'test', 'debug', 'minify']);