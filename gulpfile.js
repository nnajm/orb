/* global require */
/*jshint eqnull: true*/

/* dev deps:
   ---------
	gulp
	gulp-rename
	gulp-concat
	gulp-prettify
	gulp-util
	through2
	underscore
	remarkable
*/

'use strict';

var gulp = require('gulp');
var rename = require('gulp-rename');
var concat = require('gulp-concat');
var prettify = require('gulp-prettify');
var remarkable = require('./build/gulp-remarkable-ext');
var _ext = require('./build/gulp-underscore-ext');
var version = require('../orb/package.json').version;

var templatesFolder = 'templates/';

var pages = {
	HOME: {
		template: templatesFolder + 'home/home.md',
		context: {
			page: 'home',
			showMenu: false,
			menuFilepath: ''
		},
		html: 'index.html'
	}, 
	DOC_OVERVIEW: {
		template: templatesFolder + 'doc/overview.md',
		context: {
			page: 'doc_overview',
			showMenu: true,
			menuFilepath: './templates/doc/overview_menu.md'
		},
		html: 'doc-overview.html'
	},
	DOC_PGRID_WIDGET: {
		template: templatesFolder + 'doc/pgridwidget.md',
		context: {
			page: 'doc_pgridwidget',
			showMenu: true,
			menuFilepath: './templates/doc/pgridwidget_menu.html'
		},
		html: 'doc-pgridwidget.html'
	},
	DOWNLOADS: {
		template: templatesFolder + 'downloads/downloads.md',
		context: {
			page: 'downloads',
			showMenu: false,
			menuFilepath: '',
			orb_version: version
		},
		html: 'downloads.html'
	}
};

var tasks = [];

function buildPage(pagename) {
	var page = pages[pagename];
	var taskname = 'build_' + pagename;

	tasks.push(taskname);

	gulp.task(taskname, function () {
	    return gulp.src(page.template)
	    	.pipe(_ext(page.context))
	        .pipe(remarkable())
	        .pipe(concat(page.html))
	        .pipe(prettify())
	        .pipe(gulp.dest(page.dest || '.'));
	});
}

for(var pagename in pages) {
	buildPage(pagename);
}

gulp.task('default', tasks);