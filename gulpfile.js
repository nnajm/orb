/* global require */

'use strict';

var gulp = require('gulp'),
    runSequence = require('run-sequence');

gulp.task('clean', require('./gulp-tasks/clean'));
gulp.task('test', require('./gulp-tasks/test'));
gulp.task('buildcss', require('./gulp-tasks/buildcss'));
gulp.task('eslint', require('./gulp-tasks/eslint'));
gulp.task('buildjs', require('./gulp-tasks/buildjs'));
gulp.task('minify', require('./gulp-tasks/minify'));

gulp.task('devbuild', function(callback) {
  runSequence('clean',
             'buildcss', 
             'buildjs',
              callback);
});

// This will run in this order:
// * clean/test/eslint in parallel
// * buildcss/buildjs in parallel
// * minify
gulp.task('fullbuild', function(callback) {
  runSequence(['clean', 'test', 'eslint'],
              ['buildcss', 'buildjs'],
              'minify',
              callback);
});