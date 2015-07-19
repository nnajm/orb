/** @jsx React.DOM */

/* global module, require, React, window */
/*jshint node: true*/

'use strict';

var react = typeof window === 'undefined' ? require('react') : window.React;
var utils = require('../orb.utils');
var axe = require('../orb.axe');
var uiheaders = require('../orb.ui.header');
var filtering = require('../orb.filtering');
var domUtils = require('../orb.utils.dom');

var extraCol = 0;
var comps = module.exports;
