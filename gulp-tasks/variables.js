var pathRoot = './',
    pkg = require(pathRoot + '../package.json'),
    year = new Date().getFullYear(),
    libName = 'orb',
    libDistFolder = pathRoot + 'dist/',
    websiteDistFolder = pathRoot + '../orb-gh-pages/';

module.exports = {
    pkg: pkg,
    years: '2014' + (year > 2014 ? '-' + year : ''),
    banner:
    '/**\n' +
    ' * <%= pkg.name %> v<%= pkg.version %>, <%= pkg.description %>.\n' +
    ' *\n' +
    ' * Copyright (c) <%= years %> <%= pkg.author %>.\n' +
    ' *\n' +
    ' * @version v<%= pkg.version %>\n' +
    ' * @link <%= pkg.homepage %>\n' +
    ' * @license <%= pkg.license %>\n' +
    ' */\n\n',
    jshintOptions:
    '/* global module, require, define, window, document, global, React */\n' +
    '/*jshint node: true, eqnull: true*/\n\n',
    lib: {
        name: libName,
        versionedName: libName + '-' + pkg.version
    },
    dist: {
        folder: libDistFolder,
        versionedFolder: libDistFolder + 'v' + pkg.version + '/'
    },
    website: {
        folder: websiteDistFolder,
        jsFolder: websiteDistFolder + 'static/js/orb/',
        cssFolder: websiteDistFolder + 'static/css/orb/'
    }
};