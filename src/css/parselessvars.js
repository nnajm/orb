var themeManager = require('../js/orb.themes'); 
var themecolors = themeManager.themes;

var exceptVars = ['@fieldbutton-color-bg-alpha', '@orb-overlay-color-bg'];
var overrides = {
	"white": {
		"@bordercolor": "#d9d9d9",
		"@orb-dialog-color-header-buttonclose-bg-hover": "#d9d9d9",
		"@orb-dialog-color-border": "#808080",
		"@orb-overlay-color-bg": "rgba(0, 0, 0, 0.25)",
		"@fieldbutton-color-fg": "black",
		"@fieldbutton-color-bg-alpha": "rgba(128, 128, 128, 0.1)",
		"@togglebutton-color-hover-bg": "#d9d9d9",
		"@filterbutton-color-bg": "#555555",
		"@filterbutton-color-bg-hover": "#999999",
		"@filter-container-color-shadow": "#d9d9d9",
		"@toolbar-btn-hover-border": "#808080"
	}
};

var less = require('less');

module.exports = function(themeTemplate, themejson) {

	var parsedLessVars = {
		default: parseLessVars(themejson, '')
	};

	for(var themeoverride in overrides) {
		if(overrides.hasOwnProperty(themeoverride)) {
			parsedLessVars[themeoverride] = parseLessVars(themejson, '', null, themeoverride);
		}
	}
	var colors = [];
	for(var color in themecolors) {
		if(themecolors.hasOwnProperty(color)) {
			colors.push(color);
		}
	}

	function parseLessVars(obj, ret, prefix, currtheme) {
		prefix = prefix || '';
		for(var prop in obj) {
			if(typeof obj[prop] === 'object') {
				ret = parseLessVars(obj[prop], ret, prefix + prop + '-', currtheme);
			} else {
				if(obj[prop]) {
					var lvar = '@' + prefix + prop;
					var lvalue = currtheme && overrides[currtheme] ? overrides[currtheme][lvar] : null;
					ret += lvar + ': ' + (lvalue || obj[prop]) + ';\n';
				}
			}
		}
		return ret;
	}

	var reLessvar = /@[^:]+/;
	var reLessvalue = /:\s+([^;]+)/;
	var reRemoveclass = /\/\*\s+([^\s]+)\s\*\/[\s\S]+?.c1\s\{[\s\S]+?color:\s([^;]+);[\s\S]+?\}/g;
	var reFindRgba = /(@[^:]+)\s*:\s*(rgba\([^\)]+\))/g;

	function generateTheme(colorIndex) {
		if(colorIndex < colors.length) {
			var themecolor = colors[colorIndex];

			var lessvarsarray = ('@maincolor: ' + themecolors[themecolor] + ';\n' +
								 '@orb-theme-name: orb-' + themecolor + ';\n' +
				                 (parsedLessVars[themecolor] || parsedLessVars.default)
				                ).split('\n');
			var lessvarsclean  = '';
			var lessclasses  = '';
			for(var i = 0; i < lessvarsarray.length; i++) {
				if(lessvarsarray[i]) {
					var lessvar = lessvarsarray[i].match(reLessvar)[0];
					var lessvalue = lessvarsarray[i].match(reLessvalue)[1];
					lessvarsclean += lessvarsarray[i] + '\n';
					lessclasses += '/* ' + lessvar + ' */.c1 { color: ' + lessvar + '; }\n';
				}
			}

			less.render(lessvarsclean + lessclasses, function (e, output) {

			  	var lessoutput = output.css.replace(reRemoveclass, '$1: $2;');
			  	lessoutput = lessoutput.replace(reFindRgba, function(match, varname, rgba) {
					if(exceptVars.indexOf(varname) < 0) {
						return varname + ': ' + themeManager.utils.rgbaToHex(rgba);
				  	} else {
				  		return varname + ': ' + rgba;
				  	}
				});

				less.render(lessoutput + '\n' + themeTemplate, function(err, out) {
					result += '\n/* ' + themecolor + ' */\n\n' + out.css;
				});

				generateTheme(colorIndex + 1, result);
			});
		}
	}

	var result = '';
	generateTheme(0, '');
	return result;
};