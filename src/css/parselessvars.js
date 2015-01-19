var themecolors = {
	red: "#C72C48",
	blue: "#268BD2",
	green: "#3A9D23",
	orange: "darkorange",
	flower: "#A74AC7",
	gray: "#808080",
	white: "#FFFFFF",
	black: "#000000"
};

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
		"@filter-container-color-shadow": "#d9d9d9"
	}
};

//var through = require('through2');
var less = require('less');

module.exports = function(themeTemplate, themejson) {

	//return through.obj(function(file, enc, cb) {

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
		var reFindRgba = /(@[^:]+\s*:\s*rgba\([^\)]+\))/g;
		var reRgbaChannels = /(@[^:]+):\s*rgba\((\d+)\s*,\s*(\d+)\s*,\s*(\d+)\s*,\s*(\d+(?:\.\d+)?)\s*\)/;

		function channelAlphaToHex(opacity, channelValue) {
			return (Math.floor(opacity*parseInt(channelValue) + (1-opacity)*255) + 256).toString(16).substr(1,2);
		}

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
				  	lessoutput = lessoutput.replace(reFindRgba, function(match, rgba) {
						var matches = rgba.match(reRgbaChannels);		
						if(exceptVars.indexOf(matches[1]) < 0) {
							var opacity = parseFloat(matches[5]);
							return matches[1] + ': #' +
								channelAlphaToHex(opacity, matches[2]) +
								channelAlphaToHex(opacity, matches[3]) +
								channelAlphaToHex(opacity, matches[4]);
					  	} else {
					  		return rgba;
					  	}
					});

					less.render(lessoutput + '\n' + themeTemplate, function(err, out) {
						result += '\n/* ' + themecolor + ' */\n\n' + out.css;
					});

					generateTheme(colorIndex + 1, result);
				});
			} else {
		        /*if (file.isBuffer()) {
		            file.contents = Buffer.concat([
		                new Buffer(result),
		                file.contents
		            ]);
		        }

		        if (file.isStream()) {
		            var stream = through();
		            stream.write(new Buffer(result));
		            stream.on('error', this.emit.bind(this, 'error'));
		            file.contents = file.contents.pipe(stream);
		        }

		        // make sure the file goes through the next gulp plugin
		        this.push(file);
		        // tell the stream engine that we are done with this file
		        cb();*/
			}
		}

		var result = '';
		generateTheme(0, '');
		return result;
	//});
};

//module.exports(require('fs').readFileSync('./orb.theme.less', 'utf8'), require('./theme.default.json'))