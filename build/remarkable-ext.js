
var includePlugin = function(remarkable, opts) {
	var pluginId = 'include';

	var parse = function (state, silent) {
	  var matches = /^%\[([^\n\]]+)\]%(?:\n+|$)/.exec(state.src.slice(state.pos));
	  if(!matches) { return false; }

	  if (silent) { return false; } // stop if silent

	  state.pos += matches[0].length;
	  state.push({
		  type : pluginId,
		  level : state.level,
		  path : (matches[1] || '').trim(),
	  });
	  return true;
	};

	var render = function (tokens, idx, options /*, env */) {
	  var fs = require('fs');
	  var incSrc = fs.readFileSync((options.includeBasePath || '') + tokens[idx].path).toString();
	  return remarkable.render(incSrc);
	};

	remarkable.inline.ruler.push(pluginId, parse);
	remarkable.renderer.rules[pluginId] = render;
};

var textWithClassPlugin = function(remarkable, opts) {

	var pluginId = 'classname';

	var parse = function (state, silent) {
	  var matches = /\%([Lls])(?:\[([^\]]+)\])?\(([^\)]+)\)/.exec(state.src.slice(state.pos));
	  if(!matches) { return false; }

	  if (silent) { return false; } // stop if silent

	  state.pos += matches[0].length;
	  state.push({
		  type : pluginId,
		  level : state.level,
		  cat : (matches[1] || '').trim(),
		  opt : (matches[2] || '').trim(),
		  text : (matches[3] || '').trim(),
		  //classname : (matches[1] || '').trim(),
	  });
	  return true;
	};

	var render = function (tokens, idx, options /*, env */) {
		var tok = tokens[idx];

		var linkName = (tok.opt + tok.text.replace(/[\.\s]+/g, '_')).toLowerCase();

		if(tok.cat == 'l') {
			return '<a name="' + linkName + '" class="anchor"></a><a href="#' + linkName + '" class="prop-name">' + tok.text + '</a>';
		} else if(tok.cat == 'L') {
			return tok.text + '<a name="' + linkName + '" class="anchor"></a><a href="#' + linkName + '" class="link-after"></a>';
		} else if(tok.cat == 's') {
			return '<small>' + tok.text + '</small>';
		} else {
			return tok.text;
		}
	};

	remarkable.inline.ruler.push(pluginId, parse);
	remarkable.renderer.rules[pluginId] = render;

};

module.exports = function(configName, options) {
	var Remarkable = require('remarkable');
	return new Remarkable(configName, options)
		   .use(includePlugin)
		   .use(textWithClassPlugin);
};