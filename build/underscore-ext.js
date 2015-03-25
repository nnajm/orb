var _ = require('underscore');
var fs = require('fs');
var cwd = process.cwd() + '/';

var _underscore_template = _.template;
_.template = function(str, data) {
    while (str != (str = str.replace(
            /<%\s+require\s+([\s\S]+?)\s+%>/,
            function(match, partialPath) {
            	partialPath = partialPath[0] === '\'' ? partialPath.substr(1, partialPath.length - 2) : data[partialPath];
            	if(partialPath) {
	            	var path = cwd + partialPath;
	            	return fs.readFileSync(path).toString();
		        } else {
		        	return ' ';
		        }
            }
        )));

   		return _underscore_template(str, data);
};

module.exports = _;