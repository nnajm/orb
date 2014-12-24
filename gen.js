var args = process.argv,
    tmpl = args.length > 2 ? args[2] : '',
    dest = args.length > 3 ? args[3] : 'out.html';

if(tmpl) {
	var _ = require('underscore');
	var fs = require('fs');
	var cwd = process.cwd() + '/';

	var _underscore_template = _.template;
	_.template = function(str, data) {
	    while (str != (str = str.replace(
	            /<%\srequire\s*(.*?)\s%>/g,
	            function(match, partialPath) {
	            	var path = cwd + partialPath;
	            	return fs.readFileSync(path).toString();
	            }
	        )));

	    return _underscore_template(str, data);
	};

	fs.writeFileSync(cwd + dest, _.template(fs.readFileSync(cwd + tmpl).toString())() );
}