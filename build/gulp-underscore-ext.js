// through2 is a thin wrapper around node transform streams
var through = require('../node_modules/through2');
var _ext = require('./underscore-ext');

var gutil = require('../node_modules/gulp-util');
var PluginError = gutil.PluginError;

var PLUGIN_NAME = 'gulp-underscore-ext';

function render(templateText, context, returnStream) {
  var templateBuffer;
  try{
  templateBuffer = new Buffer(_ext.template(templateText, context)(context));
} catch(e) {
  console.log('templateText: ' + templateText);
  throw e;
}

  if(returnStream) {
    var stream = through();
    stream.write(templateBuffer);
    return stream;
  } else {
    return templateBuffer;
  }
}

// Plugin level function(dealing with files)
function gulpUnderscoreExt(context) {

  // Creating a stream through which each file will pass
  return through.obj(function(file, enc, cb) {
    if (file.isNull()) {
      // return empty file
      cb(null, file);
    }
    if (file.isBuffer()) {
      file.contents = render(String(file.contents), context);
    }
    if (file.isStream()) {
      file.contents = file.contents.pipe(render(String(file.contents), context, true));
    }

    cb(null, file);

  });

}

// Exporting the plugin main function
module.exports = gulpUnderscoreExt;