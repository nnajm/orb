// through2 is a thin wrapper around node transform streams
var through = require('../node_modules/through2');
var remarkable = require('./remarkable-ext')({
  html: true,
  breaks: false
});

var gutil = require('../node_modules/gulp-util');
var PluginError = gutil.PluginError;

var PLUGIN_NAME = 'gulp-remarkable-ext';

function render(templateText, returnStream) {
  var templateBuffer = new Buffer(remarkable.render(templateText));

  if(returnStream) {
    var stream = through();
    stream.write(templateBuffer);
    return stream;
  } else {
    return templateBuffer;
  }
}

// Plugin level function(dealing with files)
function gulpRemarkableExt() {

  // Creating a stream through which each file will pass
  return through.obj(function(file, enc, cb) {
    if (file.isNull()) {
      // return empty file
      cb(null, file);
    }
    if (file.isBuffer()) {
      file.contents = render(String(file.contents));
    }
    if (file.isStream()) {
      file.contents = file.contents.pipe(render(String(file.contents), true));
    }

    cb(null, file);

  });

}

// Exporting the plugin main function
module.exports = gulpRemarkableExt;