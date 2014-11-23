module.exports = function(grunt) {

  // Project configuration.
  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    less: {
      development: {
        options: {
          paths: ["src/css"],
          cleancss: true,
          cleancssOptions: {
            keepBreaks: true
          },
        },
        files: {
          "dist/<%= pkg.name %>.debug.css": "src/css/orb.css"
        }
      },
      production: {
        options: {
          paths: ["src/css"],
          cleancss: true
        },
        files: {
          "dist/<%= pkg.name %>.min.css": "src/css/orb.css"
        }
      }
    },
    react: {
      combined_file_output: {
        files: {
          'src/js/react/orb.react.compiled.js': [
            'src/js/react/orb.react.components.js',
            'src/js/react/orb.react.dragndrop.js'
          ]
        }
      }
    },
    concat: {
      options: {
        stripBanners: true,
        banner: '/*! <%= pkg.name %> v<%= pkg.version %>, Javascript pivot grid library.\n' +
        ' *  (c) <%= pkg.author %>, <%= grunt.template.today("yyyy-mm-dd") %>.\n' +
        ' *  Licence: <%= pkg.license %>.\n' +
        ' */\n\n',
        separator: ';'
      },
      dist: {
        src: ['src/js/orb.js', 'src/js/orb.config.js', 'src/js/orb.dimension.js',
              'src/js/orb.axe.js', 'src/js/orb.pgrid.js', 'src/js/orb.ui.header.js',
              'src/js/orb.ui.rows.js', 'src/js/orb.ui.cols.js', 'src/js/orb.ui.pgridwidget.js',
              'src/js/react/orb.react.compiled.js'],
        dest: 'dist/<%= pkg.name %>.debug.js',
      },
    },
    uglify: {
      options: {
        banner: '/*! <%= pkg.name %> v<%= pkg.version %>, Javascript pivot grid library.\n' +
                ' *  (c) <%= pkg.author %>, <%= grunt.template.today("yyyy-mm-dd") %>.\n' +
                ' *  Licence: <%= pkg.license %>.\n' +
                ' */\n\n',
        ASCIIOnly: true
      },
      build: {
        src: 'dist/<%= pkg.name %>.debug.js',
        dest: 'dist/<%= pkg.name %>.min.js'
      }
    },
    copy: {
      main: {
        files: [
          {
            src: 'dist/*',
            dest: '../orb-gh-pages/js/orb',
            flatten: true,
            expand: true
          },
          {
            src: 'lib/*',
            dest: '../orb-gh-pages/js/lib',
            flatten: true,
            expand: true
          }
        ]
      }
    }
  });

  // Load the plugin that provides the "concat" task.
  grunt.loadNpmTasks('grunt-contrib-concat');

  // Load the plugin that provides the "uglify" task.
  grunt.loadNpmTasks('grunt-contrib-uglify');

  // Load the plugin that provides the "react" task.
  grunt.loadNpmTasks('grunt-react');

  // Load the plugin that provides the "less" task.
  grunt.loadNpmTasks('grunt-contrib-less')

  // Load the plugin that provides the "copy" task.
  grunt.loadNpmTasks('grunt-contrib-copy');

  // Default task(s).
  grunt.registerTask('default', ['react', 'concat', 'uglify', 'less', 'copy']);

};