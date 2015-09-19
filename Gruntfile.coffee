fs            = require('fs')
{spawn, exec} = require('child_process')

module.exports = (grunt) ->
  grunt.initConfig
    pkg:         grunt.file.readJSON "package.json"

    #### Task to compile less files
    less:
      main:
        files: [
          { src: [ "src/main.less" ]
          , dest: "main.css" }
        ]

    jade:
      main:
        files: [
          { src: [ "src/index.jade" ]
          , dest: "index.html" }
        ]

    coffee:
      main:
        files: [
          { src: [ "src/main.coffee" ]
          , dest: "main.js" }
        ]


    #### Task to watch files
    watch:
      files: ["src/*"]
      tasks: ["less", "jade", "coffee"]

  #### Load third party tasks
  grunt.loadNpmTasks 'grunt-contrib-jade'
  grunt.loadNpmTasks 'grunt-contrib-less'
  grunt.loadNpmTasks 'grunt-contrib-watch'
  grunt.loadNpmTasks 'grunt-contrib-coffee'

  grunt.registerTask 'server', "Run server", () ->
    done = @async()
    server = spawn("python", ["-m", "SimpleHTTPServer", "8182"])
    done(true)

    server.stdout.on 'data', (data) ->
      grunt.log.writeln data
    server.stderr.on 'data', (data) ->
      grunt.log.writeln "[Log]: #{data}"
    server.on 'close', (code) ->
      grunt.log.writeln "Server exited with code #{code}."

  #### Define tasks
  grunt.registerTask 'default', [
    'less'
    'jade'
    'coffee'
    'server'
    'watch'
  ]
