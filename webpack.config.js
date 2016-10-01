'use strict';

var config
  = { entry: './src/main'
    , debug: true
    , devtool: 'source-map'
    , devServer: { contentBase: '.'
                 , port: 4008
                 , stats: 'errors-only'
                 }
    , output: { path: __dirname
              , pathinfo: true
              , filename: 'index.js'
              }
    , module: { loaders: [ { test: /\.purs$/
                           , loader: 'purs-loader'
                           , query: { src: [ 'bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs' ]
                                    , bundle: false
                                    , psc: 'psa'
                                    , pscArgs: { sourceMaps: true }
                                    , pscIde: true
                                    }
                           }
                         , { test: /\.js$/
                           , loader: 'source-map-loader'
                           , exclude: /node_modules|bower_components/
                           }
                         ]
              }
    , resolve: { modulesDirectories: [ 'node_modules', 'bower_components' ]
               , extensions: [ '', '.purs', '.js']
               }
    }
    ;

module.exports = config;
