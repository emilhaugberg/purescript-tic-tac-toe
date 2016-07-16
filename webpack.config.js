var path = require('path')

module.exports = {
  entry: './public/index.js',
  output: {
    path: path.join(__dirname, 'public'),
    filename: 'bundle.js'
  },
  module: {
    loaders: [
          { test: /\.css$/, loader: 'style!css' },
          { test: /\.purs$/,
            loader: 'purs-loader',
            exclude: /node_modules/,
            query: {
              psc: 'psa',
              src: ['bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs'],
              ffi: ['bower_components/purescript-*/src/**/*.js']
            }
          }
    ]
  }
}
