const webpack = require('webpack')
const WebpackDevServer = require('webpack-dev-server')
const config = require('./webpack.config')

new WebpackDevServer(webpack(config), {
  publicPath: config.output.publicPath,
  hot: true,
  historyApiFallback: true
}).listen(4000, 'localhost', (err, result) => {
  if (err) {
    return console.log(err)
  }
  console.log('Listening at http://localhost:4000/')
})
