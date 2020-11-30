let path = require('path')
let HtmlWebpackPlugin = require('html-webpack-plugin')
let webpack = require('webpack')
let WasmPackPlugin = require('@wasm-tool/wasm-pack-plugin')
let { watchDirectories } = require('../common')

module.exports = {
  entry: './index.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'index.js',
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: './index.html',
    }),
    new WasmPackPlugin({
      watchDirectories: watchDirectories(__dirname),
      crateDirectory: path.resolve(__dirname, '.'),
    }),
  ],
  mode: 'development',
}
