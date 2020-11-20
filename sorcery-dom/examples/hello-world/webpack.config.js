let path = require('path')
let HtmlWebpackPlugin = require('html-webpack-plugin')
let webpack = require('webpack')
let WasmPackPlugin = require('@wasm-tool/wasm-pack-plugin')

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
      watchDirectories: [
        path.resolve(__dirname, 'src'),
        path.resolve(__dirname, '..', '..', '..', 'sorcery', 'src'),
        path.resolve(__dirname, '..', '..', '..', 'sorcery-macros', 'src'),
        path.resolve(__dirname, '..', '..', '..', 'sorcery-rsx', 'src'),
        path.resolve(__dirname, '..', '..', '..', 'sorcery-dom', 'src'),
        path.resolve(__dirname, '..', '..', '..', 'sorcery-reconciler', 'src'),
      ],
      crateDirectory: path.resolve(__dirname, '.'),
    }),
  ],
  mode: 'development',
}
