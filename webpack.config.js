const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const webpack = require('webpack'); //to access built-in plugins

module.exports = {
  entry: {
    index: './src/index.js',
    blog: './src/blog.js'
  },
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: '[name].js'
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        exclude: /node_modules/,
        use: [
          {
            loader: 'style-loader',
          },
          {
            loader: 'css-loader',
            options: {
              importLoaders: 1,
            }
          },
          {
            loader: 'postcss-loader'
          }
        ]
      }
    ]
  },
  devServer: {
    watchContentBase: true,
    contentBase: path.resolve(__dirname, 'dist'),
    open: true
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: './dist/indexT.html',
      inject: true,
      chunks: ['index'],
      filename: './dist/indexT.html'
    }),
    new HtmlWebpackPlugin({
      template: './dist/index.html',
      inject: true,
      chunks: ['index'],
      filename: './dist/index.html'
    }),
    new HtmlWebpackPlugin({
      template: './dist/blog.html',
      inject: true,
      chunks: ['index'],
      filename: './dist/blog.html'
    }),
    new HtmlWebpackPlugin({
      template: './dist/blogT.html',
      inject: true,
      chunks: ['index'],
      filename: './dist/blogT.html'
    }),
    new webpack.ProvidePlugin({
      $: 'jquery',
      jQuery: 'jquery'
    })
  ]
}
