const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: './src/index.js',
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'bundle.js'
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
    })
  ]
}
