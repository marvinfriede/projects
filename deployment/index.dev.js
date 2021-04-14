// webpack.config.js

"use strict";

const HtmlWebpackPlugin = require("html-webpack-plugin");
const path = require('path');

module.exports = {
  mode: "development",
  entry: {
    main: path.resolve(__dirname, "src/js/main.js"),
  },
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "[name].js",
  },
  devtool: "inline-source-map",
  module: {
    rules: [
      {
        test: /\.html$/,
        use: ["html-loader"],
      },
      {
        test: /\.css$/,
        use: [
          //MiniCssExtractPlugin.loader, // 2. extract css into files
          "style-loader", // 2. inject styles into DOM
          "css-loader", // 1. translate css into commonjs
        ],
      },
      {
        test: /\.(svg|png|jpg|gif)$/,
        use: {
          loader: "file-loader",
          options: {
            name: "[name].[hash].[ext]",
            outputPath: "img",
          },
        },
      },
      {
        test: /\.less$/i,
        use: [
          // MiniCssExtractPlugin.loader, // 3. extract css into files
          "style-loader", // 3. inject styles into DOM
          "css-loader", // 2. turn css into commonjs
          "less-loader" // 1. turn less into css
        ],
      },
    ],
  },
  plugins: [
    new HtmlWebpackPlugin({
      filename: "./index.html",
      template: path.resolve(__dirname, "src/index.raw.html"),
      inject: "head",
      scriptLoading: "defer",
    }),
  ],
  // By default webpack logs warnings if the bundle is bigger than 200kb.
  performance: { hints: false },
  watch: true,
  watchOptions: {
    ignored: /node_modules/
  },
};
