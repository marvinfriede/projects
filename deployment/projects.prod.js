// webpack.config.js

"use strict";

const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const OptimizeCSSAssetsPlugin = require("optimize-css-assets-webpack-plugin");
const TerserJSPlugin = require("terser-webpack-plugin");
const path = require('path');

module.exports = {
  mode: "production",
  entry: {
    main: path.resolve(__dirname, "src/js/main.js"),
  },
  output: {
    path: path.resolve(__dirname, "dist"),
    filename: "[name].[contentHash].js",
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
          MiniCssExtractPlugin.loader, // 2. extract css into files
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
          MiniCssExtractPlugin.loader,
          "css-loader",
          "less-loader"
        ],
      },
    ],
  },
  optimization: {
    minimizer: [
      new TerserJSPlugin({
        terserOptions: {
          output: {
            comments: false,
          },
        },
        extractComments: false,
      }),
      new OptimizeCSSAssetsPlugin(),
      new HtmlWebpackPlugin({
        filename: "./projects.html",
        template: path.resolve(__dirname, "src/projects.raw.html"),
        inject: "head",
        minify: {
          removeAttributeQuotes: true,
          collapseWhitespace: true,
          removeComments: true,
        },
        scriptLoading: "defer",
      }),
    ],
  },
  plugins: [
    new MiniCssExtractPlugin({
      filename: "[name].[contentHash].css",
    }),
  ],
  // By default webpack logs warnings if the bundle is bigger than 200kb.
  performance: { hints: false },
};
