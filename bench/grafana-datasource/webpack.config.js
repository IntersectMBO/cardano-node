// @ts-check
'use strict';

const path = require('path');

/** @type {import('webpack').Configuration} */
module.exports = {
  entry: {
    module: path.resolve(__dirname, 'src/module.ts'),
  },
  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: '[name].js',
    // AMD is the module format Grafana uses to load frontend plugins.
    libraryTarget: 'amd',
    publicPath: 'public/plugins/iog-cardanotimeseries-datasource/',
  },
  resolve: {
    extensions: ['.ts', '.tsx', '.js', '.jsx'],
    modules: [path.resolve(__dirname, 'src'), 'node_modules'],
  },
  module: {
    rules: [
      {
        test: /\.tsx?$/,
        exclude: /node_modules/,
        use: {
          loader: 'ts-loader',
          options: { transpileOnly: true },
        },
      },
    ],
  },
  // These packages are provided by Grafana at runtime; do not bundle them.
  externals: [
    function ({ request }, callback) {
      const grafanaExternals = [
        '@grafana/data',
        '@grafana/runtime',
        '@grafana/ui',
        '@grafana/schema',
        'react',
        'react-dom',
        'rxjs',
        'rxjs/operators',
        'lodash',
        'moment',
        'jquery',
      ];
      if (grafanaExternals.some((ext) => request === ext || request?.startsWith(ext + '/'))) {
        return callback(null, `amd ${request}`);
      }
      callback();
    },
  ],
};
