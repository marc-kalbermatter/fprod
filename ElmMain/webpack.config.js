var path = require("path");

module.exports = {
  module: {
    rules: [{
        test: /\.html$/,
        exclude: /node_modules/,
        type: 'asset/resource'
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: "elm-webpack-loader"
      }
    ]
  },

  devServer: {
    static: path.join(__dirname, "src"),
  },
};
