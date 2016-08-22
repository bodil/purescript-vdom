module.exports = {
  debug: true,
  devtool: "source-map",
  entry: "./test/entry",
  output: {
    path: __dirname,
    pathinfo: true,
    filename: "app.js"
  },
  devServer: {
    port: 1337,
    contentBase: "test",
    stats: "errors-only"
  },
  module: {
    loaders: [
      {
        test: /\.purs$/,
        loader: "purs-loader",
        query: {
          bundle: false,
          pscIde: true,
          psc: "psa",
          pscArgs: {
            sourceMaps: true
          },
          src: [
            "bower_components/purescript-*/src/**/*.purs",
            "src/**/*.purs",
            "test/**/*.purs"
          ]
        }
      },
      {
        test: /\.js$/,
        loader: "source-map-loader",
        exclude: /node_modules|bower_components/
      }
    ]
  },
  resolve: {
    modulesDirectories: ["node_modules", "bower_components"],
    extensions: ["", ".purs", ".js"]
  }
};
