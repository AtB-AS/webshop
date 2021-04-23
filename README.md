# AtB Webshop

The frontend is written in Elm.  It currently uses Elm 0.19.1.

## Setting up the environment

It's recommended to use `yarn`:

```sh
$ yarn
```

This will install all dependencies.

## Development

Once your environment is up and running, start the dev server:

```sh
$ yarn start
```

By default, this will launch a development server at http://127.0.0.1:8000/ but
this can be configured (see Configuration below)

When making changes, always format Elm files using the following `elm-format`
release:

> https://github.com/mjenssen/elm-format/releases/latest

This is to ensure that `let-in` expressions are correctly indented.

## Production

To build for production, just run the `build` script:

```sh
$ yarn build
```

You will need to configure base URL and Firebase config (see Configuration
below).

This will create a `dist` folder with everything, including compressed versions
of all files that are at least 1024 bytes in size.  There are two compressed
files per original file: a zopfli (.gz) file and a brotli (.br) file.  If a file
doesn't compress well, there won't be a compressed version of it.

## Configuration

There are two ways to configure the environment.  For basic configuration, you
can use environment variables:

- `WEBSHOP_BASE_URL`: The base URL to the backend.
- `WEBSHOP_FIREBASE_CONFIG`: The Firebase config.  The config needs to be valid
  JSON, so remember when copying the config from the Firebase console to change
  the keys to be double-quoted strings (this is only necessary when using an
  environment variable).

There is also support for using a `.env` file containing these variables.

For more advanced configuration needs, you can use a local webpack config file,
named `webpack.local.config.js`.  This is a normal JavaScript file so you can do
anything you want here, like importing other modules or using code to create the
configuration.

A minimum configuration file should contain the following:

```js
module.exports = {
    host: '0.0.0.0',
    port: 8112,
    baseUrl: 'http://localhost',
    firebaseConfig: {...}
};
```

- `host`: The hostname or IP address the development server will listen on.
- `port`: The port the development server will listen on.
- `baseUrl`: The base URL to the backend.
- `firebaseConfig`: The Firebase config.  Unlike environment variables, you can
  just copy the value from the Firebase console.
- `proxy` (optional): This can be set to configure Webpack's proxy system.  Only
  relevant when running the development server.

## License

This project is licensed under the EUPL 1.2 license ([LICENSE](LICENSE) or
[http://opensource.org/licenses/EUPL-1.2](http://opensource.org/licenses/EUPL-1.2)).
