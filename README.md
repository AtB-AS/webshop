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

This will launch a server at http://127.0.0.1:8000/ but this can be
configured in your `webpack.local.config.js` file.  Simply add
something like this to that file if it doesn't exist:

```js
module.exports = {
    host: '0.0.0.0',
    port: 8112,
    baseUrl: 'http://localhost'
};
```

The `baseUrl` needs to be set to be able to make calls to the backend.  As an
alternative to using a local config file if you only need to set the base URL,
you can set the `WEBSHOP_BASE_URL` environment variable.

When making changes, always format Elm files using the following `elm-format` release:

> https://github.com/mjenssen/elm-format/releases/latest

This is to ensure that `let-in` expressions are correctly indented.

## Production

To build for production, just run the `build` script:

```sh
$ yarn build
```

This will create a `dist` folder with everything, including compressed
versions of all files that are at least 1024 bytes in size.  There are
two compressed files per original file: a zopfli (.gz) file and a
brotli (.br) file.  If a file doesn't compress well, there won't be a
compressed version of it.

## License

This project is licensed under the EUPL 1.2 license ([LICENSE](LICENSE) or
[http://opensource.org/licenses/EUPL-1.2](http://opensource.org/licenses/EUPL-1.2)).
