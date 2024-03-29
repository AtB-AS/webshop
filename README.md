# AtB Webshop

The frontend is written in Elm. It currently uses Elm 0.19.1.

## Setting up the environment

It's recommended to use `yarn`:

```sh
$ yarn
```

This will install all dependencies.

## Development

Before running the dev server, organization specific assets needs to be copied into the static folder:

```sh
$ WEBSHOP_ORG_ID=atb yarn setup
```

Current organizations supported is `atb` and `nfk`, details can be found in the [organization support](#Organization-support) section.

Once your environment is up and running, start the dev server:

```sh
$ yarn start
```

By default, this will launch a development server at http://127.0.0.1:8000/ but
this can be configured (see Configuration below)

When making changes, always format Elm files using the following `elm-format`
release:

> https://github.com/mjenssen/elm-format/releases/latest

It can also be installed with `yarn`:

```sh
$ yarn global add @atb-as/elm-format
```

This is to ensure that `let-in` expressions are correctly indented.

### Testing ticketing

[See docs at App](https://github.com/AtB-AS/mittatb-app/blob/master/docs/TicketingQA.md)

## Production

To build for production, just run the `build` script:

```sh
$ yarn build
```

You will need to configure base URL and Firebase config (see Configuration
below). If building for other organizations than AtB, you will need to replace
the `yarn setup atb` for `"build"` in `package.json` into `yarn setup <your-organization>`.

This will create a `dist` folder with everything, including compressed versions
of all files that are at least 1024 bytes in size. There are two compressed
files per original file: a zopfli (.gz) file and a brotli (.br) file. If a file
doesn't compress well, there won't be a compressed version of it.

## Configuration

There are two ways to configure the environment. For basic configuration, you
can use environment variables:

-   `WEBSHOP_ORG_ID`: OrgId (either atb or nfk).
-   `WEBSHOP_BASE_URL`: The base URL to the backend.
-   `WEBSHOP_FIREBASE_CONFIG`: The Firebase config. The config needs to be valid
    JSON, so remember when copying the config from the Firebase console to change
    the keys to be double-quoted strings (this is only necessary when using an
    environment variable).
-   `GA_TRACKING_ID`: Optional Google Analytics code.

There is also support for using a `.env` file containing these variables.

For more advanced configuration needs, you can use a local webpack config file,
named `webpack.local.config.js`. This is a normal JavaScript file so you can do
anything you want here, like importing other modules or using code to create the
configuration.

A minimum configuration file should contain the following:

```js
module.exports = {
    orgId: 'atb',
    host: '0.0.0.0',
    port: 8112,
    baseUrl: 'http://localhost',
    firebaseConfig: {...},
    gaTrackingId: '' // optional Google Analytics code
};
```

-   `orgId`: Active organization. Either atb or nfk.
-   `host`: The hostname or IP address the development server will listen on.
-   `port`: The port the development server will listen on.
-   `baseUrl`: The base URL to the backend.
-   `firebaseConfig`: The Firebase config. Unlike environment variables, you can
    just copy the value from the Firebase console.
-   `proxy` (optional): This can be set to configure Webpack's proxy system. Only
    relevant when running the development server.

## Organization support

This webshop is currently being customized for usage in the OOS collaboration featuring Entur, AtB, Nordland County Council (NFK / Reis), Møre og Romsdal County Council (FRAM), and Agder Kollektivtrafikk (AKT).
This means that the app needs a bit of configuration to work in a organization context outside of AtB.

Currently the app does not support dynamic text for each organization.
This means that all text throughout the app has to be manually changed to match the requirements of an organization.
Currently, this has been handled for NFK by creating a separate branch in this Git repository, but a fork should also be possible.
In a later release, text is planned to be handled by the same functionality that handles languages.

For development, `webpack.local.config.js` should export `orgId: 'atb' | 'nfk',` to set siteTitle and the options provided in the `orgs/<organization>.json` file.

Assets such as icons, logos, and illustrations are copied into the static folder of the application when the application in build time.
For development assets can be copied manually by running `WEBSHOP_ORG_ID=atb yarn setup` in the terminal. This is also triggered on CIs.

See [`@atb-as/generate-assets`](https://github.com/AtB-AS/design-system/tree/main/packages/assets) in design system for more details. Assets will be outputted in `./src/static/assets`.

A file called `<initials_for_organization>.json` should be placed directly in the `orgs` folder containing basic information such as `"orgId": "atb",` and `"siteTitle": "AtB Nettbutikk"`.
URLs for assets such as privacy declarations and zone maps should also be included in this file, see `/orgs/atb.json` for necessary fields.

Below is an example of a file structure containing AtB and NFK.

```
webshop/
├─ src/
├─ orgs/
   ├─ atb.json
   ├─ nfk.json
```


## Release

The webshop is automatically released to staging then new changes on the master branch. To release to production edit the latest release version in GitHub and select _Publish release_.

Normally we increment the minor version when releasing a new version, but if there are no visible changes and only small fixes then the patch version may be incremented. Bump as major version if there are any changes that require the release of webshop-backend or any other dependency.

## Verification of a new release

Quality assurance of new features, improvements and bug fixes have defined process, which is described more [here](https://github.com/AtB-AS/org/blob/master/guides/quality_assurance.md#qa-in-atb-webshop).

After a new deployment to production, there are some steps that should be followed to ensure the deployment and its integrations:
- Log in with both phone and e-mail
- Add and remove a travelcard
- Verify the chat possibility (i.e. Intercom)
- Buy a new periodic ticket with a payment card (and save this card)
    - Ensure to choose a high price that should trigger SCA
- Buy new carnet card with a different payment card
    - Ensure to choose a low price that should not trigger SCA (if enabled)
- Buy product with Vipps
- Remove the saved card
- Ask for a receipt on e-mail

Lastly, try the main functionality on different browsers, e.g. Chrome, Firefox, Edge and Safari.


## License

This project is licensed under the EUPL 1.2 license ([LICENSE](LICENSE) or
[http://opensource.org/licenses/EUPL-1.2](http://opensource.org/licenses/EUPL-1.2)).
