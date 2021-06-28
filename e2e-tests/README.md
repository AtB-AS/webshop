# E2E-tests webshop

The e2e-tests are run with [Cypress](https://on.cypress.io). Cypress is a node module that runs tests in the context of 
a browser, which ables control over all requests coming in and going out.

## Install

**Dependencies**

Cypress is installed as a node module with yarn (for other dependencies, see `package.json`)
```bash
"devDependencies": {
    ...
    "cypress": "7.1.0",
    ...
  }
```

**Install**
```bash
## install Cypress and dependencies
yarn install

## install Cypress and dependencies (from root)
yarn --cwd e2e-tests install
```

## Run commands
If running the commands from root, add `---cwd e2e-tests` to the yarn command.
```bash
## run Cypress tests (headless)
yarn cypress:run

## run Cypress tests (headless) with mochawesome-report and junit outputs
yarn cypress:run_report

## start Cypress UI
yarn cypress:open
```

## Generer HTML-testrapport

Running Cypress with the reporter options (see above), creates a result file per _e2e_spec_ file. By using [`mochawesome`](https://www.npmjs.com/package/mochawesome)
and a js-script, these specs can be combined and converted to an HTML test report. The js-script is based on [`cypress-mochawesome-s3`](https://github.com/testdrivenio/cypress-mochawesome-s3).

```bash
## combine mochawesome reports per spec file into one + create an HTML-report
yarn cypress:html-testreport
```

## a11y tests

For a11y tests, [Axe](https://github.com/dequelabs/axe-core) is used. Axe is an accessibility testing engine for websites
and other HTML-based user interfaces. Axe is used for several websites and is an underlying technology in different
products. There are different tags that set up what Axe should check the website for. These tags are set in the
`cypress.json` configuration file. For now, these tags are used

```json
"env": {
  "a11yTags": ["wcag2a", "wcag2aa"],
  "a11ySkipFailures": false,
  ...
}
```
Axe also supports WCAG 2.1 A and WCAG 2.1 AA, which will be included later.

## Resources

* [Cypress Docs](https://on.cypress.io)
* [Cypress CLI Tool Docs](https://github.com/cypress-io/cypress-cli)
* [Cypress and AXE](https://www.npmjs.com/package/cypress-axe)
* [AXE documentation](https://www.deque.com/axe/core-documentation/api-documentation/)
