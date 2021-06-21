# E2E-tests webshop

The e2e-tests are run with [Cypress](https://on.cypress.io). Cypress is a node module that runs tests in the context of 
a browser, which ables control over all requests coming in and going out.

## Install

**Dependencies**

Cypress is installed through the npm package manager, along with other npm modules for a11y-testing (axe) and reporting.
```bash
"devDependencies": {
    "axe-core": "4.2.2",
    "cypress": "7.1.0",
    "cypress-axe": "0.12.2",
    "mocha": "8.3.2",
    "mocha-multi-reporters": "1.5.1",
    "mocha-junit-reporter": "2.0.0",
    "mochawesome": "6.2.2",
    "mochawesome-report-generator": "5.2.0",
    "prettier": "2.2.1"
  }
```

**Install**
```bash
## install node_modules
npm install

## run Cypress tests (headless)
./node_modules/.bin/cypress run

## run Cypress tests (headless) with mochawesome-report and junit outputs
./node_modules/.bin/cypress run --reporter mocha-multi-reporters --reporter-options configFile=cypress-reporter-config.json

## start Cypress UI
./node_modules/.bin/cypress open -d
```

## Generer HTML-testrapport

Running Cypress with the reporter options (see above), creates a result file per _e2e_spec_ file. By using [`mochawesome`](https://www.npmjs.com/package/mochawesome)
and a js-script, these specs can be combined and converted to an HTML test report. The js-script is based on [`cypress-mochawesome-s3`](https://github.com/testdrivenio/cypress-mochawesome-s3).

```bash
## combine mochawesome reports per spec file into one
node cypress/scripts/combineTestReports.js

## create an HTML report of the combined mochawesome spec file
node_modules/.bin/marge cypress/results/cypressTestReport.json --code false --reportFilename cypressTestReport --reportDir ./cypress/results
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
