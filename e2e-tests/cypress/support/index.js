// ***********************************************************
// This example support/index.js is processed and
// loaded automatically before your other test files.
//
// This is a great place to put global configuration and
// behavior that modifies Cypress.
//
// You can change the location of this file or turn off
// automatically serving support files with the
// 'supportFile' configuration option.
//
// You can read more here:
// https://on.cypress.io/guides/configuration#section-global
// ***********************************************************

// Import commands.js and defaults.js
// using ES2015 syntax:
import "./commands.js"
import "./defaults"

// Alternatively you can use CommonJS syntax:
//require('./commands')
//require('./defaults')

//https://www.npmjs.com/package/cypress-axe
//https://www.youtube.com/watch?v=IADSsClWVtA
//https://www.deque.com/axe/core-documentation/api-documentation/
import 'cypress-axe'


/* For TypeScript
declare namespace Cypress {
    interface Chainable {
        /**
         * Custom command to select DOM element by data-cy attribute.
         * @example cy.dataCy('greeting')
         * /
        dataCy(value: string): Chainable<Element>
    }
}
*/


