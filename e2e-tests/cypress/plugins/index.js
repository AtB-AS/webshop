// ***********************************************************
// This example plugins/index.js can be used to load plugins
//
// You can change the location of this file or turn off loading
// the plugins file with the 'pluginsFile' configuration option.
//
// You can read more here:
// https://on.cypress.io/plugins-guide
// ***********************************************************

// This function is called when a project is opened or re-opened (e.g. due to
// the project's config changing)

//module.exports = (on, config) => {
// `on` is used to hook into various events Cypress emits
// `config` is the resolved Cypress config
//}

//TODO https://www.npmjs.com/package/cypress-fail-fast#configuration-by-test
// -> Not working in headed mode at the moment

module.exports = (on, config) => {
    on('task', {
        log(message) {
            console.log(message);

            return null;
        },
        table(message) {
            console.table(message);

            return null;
        }
    });

    //NEW
    // important: return the changed config
    return config;
};

/// <reference types="cypress" />
const createEmailAccount = require('./email-account.js');

module.exports = async (on, config) => {
    const emailAccount = await createEmailAccount();

    on('task', {
        getUserEmail() {
            return emailAccount.email;
        },
        getUserPw() {
            return emailAccount.pass;
        },
        getLastEmail() {
            return emailAccount.getLastEmail();
        }
    });

    // important: return the changed config
    return config;
};
