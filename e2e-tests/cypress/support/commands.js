// ***********************************************
// This example commands.js shows you how to
// create the custom commands: 'createDefaultTodos'
// and 'createTodo'.
//
// The commands.js file is a great place to
// modify existing commands and create custom
// commands for use throughout your tests.
//
// You can read more about custom commands here:
// https://on.cypress.io/commands
// ***********************************************

// ***********************************************
// Helper functions
// ***********************************************

// Print summary table after CI run - used in the a11y tests
function terminalLog(violations) {
    /*
    cy.task(
        'log',
        `${violations.length} accessibility violation${
            violations.length === 1 ? '' : 's'
        } ${violations.length === 1 ? 'was' : 'were'} detected`
    )
     */
    // pluck specific keys to keep the table readable
    const violationData = violations.map(
        ({ id, impact, description, nodes }) => ({
            id,
            impact,
            description,
            nodes: nodes.length
        })
    );
    cy.task('table', violationData);
}

// ***********************************************
// Overwrite commands
// ***********************************************

// ***********************************************
// Custom commands
// ***********************************************

// Re-define 'cy.checkA11y()' with pre-set tags from 'cypress.json#env.a11yTags'
Cypress.Commands.add('a11yCheck', (context, inOptions) => {
    //Add tags
    let options = {
        runOnly: {
            type: 'tag'
            //values: ['wcag2a', 'wcag21a']
        }
        /*
        // Example of a rule
        // https://www.deque.com/axe/core-documentation/api-documentation/
        rules: {
            'color-contrast': { enabled: false },
        }
         */
    };
    let values = [];
    for (let i = 0; i < Cypress.env('a11yTags').length; i++) {
        values.push(Cypress.env('a11yTags')[i]);
    }
    options.runOnly.values = values;

    //Add rules
    if (inOptions !== null && inOptions.rules !== null) {
        options.rules = inOptions.rules;
    }
    else {
        options.rules = {}
    }

    //Added due to custom styling on radio buttons, which in reality gives two radio buttons per user choice
    //https://github.com/AtB-AS/webshop/pull/374
    options.rules["aria-hidden-focus"] = { enabled: false }

    return cy.injectAxe().then(() => {
        cy.checkA11y(
            context,
            options,
            terminalLog,
            Cypress.env('a11ySkipFailures')
        );
    });
});

//Log out if authorized
Cypress.Commands.add('visitMainAsNotAuthorized', () => {
    /*
        //Try with dark/light modes - not working
        //https://www.cypress.io/blog/2019/12/13/test-your-web-app-in-dark-mode/
        .visit("", {
            onBeforeLoad (win) {
                cy.stub(win, 'matchMedia')
                    .withArgs('(prefers-color-scheme: dark)')
                    .returns({
                        matches: true,
                    })
            },
        })
        */
    cy.visit('').wait(2000);
    cy.window().then((win) => {
        //window.localStorage.getItem("loggedIn").length
        if (window.localStorage.getItem('loggedIn') !== null) {
            console.log('*null* ' + window.localStorage.getItem('loggedIn'));
            cy.logOut();
        } else {
            cy.get('.ui-section')
                .find('button')
                .contains('neste')
                .click({ force: true })
                .wait(500);
        }
    });
});

//Log in if not authorized
Cypress.Commands.add(
    'visitMainAsAuthorized',
    (
        userEmail = Cypress.env('email'),
        userPassword = Cypress.env('password')
    ) => {
        cy.visit('').wait(2000);
        cy.window().then((win) => {
            //window.localStorage.getItem("loggedIn").length
            if (window.localStorage.getItem('loggedIn') === null) {
                console.log(
                    '*null* ' + window.localStorage.getItem('loggedIn')
                );
                cy.logIn(userEmail, userPassword);
            }
        });
        cy.get('h1.pageHeader__logo').click({force:true});
        cy.get('h2.ui-pageHeader__title').contains('Mine billetter');
        cy.get('h2.ui-pageHeader__title').should('contain', 'Mine billetter');
        // Remove validity message to avoid covering elements
        cy.get('body').then($body => {
            if ($body.has("div.validityWarning").length){
                cy.wrap($body).find('div.validityWarning').find('button').click({force: true})
            }
        })
    }
);

//Log in
Cypress.Commands.add('logIn', (userEmail, userPassword) => {
    cy.intercept(
        'POST',
        '**/identitytoolkit/v3/relyingparty/verifyPassword**'
    ).as('login');
    cy.intercept(
        'POST',
        '**/identitytoolkit/v3/relyingparty/getAccountInfo**'
    ).as('accountInfo');
    cy.intercept('POST', '**/v1/token**').as('refreshToken');

    cy.get('.ui-section')
        .find('button')
        .contains('neste')
        .click({ force: true })
        .wait(500);
    cy.get('.ui-section')
        .find('a')
        .contains('Jeg vil heller bruke e-post')
        .click();
    cy.get('#email').type(userEmail);
    cy.get('#password').type(userPassword);
    cy.wait(100);
    cy.get('button[type=submit]').contains('Logg inn').click();
    cy.wait(['@login', '@accountInfo', '@refreshToken']);
    cy.wait(200);
});

//Log out
Cypress.Commands.add('logOut', () => {
    cy.get('button.pageHeader__nav__logout').click({ force: true });
    cy.reload();
    cy.get('.ui-section')
        .find('button')
        .contains('neste')
        .click({ force: true })
        .wait(500);
    cy.get('h2.ui-section__headerTitle').should(
        'contain',
        'Velkommen til AtBs nettbutikk'
    );
});

//Buy a ticket
Cypress.Commands.add('buyTicket', () => {
    const script =
        'python3 python-scripts/buyTicket/buyTicket.py --email ' +
        Cypress.env('email') +
        ' --password ' +
        Cypress.env('password');
    cy.exec(script, { timeout: 120000 })
        .its('stdout')
        .then(($stdout) => {
            if ($stdout.includes('ORDER_ID')) {
                let refId = $stdout.replace(/^ORDER_ID:\s(.*)/g, '$1');
                console.log('Ticket refId ' + refId);
                return refId;
            } else {
                console.log('Ticket ERROR');
                return 'ERROR';
            }
        });
});
