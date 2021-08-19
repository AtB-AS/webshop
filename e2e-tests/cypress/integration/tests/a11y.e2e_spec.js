import { auth } from '../pageobjects/authentication.pageobject.js';
import { menu, verify } from '../pageobjects/common.pageobject.js';
import { mytickets } from '../pageobjects/mytickets.pageobject.js';
import { myprofile } from '../pageobjects/myprofile.pageobject.js';
import { history } from '../pageobjects/history.pageobject.js';
import { newTicket, options, products, traveller, travelTime } from '../pageobjects/buyticket.pageobject.js';

/**
 * Accessibility checks using axe (https://github.com/dequelabs/axe-core)
 */

describe('a11y check authentication', () => {
    it('sms login', () => {
        cy.visitMainAsNotAuthorized();
        cy.a11yCheck(null, null);
    });
    it('email login', () => {
        cy.visitMainAsNotAuthorized();
        auth.useEmailAsLogIn();
        cy.a11yCheck(null, null);
    });
    it('email create', () => {
        cy.visitMainAsNotAuthorized();
        auth.useEmailAsLogIn();
        auth.createNewEmailProfile();
        cy.a11yCheck(null, null);
    });
});

describe('a11y check webshop overview', () => {
    after(() => {
        cy.logOut();
    });
    beforeEach(function () {
        cy.visitMainAsAuthorized();
    });

    it('overview', () => {
        cy.a11yCheck(null, null);
    });

    //Buy a ticket is configured in 'cypress.json'
    if (Cypress.env('withBuyTicket')) {
        it('overview with ticket', () => {
            let order_id = 'ERROR';
            const timeoutValue = Cypress.env('buyTicketTimeout'); //default: 10 min

            //Only to navigate back to overview after buying a ticket
            menu.myProfile().click();
            verify.verifyHeader('h2', 'Min profil');

            //Buy ticket
            cy.buyTicket()
                .then(($order_id) => {
                    //Set order_id to 'order_id' or ERROR
                    order_id = $order_id;
                })
                //Wait for ticket to appear
                .then(() => {
                    //Overview
                    menu.startPage().click();
                    verify.verifyHeader('h2', 'Mine billetter');

                    mytickets.waitForTicket(order_id, timeoutValue);
                });

            mytickets.tickets().then(($tickets) => {
                if (!order_id.includes('ERROR')) {
                    mytickets.ticket(order_id).then(($ticket) => {
                        mytickets
                            .ticketDetails($ticket)
                            .should('not.be.visible');

                        //Check for collapsed tickets
                        cy.a11yCheck(null, null);

                        mytickets.showDetails($ticket);
                        mytickets.ticketDetails($ticket).should('be.visible');

                        //Check for open ticket
                        cy.a11yCheck(null, null);
                    });
                }
            });
        });
    }
});

describe('a11y check webshop my profile', () => {
    after(() => {
        cy.logOut();
    });
    beforeEach(function () {
        cy.visitMainAsAuthorized();
    });

    it('my profile', () => {
        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');

        cy.a11yCheck(null, null);
    });

    it('my profile - change phone number', () => {
        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');
        myprofile.editPhoneNumber();

        //TODO https://github.com/AtB-AS/webshop/issues/249
        cy.a11yCheck(null, {
            rules: {
                'color-contrast': { enabled: false }
            }
        });
    });

    it('my profile - change consent', () => {
        cy.intercept('POST', '**/webshop/v1/consent').as('consent');

        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');

        myprofile.consent().check();
        cy.wait('@consent');
        cy.injectAxe().then(() => {
            cy.a11yCheck(null, null);
        });

        myprofile.consent().uncheck();
        cy.wait('@consent');
        cy.injectAxe().then(() => {
            cy.a11yCheck(null, null);
        });
    });
});

describe('a11y check webshop ticket history', () => {
    after(() => {
        cy.logOut();
    });
    beforeEach(function () {
        cy.visitMainAsAuthorized();
    });

    it('ticket history', () => {
        const order_id = 'HUCVIBHX';

        menu.history().click();
        verify.verifyHeader('h2', 'Kjøpshistorikk');
        history.ticket(order_id);

        cy.a11yCheck(null, null);
    });

    it('ticket history details', () => {
        const order_id = 'HUCVIBHX';

        menu.history().click();
        verify.verifyHeader('h2', 'Kjøpshistorikk');
        history.ticket(order_id).then(($ticket) => {
            history.showDetails($ticket);
            history.ticketIsCollapsed($ticket, false);
        });

        cy.a11yCheck(null, null);
    });
});

describe('a11y check webshop buy ticket', () => {
    after(() => {
        cy.logOut();
    });
    beforeEach(function () {
        cy.visitMainAsAuthorized();
    });

    it('period ticket', () => {
        menu.buyPeriodTicket().click();
        verify.verifyHeader('h2', 'Kjøp ny periodebillett');

        cy.a11yCheck(null, null);
    });

    it('period ticket - open categories', () => {
        menu.buyPeriodTicket().click();
        verify.verifyHeader('h2', 'Kjøp ny periodebillett');

        products.showOptions()
        newTicket.productsSection().then($product => {
            options.areVisible($product, true)
        })

        cy.a11yCheck(null, null);
    });

    it('period ticket - open travellers', () => {
        menu.buyPeriodTicket().click();
        verify.verifyHeader('h2', 'Kjøp ny periodebillett');

        traveller.showOptions()
        newTicket.travellerSection().then($traveller => {
            options.areVisible($traveller, true)
        })

        cy.a11yCheck(null, null);
    });

    it('period ticket - open time picker', () => {
        menu.buyPeriodTicket().click();
        verify.verifyHeader('h2', 'Kjøp ny periodebillett');

        //Check travel now
        travelTime.showOptions()
        newTicket.travelTimeSection().then($traveller => {
            options.areVisible($traveller, true)
        })
        cy.a11yCheck(null, null);

        //Check travel in future
        travelTime.inFuture().click();
        travelTime.date().should('be.visible');
        cy.a11yCheck(null, null);

        //Check date picker
        travelTime.date().click();
        cy.a11yCheck(null, null);

        //Check time picker
        travelTime.time().click();
        cy.a11yCheck(null, null);
    });

    //Existing future ticket is starting at 12:00
    it('period ticket - overlapping tickets warning', () => {
        cy.intercept("**/ticket/v1/search/zones").as("zones")

        const validFrom = Cypress.env('futureTicketStartYear') + "-" + Cypress.env('futureTicketStartMonth') + "-" + Cypress.env('futureTicketStartDay')

        menu.buyPeriodTicket().click();
        verify.verifyHeader('h2', 'Kjøp ny periodebillett');

        //Change date to Cypress.env.futureTicketStartX
        travelTime.showOptions()
        travelTime.inFuture().click()
        cy.wait("@zones")

        travelTime.date().type(validFrom)
        travelTime.time().type("23:00")

        newTicket.warning()
            .should("contain", "Du har allerede en billett i dette tidsrommet")

        cy.a11yCheck(null, null);
    })

    it('period ticket - time validity error', () => {
        cy.intercept("**/ticket/v1/search/zones").as("zones")

        menu.buyPeriodTicket().click();
        verify.verifyHeader('h2', 'Kjøp ny periodebillett');

        travelTime.showOptions()
        travelTime.inFuture().click()
        cy.wait("@zones")

        //Back in time
        travelTime.time().type("00:00")
        travelTime.validityError()
            .should("contain", "Starttidspunkt kan ikke være før nåværende tid og dato")

        cy.a11yCheck(null, null);
    })

    it('period ticket - summary', () => {
        cy.intercept('POST', '**/ticket/v1/search/zones').as('zones');

        menu.buyPeriodTicket().click();
        verify.verifyHeader('h2', 'Kjøp ny periodebillett');
        cy.wait('@zones');

        newTicket.goToSummary();
        verify.verifyHeader('h2', 'Oppsummering');

        cy.a11yCheck(null, null);
    });

    it('carnet ticket', () => {
        menu.buyCarnetTicket().click();
        verify.verifyHeader('h2', 'Kjøp nytt klippekort');

        cy.a11yCheck(null, null);
    });

    it('carnet ticket - open travellers', () => {
        menu.buyCarnetTicket().click();
        verify.verifyHeader('h2', 'Kjøp nytt klippekort');

        traveller.showOptions()
        newTicket.travellerSection().then($traveller => {
            options.areVisible($traveller, true)
        })

        cy.a11yCheck(null, null);
    });

    it('carnet ticket - summary', () => {
        cy.intercept('POST', '**/ticket/v1/search/zones').as('zones');

        menu.buyCarnetTicket().click();
        verify.verifyHeader('h2', 'Kjøp nytt klippekort');
        cy.wait('@zones');

        newTicket.goToSummary();
        verify.verifyHeader('h2', 'Oppsummering');

        cy.a11yCheck(null, null);
    });
});
