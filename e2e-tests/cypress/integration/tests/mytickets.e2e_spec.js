import { menu, verify } from '../pageobjects/common.pageobject';
import { mytickets } from '../pageobjects/mytickets.pageobject';
import { myprofile } from '../pageobjects/myprofile.pageobject';

describe('account information', () => {
    beforeEach(function () {
        cy.visitMainAsAuthorized();
    });

    it('profile name should be visible', () => {
        const firstname = 'Mr';
        const lastname = 'Test ' + randomNumbers(8);

        //Edit name
        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');

        myprofile.setProfileName(firstname, lastname);
        menu.startPage().click();

        //Verify
        mytickets.accountInfo().should('contain', firstname + ' ' + lastname);
    });

    it('contact phone number should be visible', () => {
        const phoneNumberFormatted = '+47 ' + randomFormattedPhoneNumber();
        const phoneNumber = getNonFormattedPhoneNumber(phoneNumberFormatted);

        //Edit name
        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');

        myprofile.setPhoneNumber(phoneNumber);
        menu.startPage().click();

        //Verify
        mytickets.accountInfo().should('contain', phoneNumberFormatted);
    });

    it('should be able to add travel card', () => {
        const travelCardNo = '3445454533634636';
        const travelCardAsDisplayed = '45 3363463';

        cy.intercept('POST', '**/webshop/v1/travelcard').as('addTravelcard');

        //Remove if exist
        myprofile.travelCardOperation('remove');
        menu.startPage().click();
        mytickets.travelCard().should('not.exist');

        //Add
        mytickets.addTravelCard().click();
        myprofile.travelCardInput().type(travelCardNo);
        myprofile.saveValue();
        cy.wait('@addTravelcard');
        menu.startPage().click();

        //Verify
        mytickets.travelCard().should('contain', travelCardAsDisplayed);
    });

    function randomNumbers(number) {
        let rand = '';
        for (let i = 0; i < number; i++) {
            rand += Math.floor(Math.random() * 10);
        }
        return rand;
    }

    function randomFormattedPhoneNumber() {
        let rand = '';
        for (let i = 0; i < 4; i++) {
            rand += Math.floor(Math.random() * 10);
            rand += Math.floor(Math.random() * 10);
            if (i !== 3) {
                rand += ' ';
            }
        }
        return rand;
    }

    function getNonFormattedPhoneNumber(phoneNumber) {
        return phoneNumber.replace(/\s/g, '');
    }
});

describe('ticket details', () => {
    beforeEach(function () {
        cy.visitMainAsAuthorized();
    });

    //** NOTE! Only valid until 31.08.2021 **
    it('future ticket should be waiting and correct', () => {
        const order_id = 'HUCVIBHX';
        const validFrom = '01.09.2021 - 12:00';
        const header = 'Gyldig fra ' + validFrom;
        const type = '7-dagersbillett';
        const zones = 'Reise i 3 soner (Sone A til C1)';
        const traveller = '1 Voksen';
        const payment = 'Vipps';

        verify.verifyHeader('h2', 'Mine billetter');

        //Verify overview
        mytickets.ticket(order_id).then(($ticket) => {
            mytickets.ticketIconIsWaiting($ticket);
            mytickets.ticketHeader($ticket).should('contain', header);
            mytickets
                .ticketSummary($ticket)
                .should('contain', type)
                .and('contain', zones)
                .and('contain', traveller);
            mytickets.ticketIsCollapsed($ticket, true);
            mytickets.ticketDetails($ticket).should('not.be.visible');
        });

        //Verify details
        mytickets.ticket(order_id).then(($ticket) => {
            mytickets.showDetails($ticket);
            mytickets.ticketIsCollapsed($ticket, false);
            mytickets
                .ticketDetails($ticket)
                .should('be.visible')
                .and('contain', 'Gyldig fra')
                .and('contain', validFrom)
                .and('contain', 'Gyldig til')
                .and('contain', 'Kjøpstidspunkt')
                .and('contain', 'Betalt med')
                .and('contain', payment)
                .and('contain', 'Ordre-ID')
                .and('contain', order_id);
        });

        //Hide details
        mytickets.ticket(order_id).then(($ticket) => {
            mytickets.hideDetails($ticket);
            mytickets.ticketIsCollapsed($ticket, true);
            mytickets.ticketDetails($ticket).should('not.be.visible');
        });
    });

    //Buy a ticket is configured in 'cypress.json'
    if (Cypress.env('withBuyTicket')) {
        it('current ticket should be valid and correct', () => {
            let order_id = 'ERROR';
            const timeoutValue = Cypress.env('buyTicketTimeout'); //default: 10 min
            const header = 'minutter igjen';
            const type = 'Enkeltbillett buss/trikk';
            const zones = 'Reise i 1 sone (Sone A)';
            const traveller = '1 Voksen';
            const payment = 'Bankkort';

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
                    //Verify overview
                    mytickets.ticket(order_id).then(($ticket) => {
                        mytickets.ticketIconIsValid($ticket);
                        mytickets
                            .ticketHeader($ticket)
                            .should('contain', header);
                        mytickets
                            .ticketSummary($ticket)
                            .should('contain', type)
                            .and('contain', zones)
                            .and('contain', traveller);
                        mytickets.ticketIsCollapsed($ticket, true);
                        mytickets
                            .ticketDetails($ticket)
                            .should('not.be.visible');
                    });

                    //Verify details
                    mytickets.ticket(order_id).then(($ticket) => {
                        mytickets.showDetails($ticket);
                        mytickets.ticketIsCollapsed($ticket, false);
                        mytickets
                            .ticketDetails($ticket)
                            .should('be.visible')
                            .and('contain', 'Gyldig fra')
                            .and('contain', 'Gyldig til')
                            .and('contain', 'Kjøpstidspunkt')
                            .and('contain', 'Betalt med')
                            .and('contain', payment)
                            .and('contain', 'Ordre-ID')
                            .and('contain', order_id);
                    });

                    //Hide details
                    mytickets.ticket(order_id).then(($ticket) => {
                        mytickets.hideDetails($ticket);
                        mytickets.ticketIsCollapsed($ticket, true);
                        mytickets
                            .ticketDetails($ticket)
                            .should('not.be.visible');
                    });
                }
            });
        });
    }
});
