import { menu, verify } from '../pageobjects/common.pageobject';
import { mytickets } from '../pageobjects/mytickets.pageobject';
import { myprofile } from '../pageobjects/myprofile.pageobject';

/*
 TravelCard for default user is 344545453363471*
 */

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

    it('registered travel card should be visible', () => {
        mytickets.travelCard().should("contain", "45 3363471")
    })

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

    it('carnet ticket should be correct', () => {
        const order_id = 'R72EMYQA';
        const validFrom = '20.08.2021 - ' + getValidCarnetHours(11) + ':50'
        const validTo = '21.08.2022 - ' + getValidCarnetHours(11) + ':50'
        const header1 = '10 klipp igjen'
        const header2 = 'Ingen aktive klipp'
        const type = 'Klippekort (10 billetter)';
        const zones = 'Reise i 1 sone (Sone A)';
        const traveller = '1 Voksen';
        const payment = 'Vipps';

        verify.verifyHeader('h2', 'Mine billetter');

        //Verify overview
        mytickets.ticket(order_id).then(($ticket) => {
            mytickets.carnetTicketIconIsCorrect($ticket);
            mytickets.carnetTicketHeader($ticket)
                .should('contain', header1)
                .and('contain', header2)
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
                .and('contain', validTo)
                .and('contain', 'Kjøpstidspunkt')
                .and('contain', 'Betalt med')
                .and('contain', payment)
                .and('contain', 'Ordre-ID')
                .and('contain', order_id);
            mytickets.ticketReceipt($ticket)
                .should("be.enabled")
        });

        //Hide details
        mytickets.ticket(order_id).then(($ticket) => {
            mytickets.hideDetails($ticket);
            mytickets.ticketIsCollapsed($ticket, true);
            mytickets.ticketDetails($ticket).should('not.be.visible');
        });
    });

    //** NOTE! Only valid until pre-set date **
    it('future period ticket should be waiting and correct', () => {
        const order_id = Cypress.env("futureTicketOrderId");
        const validFrom = getValidTime(13, 0)
        const validTo = getValidTime(13, 0, 7)
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
                .and('contain', validTo)
                .and('contain', 'Kjøpstidspunkt')
                .and('contain', 'Betalt med')
                .and('contain', payment)
                .and('contain', 'Ordre-ID')
                .and('contain', order_id);
            mytickets.ticketReceipt($ticket)
                .should("be.enabled")
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

    function getValidTime(hours, minutes, increaseDays = 0, ){
        let myDate = new Date();
        let days = Number.parseInt(Cypress.env('futureTicketStartDay')) + increaseDays
        myDate.setFullYear(Cypress.env('futureTicketStartYear'))
        myDate.setMonth(Cypress.env('futureTicketStartMonth') - 1)
        myDate.setDate(days)
        //Different timezone on the host running GH Actions
        if (Cypress.env('runOnGitHub')){
            myDate.setHours(hours - 2)
        }
        else {
            myDate.setHours(hours)
        }
        myDate.setMinutes(minutes)

        let dd = myDate.getDate();
        let mm = myDate.getMonth();
        let yyyy = myDate.getFullYear();
        let HH = myDate.getHours()
        let MM = myDate.getMinutes()

        //Padding
        if(dd < 10){ dd = '0' + dd }
        if(mm < 10){ mm = '0' + mm }
        if(HH < 10){ HH = '0' + HH }
        if(MM < 10){ MM = '0' + MM }

        return dd + '.' + mm + '.' + yyyy + " - " + HH + ":" + MM
    }

    //Different timezone on the host running GH Actions
    function getValidCarnetHours(hours){
        if (Cypress.env('runOnGitHub')){
            let hh = hours - 2
            if (hh < 10){ hh = '0' + hh}
            return hh
        }
        else {
            return hours
        }
    }
});
