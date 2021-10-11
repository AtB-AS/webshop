import { history } from '../pageobjects/history.pageobject';
import { menu, verify } from '../pageobjects/common.pageobject';

describe('ticket history', () => {
    beforeEach(function () {
        cy.visitMainAsAuthorized();
        menu.history().click();
        verify.verifyHeader('h2', 'Kjøpshistorikk');
    });

    it('should present a list of purchased tickets', () => {
        history
            .info()
            .should(
                'contain',
                'Se historikk over alle kjøp og be om å få kvittering tilsendt via e-post'
            );

        history.tickets().should('have.length.gt', 0);
        history
            .ticketHeaders()
            .should('contain', '7-dagersbillett');
    });

    it('should include a specific purchased ticket', () => {
        const order_id = 'HUCVIBHX';
        const ticketType = '7-dagersbillett';
        const traveller = 'Voksen';
        const zone = 'Sone A, Sone B1, Sone C1';
        const header = '09.07.2021 - 7-dagersbillett';
        const timeOfPurchase = getTimeOfPurchase()
        const price = '840,00';
        const paymentMethod = 'Vipps';

        history.ticket(order_id).then(($ticket) => {
            history.ticketHeader($ticket).should('contain', header);
            history.ticketIsCollapsed($ticket, true);
            history.showDetails($ticket);
            history.ticketIsCollapsed($ticket, false);

            history
                .paymentInfo($ticket)
                .should('contain', timeOfPurchase)
                .and('contain', price)
                .and('contain', paymentMethod)
                .and('contain', order_id);
            history
                .ticketInfo($ticket)
                .should('contain', ticketType)
                .and('contain', traveller)
                .and('contain', zone);
            history.sendReceipt($ticket).should('be.enabled');

            history.hideDetails($ticket);
            history.ticketIsCollapsed($ticket, true);
        });
    });

    it('should show multiple products on one order correctly', () => {
        const order_id = '4YYKR8RD';
        const ticketType = '10 billetter';
        const traveller = 'Voksen';
        const zone = 'Sone A';
        const header = '08.10.2021 - 10 billetter, 3 reisende';
        const price = '??'; //Når betalt med rekvisisjon
        //TODO https://github.com/AtB-AS/webshop/issues/357
        const paymentMethod = 'Ukjent';

        history.ticket(order_id).then(($ticket) => {
            history.ticketHeader($ticket).should('contain', header);
            history.ticketIsCollapsed($ticket, true);
            history.showDetails($ticket);
            history.ticketIsCollapsed($ticket, false);

            history
                .paymentInfo($ticket)
                //.should('contain', timeOfPurchase)
                .and('contain', price)
                .and('contain', paymentMethod)
                .and('contain', order_id);
            history
                .ticketInfoMultipleLabel($ticket, 0)
                .should('contain', 'Billett 1 / 3')
            history
                .ticketInfoMultiple($ticket, 0)
                .should('contain', ticketType)
                .and('contain', traveller)
                .and('contain', zone);
            history
                .ticketInfoMultipleLabel($ticket, 1)
                .should('contain', 'Billett 2 / 3')
            history
                .ticketInfoMultiple($ticket, 1)
                .should('contain', ticketType)
                .and('contain', traveller)
                .and('contain', zone);
            history
                .ticketInfoMultipleLabel($ticket, 2)
                .should('contain', 'Billett 3 / 3')
            history
                .ticketInfoMultiple($ticket, 2)
                .should('contain', ticketType)
                .and('contain', traveller)
                .and('contain', zone);

            history.sendReceipt($ticket).should('be.enabled');

            history.hideDetails($ticket);
            history.ticketIsCollapsed($ticket, true);
        });
    });

    //Different timezone on the host running GH Actions
    function getTimeOfPurchase(){
        if (Cypress.env('runOnGitHub')){
            return '09.07.2021 - 10:12'
        }
        else {
            return '09.07.2021 - 12:12'
        }
    }
});
