import { myprofile } from '../pageobjects/myprofile.pageobject';
import { menu, verify } from '../pageobjects/common.pageobject';
import { newTicket } from '../pageobjects/buyticket.pageobject';
import { mytickets } from '../pageobjects/mytickets.pageobject';

//User without travel card and stored payment cards

describe('missing stored information scenarios', () => {
    before(() => {
        cy.visitMainAsNotAuthorized();
    });
    beforeEach(function() {
        cy.visitMainAsAuthorized(Cypress.env('email_noTravelCard'));
    });
    after(() => {
        cy.logOut();
    });

    it('a11y check - my profile - change travel card', () => {
        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');

        //Check on hover
        myprofile.addTravelCard().trigger('mouseover');
        cy.injectAxe().then(() => {
            cy.a11yCheck(null, null);
        });

        //Check on editing mode
        myprofile.addTravelCard().click();
        //Color-contrast error is checked out in https://github.com/AtB-AS/webshop/issues/249
        cy.a11yCheck(null, {
            rules: {
                'color-contrast': { enabled: false }
            }
        });
        myprofile.cancel()
    });

    it('period ticket - summary should be disabled for non existing travel card', () => {
        menu.buyPeriodTicket().click();
        verify.verifyHeader('h2', 'Kjøp ny periodebillett');

        //Verify
        newTicket.infoText().should("contain", "Legg til et t:kort før kjøp av billett")
        newTicket.goToSummaryButton().should("have.class", "ui-button--disabled")
    })

    it('carnet ticket - summary should be disabled for non existing travel card', () => {
        menu.buyCarnetTicket().click();
        verify.verifyHeader('h2', 'Kjøp nytt klippekort');

        //Verify
        newTicket.infoText().should("contain", "Legg til et t:kort før kjøp av billett")
        newTicket.goToSummaryButton().should("have.class", "ui-button--disabled")
    })

    it('my profile - should not have any stored payment cards', () => {
        cy.intercept('GET', '**/ticket/v2/recurring-payments').as('recurringPayments');

        menu.myProfile().click();
        cy.wait("@recurringPayments")
        verify.verifyHeader('h2', 'Min profil');

        myprofile.storedPayments().should("contain", "Ingen lagrede betalingskort")
    })

    it('my profile - should get error messages for wrong travel card', () => {
        const travelCardError = "3443354354353454"
        const travelCardError2 = "1616006085855858"

        cy.intercept('POST', '**/webshop/v1/travelcard').as('addTravelcard');

        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');

        myprofile.addTravelCard().click()
        myprofile.travelCardInput().type(travelCardError);
        myprofile.saveValue();
        myprofile.travelCardError().should("contain", "t:kort id må starte på 1616 0060")
        myprofile.travelCardInput().type("{selectall}{del}");
        myprofile.travelCardInput().type(travelCardError2)
        myprofile.saveValue();
        cy.wait("@addTravelcard")
        myprofile.travelCardError()
            .should("contain", "du har tastet inn feil t:kortnummer")
            .and("contain", "se over og kontroller at alt stemmer")
        myprofile.cancel()
    })

    //TODO Disabled due to problems when changing travelcards (each travelcard is represented as a GUID that is not checked, i.e. multiple travelcards will exist on the accound)
    xit('account information - should be able to add travel card', () => {
        const travelCardNo = Cypress.env('travelCardNo')
        const travelCardAsDisplayed = Cypress.env('travelCardAsDisplayed')

        cy.intercept('POST', '**/webshop/v1/travelcard').as('addTravelcard');
        cy.intercept('DELETE', '**/webshop/v1/travelcard').as('removeTravelcard');

        //Pre
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

        //Remove
        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');
        myprofile.removeTravelCard()
        cy.wait('@removeTravelcard');
        menu.startPage().click();

        //Verify
        mytickets.travelCard().should('not.exist');
    });

})