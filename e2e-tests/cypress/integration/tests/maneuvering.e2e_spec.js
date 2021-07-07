import { menu, verify } from '../pageobjects/common.pageobject';
import { mytickets } from '../pageobjects/mytickets.pageobject';

describe('maneuver in the webshop on desktop', () => {
    beforeEach(function () {
        cy.visitMainAsAuthorized();
    });

    it('should go to my tickets from AtB logo', () => {
        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');

        menu.startPage().click();
        verify.verifyHeader('h2', 'Mine billetter');
    });

    it('should open period tickets', () => {
        //Nav
        menu.buyPeriodTicket().click();
        verify.verifyHeader('h2', 'Kjøp ny periodebillett');
        menu.cancel();
        verify.verifyHeader('h2', 'Mine billetter');

        //Button from my tickets
        mytickets.buyPeriodTicket().click();
        verify.verifyHeader('h2', 'Kjøp ny periodebillett');
        menu.cancel();
        verify.verifyHeader('h2', 'Mine billetter');
    });

    it('should open carnet tickets', () => {
        //Nav
        menu.buyCarnetTicket().click();
        verify.verifyHeader('h2', 'Kjøp nytt klippekort');
        menu.cancel();
        verify.verifyHeader('h2', 'Mine billetter');

        //Button from my tickets
        mytickets.buyCarnetTicket().click();
        verify.verifyHeader('h2', 'Kjøp nytt klippekort');
        menu.cancel();
        verify.verifyHeader('h2', 'Mine billetter');
    });

    it('should open my profile', () => {
        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');

        menu.backToOverview();
        verify.verifyHeader('h2', 'Mine billetter');
    });

    it('should open ticket history', () => {
        menu.history().click();
        verify.verifyHeader('h2', 'Kjøpshistorikk');

        menu.backToOverview();
        verify.verifyHeader('h2', 'Mine billetter');
    });

    it('should log out', () => {
        verify.verifyHeader('h2', 'Mine billetter');

        cy.logOut();
    });
});

describe('maneuver in the webshop on mobile', () => {
    beforeEach(function () {
        cy.viewport('iphone-8');
        cy.visitMainAsAuthorized();
    });

    it('should go to my tickets from AtB logo', () => {
        menu.openBurgerMenu();

        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');

        menu.startPage().click();
        verify.verifyHeader('h2', 'Mine billetter');
    });

    it('should open period tickets', () => {
        //Nav
        menu.openBurgerMenu();
        menu.buyPeriodTicket().click();
        verify.verifyHeader('h2', 'Kjøp ny periodebillett');
        menu.cancel();
        verify.verifyHeader('h2', 'Mine billetter');

        //Button from my tickets
        mytickets.buyPeriodTicket().click();
        verify.verifyHeader('h2', 'Kjøp ny periodebillett');
        menu.cancel();
        verify.verifyHeader('h2', 'Mine billetter');
    });

    it('should open carnet tickets', () => {
        //Nav
        menu.openBurgerMenu();
        menu.buyCarnetTicket().click();
        verify.verifyHeader('h2', 'Kjøp nytt klippekort');
        menu.cancel();
        verify.verifyHeader('h2', 'Mine billetter');

        //Button from my tickets
        mytickets.buyCarnetTicket().click();
        verify.verifyHeader('h2', 'Kjøp nytt klippekort');
        menu.cancel();
        verify.verifyHeader('h2', 'Mine billetter');
    });

    it('should open my profile', () => {
        menu.openBurgerMenu();
        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');

        menu.backToOverview();
        verify.verifyHeader('h2', 'Mine billetter');
    });

    it('should open ticket history', () => {
        menu.openBurgerMenu();
        menu.history().click();
        verify.verifyHeader('h2', 'Kjøpshistorikk');

        menu.backToOverview();
        verify.verifyHeader('h2', 'Mine billetter');
    });

    it('should log out', () => {
        verify.verifyHeader('h2', 'Mine billetter');

        menu.openBurgerMenu();
        menu.logOut();
    });
});
