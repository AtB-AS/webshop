import { menu, verify } from './common.pageobject';

export const myprofile = {
    firstName: () =>
        cy
            .get('.ui-textContainer')
            .contains('Fornavn')
            .parents('.ui-labelItem'),
    lastName: () =>
        cy
            .get('.ui-textContainer')
            .contains('Etternavn')
            .parents('.ui-labelItem'),
    phoneNumber: () =>
        cy
            .get('.ui-textContainer')
            .contains('Telefonnummer')
            .parents('.ui-labelItem'),
    logInMethod: () =>
        cy
            .get('h2.ui-section__headerTitle')
            .contains('Innloggingsmetode')
            .parents('.ui-section__item'),

    saveValue: () =>
        cy
            .get('button.ui-editSection__fieldset__saveButton')
            .contains('Lagre')
            .click(),
    setProfileName: (firstname, lastname) => {
        cy.intercept('PATCH', '**/webshop/v1/profile').as('profile');

        cy.get('button').contains('Endre navn').click();
        cy.get('input#firstname').type('{selectall}{del}' + firstname);
        cy.get('input#lastname').type('{selectall}{del}' + lastname);
        cy.get('button.ui-editSection__fieldset__saveButton')
            .contains('Lagre')
            .click();
        cy.wait('@profile');
    },
    editPhoneNumber: () =>
        cy.get('button').contains('Endre telefonnummer').click(),
    setPhoneNumber: (phoneNumber) => {
        cy.intercept('PATCH', '**/webshop/v1/profile').as('profile');

        cy.get('button').contains('Endre telefonnummer').click();
        cy.get('input#phone').type('{selectall}{del}' + phoneNumber);
        cy.get('button.ui-editSection__fieldset__saveButton')
            .contains('Lagre')
            .click();
        cy.wait('@profile');
    },
    addTravelCard: () => cy.get('button').contains('Legg til t:kort'),
    travelCardInput: () => cy.get('input#tkort'),

    consent: () => cy.get('input#consent1186'),

    //remove or add
    travelCardOperation: (operation) => {
        const travelCardNo = '3445454533634636';

        cy.intercept('POST', '**/webshop/v1/travelcard').as('addTravelcard');
        cy.intercept('DELETE', '**/webshop/v1/travelcard').as(
            'removeTravelcard'
        );

        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');

        //Add or remove travel card
        cy.get('h2.ui-section__headerTitle')
            .contains('Billettbærere')
            .parents('.ui-section__item')
            .find('.ui-labelItem')
            .then(($elem) => {
                if ($elem.text().includes('Du har ingen t:kort registrert')) {
                    if (operation === 'add') {
                        cy.get('button')
                            .contains('Legg til t:kort')
                            .click({ force: true });
                        cy.get('input#tkort').type(travelCardNo);
                        cy.get('button')
                            .contains('Lagre')
                            .click({ force: true });
                        cy.wait('@addTravelcard');
                    }
                } else {
                    if (operation === 'remove') {
                        cy.get('button.ui-editSection__editButton')
                            .contains('Fjern t:kort')
                            .click({ force: true });
                        cy.get('button.ui-editSection__fieldset__saveButton')
                            .contains('Fjern t:kort')
                            .click({ force: true });
                        cy.wait('@removeTravelcard');
                    }
                }
            });
    }
};
