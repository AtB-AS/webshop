export const menu = {
    openBurgerMenu: () => {
        cy.get('button#burgermenu').click();
        cy.get('button#burgermenu').should(
            'have.class',
            'ui-hamburgerButton--open'
        );
        cy.get('.pageHeader__nav__item').should('be.visible');
    },
    closeBurgerMenu: () => {
        cy.get('button#burgermenu').click();
        cy.get('button#burgermenu').should(
            'not.have.class',
            'ui-hamburgerButton--open'
        );
        cy.get('.pageHeader__nav__item').should('not.be.visible');
    },
    myProfile: () => cy.get('nav').find('a').contains('Min profil'),
    history: () => cy.get('nav').find('a').contains('Kjøpshistorikk'),
    buyPeriodTicket: () =>
        cy.get('nav').find('a').contains('Ny periodebillett'),
    buyCarnetTicket: () => cy.get('nav').find('a').contains('Nytt klippekort'),
    startPage: () => cy.get('h1.pageHeader__logo'),

    cancel: () =>
        cy.get('button.ui-pageHeader__back').contains('Avbryt').click(),
    backToOverview: () =>
        cy.get('a.ui-pageHeader__back').contains('Oversikt').click(),
    logOut: () => cy.get('.pageHeader__nav__logout').click()
};

export const verify = {
    verifyHeader: (header, text) => cy.get(header).contains(text)
};

//external email
export const email = {
    body: () => cy.get('body'),
    verifyUrl: () => cy.get('a')
};
