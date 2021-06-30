export const menu = {
    myProfile: () => cy.get('nav').find('a').contains('Min profil'),
    history: () => cy.get('nav').find('a').contains('Kjøpshistorikk'),
    buyPeriodTicket: () =>
        cy.get('nav').find('a').contains('Ny periodebillett'),
    buyCarnetTicket: () => cy.get('nav').find('a').contains('Nytt klippekort'),
    startPage: () => cy.get('h1.pageHeader__logo')
};

export const verify = {
    verifyHeader: (header, text) => cy.get(header).contains(text)
};

//external email
export const email = {
    body: () => cy.get('body'),
    verifyUrl: () => cy.get('a')
};
