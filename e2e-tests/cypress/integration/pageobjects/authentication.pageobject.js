export const auth = {
    setEmail: (email) => cy.get('input#email').type(email),
    setPassword: (pw) => cy.get('input#password').type(pw),
    createProfile: () =>
        cy.get('button').contains('Opprett profil').click({ force: true }),
    useEmailAsLogIn: () =>
        cy
            .get('.ui-section')
            .find('a')
            .contains('Jeg vil heller bruke e-post')
            .click(),
    createNewEmailProfile: () =>
        cy
            .get('.ui-section')
            .find('a')
            .contains('Opprett en ny profil')
            .click(),
    forgotPassword: () => cy.get('a').contains('Glemt passord').click(),
    info: () => cy.get('.ui-section'),
    resetPassword: () =>
        cy.get('button').contains('Tilbakestill passord').click({ force: true })
};

//External Firebase confirmation page
export const firebase = {
    confirm: () => cy.get('button').contains('Continue').click({ force: true }),
    infoText: () => cy.get('.firebaseui-text'),
    setNewPassword: (pw) => cy.get('input[name="newPassword"').type(pw),
    savePassword: () =>
        cy.get('button').contains('Save').click({ force: true }),
    title: () => cy.get('.firebaseui-title')
};
