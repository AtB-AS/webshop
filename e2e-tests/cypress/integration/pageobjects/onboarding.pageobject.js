export const onboarding = {
    stepTitle: () => cy.get('#progress-header'),
    back: () =>
        cy
            .get('.ui-progressHeader')
            .find('button.ui-progressHeader__button--left'),
    skip: () =>
        cy
            .get('.ui-progressHeader')
            .find('button.ui-progressHeader__button--right')
};

export const onboardingStep1 = {
    setFirstname: (firstname) => cy.get('input#firstname').type(firstname),
    setLastname: (lastname) => cy.get('input#lastname').type(lastname),
    setPhoneNumber: (phone) => cy.get('input#phone').type(phone),

    email: () => cy.get('input#email'),
    emailError: (hasError) => {
        if (hasError) {
            cy.get('label[for=phone]').should(
                'have.class',
                'ui-input-text--error'
            );
        } else {
            cy.get('label[for=phone]').should(
                'not.have.class',
                'ui-input-text--error'
            );
        }
    },
    emailErrorMsg: () =>
        cy.get('label[for=phone]').find('.ui-input-text__errorMessage'),

    nextStep: () =>
        cy.get('.ui-section').find('button').contains('Neste').click()
};

export const onboardingStep2 = {
    emailConsentLabel: () => cy.get('label[for=consent1186]'),
    emailConsent: () => cy.get('input#consent1186'),
    notificationConsentLabel: () => cy.get('label[for=consent1197]'),
    notificationConsent: () => cy.get('input#consent1197'),
    saveConsents: () =>
        cy
            .get('.ui-section')
            .find('button')
            .contains('Lagre mine samtykker')
            .click()
};

export const onboardingStep3 = {
    setTravelCard: (cardNo) => cy.get('input#travelCard').type(cardNo),
    travelCard: () => cy.get('input#travelCard'),
    addTravelCard: () =>
        cy
            .get('.ui-section')
            .find('button')
            .contains('Legg til t:kort')
            .click(),
    skipTravelCard: () =>
        cy
            .get('.ui-section')
            .find('button')
            .contains('Legg til senere')
            .click(),
    travelCardError: (hasError) => {
        if (hasError) {
            cy.get('label[for=travelCard]').should(
                'have.class',
                'ui-input-text--error'
            );
        } else {
            cy.get('label[for=travelCard]').should(
                'not.have.class',
                'ui-input-text--error'
            );
        }
    },
    travelCardErrorMsg: () =>
        cy.get('label[for=travelCard]').find('.ui-input-text__errorMessage')
};

export const onboardingStep4 = {
    appleStoreBadge: () => cy.get('img[alt="iOS badge"'),
    playStoreBadge: () => cy.get('img[alt="Android badge"'),
    completeOnboarding: () =>
        cy.get('.ui-section').find('button').contains('Fullfør').click()
};
