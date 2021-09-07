export const newTicket = {

    travelType: () => cy.get(".ui-labelItem__label").contains("Reisetype").parents(".ui-labelItem"),
    carnetTickets: () => cy.get(".ui-labelItem__label").contains("Antall billetter").parents(".ui-labelItem"),
    productsSection: () => cy.get("button#varighet").parents("section"),
    travellerSection: () => cy.get("button#reisende").parents("section"),
    travelTimeSection: () => cy.get("button#duration").parents("section"),
    departureZoneSection: () => cy.get("label[for='travelFromZone']"),
    arrivalZoneSection: () => cy.get("label[for='travelToZone']"),

    goToSummary: () => {
        cy.wait(200);
        cy.get(".shop__summaryPrice").contains("00");
        cy.get('button').contains('Gå til oppsummering').click();
    },
    goToSummaryButton: () => cy.get('button').contains('Gå til oppsummering').parents("button"),

    price: () => cy.get(".shop__summaryPrice").contains("00"),
    mva: () => cy.get(".shop__summaryPrice").parents(".ui-section").find(".ui-labelItem__label").contains("mva").parents(".ui-labelItem"),
    infoText: () => cy.get(".ui-message__content"),
    warning: () => cy.get(".ui-message--warning")
};

export const summary = {
    back: () => cy.get("button.ui-pageHeader__back").click(),
    ticketDetails: detail => cy.get("h2.ui-section__headerTitle").contains("Om billetten").parents(".ui-section").find(".ui-labelItem__label").contains(detail).parents(".ui-labelItem"),
    price: () => cy.get(".shop__summaryPrice"),
    payButton: () => cy.get('button').contains('etal'),
    pay: () => cy.get('button').contains('Betal nå').click(),

    storePayment: () => cy.get("input#storePayment"),
    storePaymentConfirm: () => cy.get("label[for='storePayment']").find(".ui-input-checkbox__box"),
    storePaymentLabel: () => cy.get("label[for='storePayment']"),
    paymentOption: type => cy.get("label[for='" + type + "']").find(".ui-input-radio__box"),
    paymentOptionLabel: type => cy.get("label[for='" + type + "']").find(".ui-input-radio__title"),
    storedPaymentOption: type => cy.get("input[name='paymentType']").siblings("label.ui-input-radio").contains(type).parents(".ui-group__item").find((".ui-input-radio__box")),
    storedPaymentOptionLabel: type => cy.get("input[name='paymentType']").siblings("label.ui-input-radio").contains(type).parents(".ui-group__item").find(".ui-input-radio__title"),
    storedPaymentOptionExpiry: type => cy.get("input[name='paymentType']").siblings("label.ui-input-radio").contains(type).parents(".ui-group__item").find(".ui-input-radio__subtitle"),
    storedPaymentOptionIcon: type => cy.get("input[name='paymentType']").siblings("label.ui-input-radio").contains(type).parents(".ui-group__item").find("img"),
}

export const options = {
    value: elem => cy.wrap(elem).find(".ui-labelItem"),
    areVisible: (elem, bool) => {
        if (bool){
            cy.wrap(elem).should("have.class", "ui-group--open")
            cy.wrap(elem).find(".ui-group__headerButton__editText__text").should("not.exist")
        }
        else {
            cy.wrap(elem).should("not.have.class", "ui-group--open")
            cy.wrap(elem).find(".ui-group__headerButton__editText__text").should("contain", "Endre")
        }
    }
};

export const zone = {
    departureZone: () => cy.get("select#travelFromZone"),
    departureZoneTariff: tariff => cy.get("select#travelFromZone").find("option[value='" + tariff + "']"),
    arrivalZone: () => cy.get("select#travelToZone"),
    arrivalZoneTariff: tariff => cy.get("select#travelToZone").find("option[value='" + tariff + "']"),
}

export const products = {
    set: value => cy.get("#varighetregion").find(".ui-input-radio__title").contains(value).parentsUntil(".ui-group__item").find("input").check({force: true}),
    showOptions: () => {
        cy.get('#varighetregion').then($details => {
            if (!$details.hasClass('ui-group__content--open')) {
                cy.get("button#varighet").click()
            }
        })
    },
    hideOptions: () => {
        cy.get('#varighetregion').then($details => {
            if ($details.hasClass('ui-group__content--open')) {
                cy.get("button#varighet").click()
            }
        })
    },
}

export const traveller = {
    set: value => cy.get("#reisenderegion").find(".ui-input-radio__title").contains(value).parentsUntil(".ui-group__item").find("input").check({force: true}),
    showOptions: () => {
        cy.get('#reisenderegion').then($details => {
            if (!$details.hasClass('ui-group__content--open')) {
                cy.get("button#reisende").click()
            }
        })
    },
    hideOptions: () => {
        cy.get('#reisenderegion').then($details => {
            if ($details.hasClass('ui-group__content--open')) {
                cy.get("button#reisende").click()
            }
        })
    },
};

export const travelTime = {
    showOptions: () => {
        cy.get('#durationregion').then($details => {
            if (!$details.hasClass('ui-group__content--open')) {
                cy.get("button#duration").click()
            }
        })
    },
    hideOptions: () => {
        cy.get('#durationregion').then($details => {
            if ($details.hasClass('ui-group__content--open')) {
                cy.get("button#duration").click()
            }
        })
    },
    now: () => cy.get('input#travel-now'),
    inFuture: () => cy.get("label.ui-input-radio[for=travel-future]"),
    date: () => cy.get('input#date'),
    time: () => cy.get('input#time'),
    validityError: () => cy.wait(200).get("#durationregion").find(".ui-message--error")
};
