export const newTicket = {
    ticketCategory: () => cy.get('button#varighet'),
    ticketCategoryDetails: () => cy.get('#varighetregion'),
    ticketProduct: (product) =>
        cy.get('.ui-input-radio__title').contains(product),

    travellerDetails: () => cy.get('#reisenderegion'),
    travellers: () => cy.get('button#reisende'),
    traveller: (type) => cy.get('input#UserType' + type),

    travelTimeDetails: () => cy.get('#durationregion'),
    travelTime: () => cy.get('button#duration'),

    goToSummary: () => cy.get('button').contains('Gå til oppsummering').click(),

    travelType: () => cy.get(".ui-labelItem__label").contains("Reisetype").parents(".ui-labelItem"),
    productSection: () => cy.get("button#varighet").parents("section"),
    travellerSection: () => cy.get("button#reisende").parents("section"),
    travelTimeSection: () => cy.get("button#duration").parents("section"),
    departureZoneSection: () => cy.get("label[for='travelFromZone']"),
    arrivalZoneSection: () => cy.get("label[for='travelToZone']"),

    price: () => cy.get(".shop__summaryPrice"),
    mva: () => cy.get(".shop__summaryPrice").parents(".ui-section").find(".ui-labelItem__label").contains("mva").parents(".ui-labelItem")
};
//TODO Re-name noen export. Tester om 'options' fungerer. Når må man ha egne per section? Tester lengre ned her..
export const options = {
    value: elem => cy.wrap(elem).find(".ui-labelItem"),
    show: () => cy.get("button#reisende").click(),
    hide: () => cy.get("button#reisende").click(),
    traveller: (type) => cy.get('input#UserType' + type),
    areVisible: (elem, bool) => {
        if (bool){
            cy.wrap(elem).should("have.class", "ui-group--open")
            cy.wrap(elem).find(".ui-group__headerButton__editText__text").should("not.exist")
            //cy.wrap(elem).find(".ui-group__item").find("label[for='UserTypeStudent'").should("be.visible")
        }
        else {
            cy.wrap(elem).should("not.have.class", "ui-group--open")
            cy.wrap(elem).find(".ui-group__headerButton__editText__text").should("contain", "Endre")
            //cy.wrap(elem).find(".ui-group__item").find("label[for='UserTypeStudent'").should("not.be.visible")
        }
    }
};

export const zone = {
    departureZone: () => cy.get("select#travelFromZone"),
    departureZoneTariff: tariff => cy.get("select#travelFromZone").find("option[value='" + tariff + "']"),
    arrivalZone: () => cy.get("select#travelToZone"),
    arrivalZoneTariff: tariff => cy.get("select#travelToZone").find("option[value='" + tariff + "']"),
}

export const traveller = {
    value: elem => cy.wrap(elem).find(".ui-labelItem"),
    option: (type) => cy.get('input#UserType' + type),
    showOptions: () => cy.get("button#reisende").click(),
    hideOptions: () => cy.get("button#reisende").click(),
    optionsAreVisible: (elem,bool) => {
        if (bool){
            cy.wrap(elem).should("have.class", "ui-group--open")
            cy.wrap(elem).find(".ui-group__headerButton__editText__text").should("not.exist")
            cy.wrap(elem).find(".ui-group__item").find("label[for='UserTypeStudent'").should("be.visible")
        }
        else {
            cy.wrap(elem).should("not.have.class", "ui-group--open")
            cy.wrap(elem).find(".ui-group__headerButton__editText__text").should("contain", "Endre reisende")
            cy.wrap(elem).find(".ui-group__item").find("label[for='UserTypeStudent'").should("not.be.visible")
        }
    }
};

export const travelTime2 = {
    value: elem => cy.wrap(elem).find(".ui-labelItem"),
    option: (type) => cy.get('input#UserType' + type),
    showOptions: () => cy.get("button#reisende").click(),
    hideOptions: () => cy.get("button#reisende").click(),
    optionsAreVisible: (elem,bool) => {
        if (bool){
            cy.wrap(elem).should("have.class", "ui-group--open")
            cy.wrap(elem).find(".ui-group__headerButton__editText__text").should("not.exist")
            cy.wrap(elem).find(".ui-group__item").find("label[for='UserTypeStudent'").should("be.visible")
        }
        else {
            cy.wrap(elem).should("not.have.class", "ui-group--open")
            cy.wrap(elem).find(".ui-group__headerButton__editText__text").should("contain", "Endre tid")
            cy.wrap(elem).find(".ui-group__item").find("label[for='UserTypeStudent'").should("not.be.visible")
        }
    }
};

export const travelTime = {
    now: () => cy.get('input#travel-now'),
    inFuture: () => cy.get('input#travel-future'),
    date: () => cy.get('input#date'),
    time: () => cy.get('input#time')
};
