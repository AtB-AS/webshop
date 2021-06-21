export const buy = {
    ticketCategory: () => cy.get("button#varighet"),
    ticketCategoryDetails: () => cy.get("#varighetregion"),
    ticketProduct: product => cy.get(".ui-input-radio__title").contains(product),

    travellerDetails: () => cy.get("#reisenderegion"),
    travellers: () => cy.get("button#reisende"),
    traveller: type => cy.get("input#UserType" + type),

    travelTimeDetails: () => cy.get("#durationregion"),
    travelTime: () => cy.get("button#duration"),
};

export const travelTime = {
    now: () =>  cy.get("input#travel-now"),
    inFuture: () =>  cy.get("input#travel-future"),
    date: () => cy.get("input#date"),
    time: () => cy.get("input#time")
}
