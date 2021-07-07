export const history = {
    info: () => cy.get('.ui-section'),

    tickets: () =>
        cy.get('.main').find('.typo-body__primary.ui-textContainer '),
    ticketHeaders: () =>
        cy.get('.main').find('.ui-expandable__headerButton__title'),
    ticket: (order_id) =>
        cy
            .get("button[id='ATB:FareContract:" + order_id + "'")
            .parents('section')
            .scrollIntoView(),
    ticketHeader: (ticket) =>
        cy.wrap(ticket).find('.ui-expandable__headerButton__title'),
    ticketIsCollapsed: (ticket, isCollapsed) => {
        if (isCollapsed) {
            cy.wrap(ticket).should('not.have.class', 'ui-expandable--open');
            cy.wrap(ticket)
                .find('.ui-expandable__headerButton__expandText')
                .should('contain', 'Vis');
        } else {
            cy.wrap(ticket).should('have.class', 'ui-expandable--open');
            cy.wrap(ticket)
                .find('.ui-expandable__headerButton__expandText')
                .should('contain', 'Skjul');
        }
    },
    showDetails: (ticket) =>
        cy.wrap(ticket).find('button.ui-expandable__headerButton').click(),
    hideDetails: (ticket) =>
        cy.wrap(ticket).find('button.ui-expandable__headerButton').click(),
    paymentInfo: (ticket) => cy.wrap(ticket).find('.metadata-list'),
    ticketInfo: (ticket) => cy.wrap(ticket).find('.ticket-list'),
    sendReceipt: (ticket) =>
        cy
            .wrap(ticket)
            .find('button')
            .contains('Be om kvittering på e-post')
            .parents('button')
};
