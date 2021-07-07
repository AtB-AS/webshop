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
    showDetails: (tickets) =>
        cy.wrap(tickets).find('button.ui-expandable__headerButton').click(),
    hideDetails: (tickets) =>
        cy.wrap(tickets).find('button.ui-expandable__headerButton').click()
};
