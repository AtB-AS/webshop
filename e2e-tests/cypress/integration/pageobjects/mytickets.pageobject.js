export const mytickets = {
    tickets: () => cy.get('.ui-ticketDetails'),
    ticket: (order_id) =>
        cy
            .get('.ui-ticketDetails')
            .find('button#' + order_id)
            .parents('.ui-ticketDetails'),
    //Timeout = 5 min to ensure ticket is received
    waitForTicket: (order_id, timeoutValue) =>
        cy
            .get('.ui-ticketDetails')
            .find('button#' + order_id, { timeout: timeoutValue })
            .should('have.id', order_id),
    showDetails: (tickets) => cy.wrap(tickets).find('button').click(),
    hideDetails: (tickets) => cy.wrap(tickets).find('button').click(),
    ticketHeader: (ticket) =>
        cy.wrap(ticket).find('.ui-ticketDetails__headerButton__title'),
    ticketSummary: (ticket) =>
        cy.wrap(ticket).find('.ui-ticketDetails__metaDataitem'),
    ticketDetails: (ticket) =>
        cy.wrap(ticket).find('.ui-ticketDetails__content'),
    ticketIconIsWaiting: (ticket) => {
        cy.wrap(ticket)
            .find('.ui-ticketDetails__headerButton__icon')
            .find('path')
            .each(($path, index, $paths) => {
                expect($path).to.have.attr('fill', '#007C92');
            });
    },
    ticketIconIsValid: (ticket) => {
        cy.wrap(ticket)
            .find('.ui-ticketDetails__headerButton__icon')
            .find('path')
            .each(($path, index, $paths) => {
                expect($path).to.have.attr('fill', '#A2AD00');
            });
    },
    ticketIsCollapsed: (ticket, isCollapsed) => {
        if (isCollapsed) {
            cy.wrap(ticket).should('not.have.class', 'ui-ticketDetails--open');
            cy.wrap(ticket)
                .find('.ui-ticketDetails__headerButton__toggleText')
                .should('contain', 'Vis detaljer');
        } else {
            cy.wrap(ticket).should('have.class', 'ui-ticketDetails--open');
            cy.wrap(ticket)
                .find('.ui-ticketDetails__headerButton__toggleText')
                .should('contain', 'Skjul detaljer');
        }
    },

    accountInfo: () => cy.get('.sidebar').find('.ui-labelItem'),
    addTravelCard: () =>
        cy.get('.sidebar').find('button').contains('Legg til t:kort '),
    travelCard: () => cy.get('.sidebar').find('.ui-travelCardText'),

    buyPeriodTicket: () =>
        cy.get('.sidebar').find('a').contains('Kjøp ny periodebillett'),
    buyCarnetTicket: () =>
        cy.get('.sidebar').find('a').contains('Kjøp nytt klippekort')
};
