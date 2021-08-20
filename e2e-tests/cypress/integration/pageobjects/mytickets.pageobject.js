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
    showDetails: (tickets) => cy.wrap(tickets).find('button.ui-ticketDetails__headerButton').click(),
    hideDetails: (tickets) => cy.wrap(tickets).find('button.ui-ticketDetails__headerButton').click(),
    ticketHeader: (ticket) =>
        cy.wrap(ticket).find('.ui-ticketDetails__headerButton__title'),
    carnetTicketHeader: (ticket) =>
        cy.wrap(ticket).find('.ui-ticketDetails__headerButton__title--carnet'),
    ticketSummary: (ticket) =>
        cy.wrap(ticket).find('.ui-ticketDetails__metaDataitem'),
    ticketDetails: (ticket) =>
        cy.wrap(ticket).find('.ui-ticketDetails__content'),
    ticketReceipt: (ticket) =>
        cy.wrap(ticket).find('.ui-ticketDetails__content').find("button").contains("Be om kvittering på e-post").parent("button"),
    ticketIconIsWaiting: (ticket) => {
        cy.wrap(ticket)
            .find('.ui-ticketDetails__headerButton__icon')
            .find("title")
            .should("contain", "Venter på billett")
    },
    carnetTicketIconIsCorrect: (ticket) => {
        cy.wrap(ticket)
            .find('.ui-ticketDetails__headerButton__icon')
            .find("title")
            .should("contain", "Billetter")
    },
    ticketIconIsValid: (ticket) => {
        cy.wrap(ticket)
            .find('.ui-ticketDetails__headerButton__icon')
            .find("title")
            .should("contain", "Gyldig billett")
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
