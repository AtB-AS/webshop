export const overview = {
  tickets: () => cy.get(".ui-ticketDetails"),
  ticket: (tickets, order_id) => cy.wrap(tickets).find("button#" + order_id),
  ticketDetails: order_id => cy.get("#" + order_id + "region")
};
