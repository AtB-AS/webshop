export const history = {
    ticket: (order_id) =>
        cy.get("button[id='ATB:FareContract:" + order_id + "'")
};
