export const myprofile = {
  editPhoneNumber: () => cy.get("button").contains("Endre telefonnummer").click(),
  addTravelCard: () => cy.get("button").contains("Legg til t:kort")
};
