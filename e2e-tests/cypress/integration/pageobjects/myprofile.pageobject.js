export const myprofile = {
  firstName: () => cy.get(".ui-textContainer").contains("Fornavn").parents(".ui-labelItem"),
  lastName: () => cy.get(".ui-textContainer").contains("Etternavn").parents(".ui-labelItem"),
  phoneNumber: () => cy.get(".ui-textContainer").contains("Telefonnummer").parents(".ui-labelItem"),
  logInMethod: () => cy.get("h2.ui-section__headerTitle").contains("Innloggingsmetode").parents(".ui-section__item"),

  editPhoneNumber: () => cy.get("button").contains("Endre telefonnummer").click(),
  addTravelCard: () => cy.get("button").contains("Legg til t:kort"),
  consent: () => cy.get("input#consent1186")
};
