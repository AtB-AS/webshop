export const menu = {
  myProfile: () => cy.get("nav").find("a").contains("Min profil"),
  history: () => cy.get("nav").find("a").contains("Kjøpshistorikk"),
  buyTicket: () => cy.get("nav").find("a").contains("Kjøp billett"),
  startPage: () => cy.get("h1.pageHeader__logo")
};

export const verify = {
  verifyHeader: (header, text) => cy.get(header).contains(text)
}
