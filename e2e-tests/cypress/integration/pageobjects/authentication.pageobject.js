export const auth = {
  useEmailAsLogIn: () => cy.get(".ui-section").find("a").contains("Jeg vil heller bruke e-post").click(),
  createNewEmailProfile: () => cy.get(".ui-section").find("a").contains("Opprett en ny profil").click()
};
