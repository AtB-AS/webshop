import { menu, verify } from '../pageobjects/common.pageobject';
import { myprofile } from '../pageobjects/myprofile.pageobject';

/*
 TravelCard for default user is 1616006933634710

 */

describe('my profile', () => {

    const emailConsentId = 1186
    const notificationConsentId = 1197
    let currentEmailConsent = false
    let currentNotificationConsent = false

    beforeEach(function () {
        cy.intercept("GET", "**/webshop/v1/consent").as("consent")

        cy.visitMainAsAuthorized();

        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');
        //Get current consent status
        cy.wait("@consent").then(req => {
            expect(req.response.statusCode).to.eq(200);
            const emailConsent = getConsent(req.response.body, emailConsentId)
            currentEmailConsent = emailConsent.choice
            const notificationConsent = getConsent(req.response.body, notificationConsentId)
            currentNotificationConsent = notificationConsent.choice
        })
    });

    it('should change profile name', () => {
        cy.intercept('PATCH', '**/webshop/v1/profile').as('profile');

        const firstname = 'Mr';
        const lastname = 'Test ' + randomNumbers(8);
        let currFirstName = ''

        //Pre
        myprofile.firstName().then($name => {
            currFirstName = $name.text()
        })

        //Empty
        myprofile.editProfileName()
        myprofile.setFirstName('')
        myprofile.saveValue()
        //TODO Error msg: https://github.com/AtB-AS/webshop/issues/220
        myprofile.cancel()

        //Verify
        myprofile.firstName().then($name => {
            expect($name.text()).to.eq(currFirstName)
        })

        //New name
        myprofile.editProfileName()
        myprofile.setFirstName(firstname)
        myprofile.setLastName(lastname)
        myprofile.saveValue()
        cy.wait("@profile")

        //Verify
        myprofile.firstName().should("contain", firstname)
        myprofile.lastName().should("contain", lastname)
    });

    it('should change phone number', () => {
        cy.intercept('PATCH', '**/webshop/v1/profile').as('profile');

        const phoneNumber = "9" + randomNumbers(7);
        const foreignPhoneNumber = '+1233444335354'
        let currPhoneNumber = '12345678'

        //Pre
        myprofile.setPhoneNumber(currPhoneNumber)

        //Cancel
        myprofile.editPhoneNumber()
        myprofile.typePhoneNumber('+4711111111')
        myprofile.cancel()

        //Verify
        myprofile.phoneNumber().should("contain", currPhoneNumber)

        //Empty
        myprofile.setPhoneNumber('')
        myprofile.phoneNumber().should("contain", "ikke utfylt")

        //Error
        myprofile.editPhoneNumber()
        myprofile.typePhoneNumber('+47')
        myprofile.saveValue()
        cy.wait('@profile');
        //TODO Error msg: https://github.com/AtB-AS/webshop/issues/290
        myprofile.phoneError().should("contain", "feil med tjenesten")
        myprofile.cancel()

        //Foreign number
        myprofile.setPhoneNumber(foreignPhoneNumber)
        myprofile.phoneNumber().should("contain", foreignPhoneNumber)

        //New NO phone number
        myprofile.setPhoneNumber(phoneNumber)
        myprofile.phoneNumber().should("contain", "+47" + phoneNumber)
    });

    it('authentication method should be correct', () => {
        myprofile.logInMethod()
            .should("contain", "E-post og passord")
            .and("contain", Cypress.env("email"))
    })

    it('should list stored payment cards', () => {
        myprofile.storedPayment("Visa").should("contain", "Visa, **** 0004")
        myprofile.storedPaymentIcon("Visa").should("have.attr", "src", "images/paymentcard-visa.svg")
        myprofile.storedPaymentExpiry("Visa").should("contain", "Utløpsdato 08/24")

        myprofile.storedPayment("MasterCard").should("contain", "MasterCard, **** 0000")
        myprofile.storedPaymentIcon("MasterCard").should("have.attr", "src", "images/paymentcard-mastercard.svg")
        myprofile.storedPaymentExpiry("MasterCard").should("contain", "Utløpsdato 06/24")
    })

    it('should get a warning when removing a stored payment card', () => {
        myprofile.removeStoredPayment("Visa").click()
        myprofile.storedPaymentRemovalWarning()
            .should("contain", "Er du sikker på at du vil fjerne dette kortet?")
        myprofile.cancel()
    })

    it('should get a warning when removing the travel card', () => {
        myprofile.travelCard()
            .should("contain", "69 3363471")

        myprofile.removeTravelCardButton().click()
        myprofile.removeTravelCardWarning()
            .should("contain", "Er du sikker på at du ønsker å fjerne dette t:kortet")
        myprofile.cancel()

        myprofile.travelCard()
            .should("contain", "69 3363471")
    })

    it('should be able to change email consent', () => {
        cy.intercept("POST", "**/webshop/v1/consent").as("postConsent")

        //Change consent
        cy.window().then($win => {
            //Current consent is TRUE --> change to FALSE
            if (currentEmailConsent){
                myprofile.emailConsent().uncheck()
                cy.wait("@postConsent").then($req => {
                    const choice = $req.response.body.choice
                    const email = $req.response.body.email

                    expect($req.response.statusCode).to.eq(201);
                    currentEmailConsent = choice
                    expect(currentEmailConsent).to.eq(false)
                    expect(email).to.be.empty
                })

                //Verify
                menu.goToStartPage()
                verify.verifyHeader('h2', 'Mine billetter')
                menu.myProfile().click();
                verify.verifyHeader('h2', 'Min profil')
                cy.wait("@consent").then($req => {
                    expect($req.response.statusCode).to.eq(200);
                    const consent = getConsent($req.response.body, emailConsentId)
                    expect(consent.choice).to.eq(false)
                    expect(consent.email).to.be.empty
                })
            }
            //Current consent is FALSE --> change to TRUE
            else {
                myprofile.emailConsent().check()
                cy.wait("@postConsent").then($req => {
                    const choice = $req.response.body.choice
                    const email = $req.response.body.email

                    expect($req.response.statusCode).to.eq(201);
                    currentEmailConsent = choice
                    expect(currentEmailConsent).to.eq(true)
                    expect(email).to.eq(Cypress.env("email"))
                })

                //Verify
                menu.goToStartPage()
                verify.verifyHeader('h2', 'Mine billetter')
                menu.myProfile().click();
                verify.verifyHeader('h2', 'Min profil')
                cy.wait("@consent").then($req => {
                    expect($req.response.statusCode).to.eq(200);
                    const consent = getConsent($req.response.body, emailConsentId)
                    expect(consent.choice).to.eq(true)
                    expect(consent.email).to.eq(Cypress.env("email"))
                })
            }
        })
    })

    it('should be able to change notification consent', () => {
        cy.intercept("POST", "**/webshop/v1/consent").as("postConsent")

        //Change consent
        cy.window().then($win => {
            //Current consent is TRUE --> change to FALSE
            if (currentNotificationConsent){
                myprofile.notificationConsent().uncheck()
                cy.wait("@postConsent").then($req => {
                    const choice = $req.response.body.choice
                    const email = $req.response.body.email

                    expect($req.response.statusCode).to.eq(201);
                    currentNotificationConsent = choice
                    expect(currentNotificationConsent).to.eq(false)
                    expect(email).to.be.empty
                })

                //Verify
                menu.goToStartPage()
                verify.verifyHeader('h2', 'Mine billetter')
                menu.myProfile().click();
                verify.verifyHeader('h2', 'Min profil')
                cy.wait("@consent").then($req => {
                    expect($req.response.statusCode).to.eq(200);
                    const consent = getConsent($req.response.body, notificationConsentId)
                    expect(consent.choice).to.eq(false)
                    expect(consent.email).to.be.empty
                })
            }
            //Current consent is FALSE --> change to TRUE
            else {
                myprofile.notificationConsent().check()
                cy.wait("@postConsent").then($req => {
                    const choice = $req.response.body.choice
                    const email = $req.response.body.email

                    expect($req.response.statusCode).to.eq(201);
                    currentNotificationConsent = choice
                    expect(currentNotificationConsent).to.eq(true)
                    expect(email).to.eq(Cypress.env("email"))
                })

                //Verify
                menu.goToStartPage()
                verify.verifyHeader('h2', 'Mine billetter')
                menu.myProfile().click();
                verify.verifyHeader('h2', 'Min profil')
                cy.wait("@consent").then($req => {
                    expect($req.response.statusCode).to.eq(200);
                    const consent = getConsent($req.response.body, notificationConsentId)
                    expect(consent.choice).to.eq(true)
                    expect(consent.email).to.eq(Cypress.env("email"))
                })
            }
        })
    })

    it('should include policy statement', () => {
        myprofile.policyStatement()
            .should("contain", "Les vår personvernerklæring")
            .and("have.attr", "href", "https://beta.atb.no/private-policy")
    })

    //Get the correct consent from a list of consents
    function getConsent(body, consentId) {
        for (let i = 0; i < body.length; i++){
            if (body[i].consentId === consentId){
                console.log("Consent " + consentId)
                return body[i]
            }
            else {
                console.log("NOT consent " + consentId)
            }
        }
        return null
    }

    function randomNumbers(number) {
        let rand = '';
        for (let i = 0; i < number; i++) {
            rand += Math.floor(Math.random() * 10);
        }
        return rand;
    }
});