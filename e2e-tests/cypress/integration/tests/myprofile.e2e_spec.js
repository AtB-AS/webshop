import { menu, verify } from '../pageobjects/common.pageobject';
import { myprofile } from '../pageobjects/myprofile.pageobject';

/*
 TravelCard for default user is 3445454533634693
 */

describe('my profile', () => {

    let currentConsent = false

    beforeEach(function () {
        cy.intercept("GET", "**/webshop/v1/consent").as("consent")

        cy.visitMainAsAuthorized();

        menu.myProfile().click();
        verify.verifyHeader('h2', 'Min profil');
        //Get current consent status
        cy.wait("@consent").then(req => {
            expect(req.response.statusCode).to.eq(200);
            currentConsent = req.response.body[0].choice
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
        let currPhoneNumber = ''

        //Pre
        myprofile.phoneNumber().then($no => {
            currPhoneNumber = $no.text()
        })

        //Cancel
        myprofile.editPhoneNumber()
        myprofile.typePhoneNumber('+4711111111')
        myprofile.cancel()

        //Verify
        myprofile.phoneNumber().then($no => {
            expect($no.text()).to.eq(currPhoneNumber)
        })

        //Empty
        myprofile.setPhoneNumber('')
        myprofile.phoneNumber().should("contain", "ikke utfylt")

        //Error
        myprofile.editPhoneNumber()
        myprofile.typePhoneNumber('11')
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

    it('should get a warning when removing the travel card', () => {
        myprofile.travelCard()
            .should("contain", "45 3363469")

        myprofile.removeTravelCardButton().click()
        myprofile.removeTravelCardWarning()
            .should("contain", "Er du sikker på at du ønsker å fjerne dette t:kortet")
        myprofile.cancel()

        myprofile.travelCard()
            .should("contain", "45 3363469")
    })

    it('should be able to change consents', () => {
        cy.intercept("POST", "**/webshop/v1/consent").as("postConsent")

        //Change consent
        cy.window().then($win => {
            //Current consent is TRUE --> change to FALSE
            if (currentConsent){
                myprofile.consent().uncheck()
                cy.wait("@postConsent").then($req => {
                    const choice = $req.response.body.choice
                    const email = $req.response.body.email

                    expect($req.response.statusCode).to.eq(201);
                    currentConsent = choice
                    expect(currentConsent).to.eq(false)
                    expect(email).to.be.empty
                })

                //Verify
                menu.goToStartPage()
                verify.verifyHeader('h2', 'Mine billetter')
                menu.myProfile().click();
                verify.verifyHeader('h2', 'Min profil')
                cy.wait("@consent").then($req => {
                    expect($req.response.statusCode).to.eq(200);
                    expect($req.response.body[0].choice).to.eq(false)
                    expect($req.response.body[0].email).to.be.empty
                })
            }
            //Current consent is FALSE --> change to TRUE
            else {
                myprofile.consent().check()
                cy.wait("@postConsent").then($req => {
                    const choice = $req.response.body.choice
                    const email = $req.response.body.email

                    expect($req.response.statusCode).to.eq(201);
                    currentConsent = choice
                    expect(currentConsent).to.eq(true)
                    expect(email).to.eq(Cypress.env("email"))
                })

                //Verify
                menu.goToStartPage()
                verify.verifyHeader('h2', 'Mine billetter')
                menu.myProfile().click();
                verify.verifyHeader('h2', 'Min profil')
                cy.wait("@consent").then($req => {
                    expect($req.response.statusCode).to.eq(200);
                    expect($req.response.body[0].choice).to.eq(true)
                    expect($req.response.body[0].email).to.eq(Cypress.env("email"))
                })
            }
        })
    })

    it('should include policy statement', () => {
        myprofile.policyStatement()
            .should("contain", "Les vår personvernerklæring")
            .and("have.attr", "href", "https://beta.atb.no/private-policy")
    })

    function randomNumbers(number) {
        let rand = '';
        for (let i = 0; i < number; i++) {
            rand += Math.floor(Math.random() * 10);
        }
        return rand;
    }
});