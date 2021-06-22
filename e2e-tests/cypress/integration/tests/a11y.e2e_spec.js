import { auth } from '../pageobjects/authentication.pageobject.js'
import { menu, verify } from '../pageobjects/common.pageobject.js'
import { overview } from '../pageobjects/overview.pageobject.js'
import { myprofile } from '../pageobjects/myprofile.pageobject.js'
import { history } from '../pageobjects/history.pageobject.js'
import { buy, travelTime } from '../pageobjects/buyticket.pageobject.js'

/**
 * Accessibility checks using axe (https://github.com/dequelabs/axe-core)
 */

describe("a11y check authentication", () => {
    it("sms login", () => {
        cy.visitMainAsNotAuthorized()
        cy.a11yCheck(null, null)
    })
    it("email login", () => {
        cy.visitMainAsNotAuthorized()
        auth.useEmailAsLogIn()
        cy.a11yCheck(null, null)
    })
    it("email create", () => {
        cy.visitMainAsNotAuthorized()
        auth.useEmailAsLogIn()
        auth.createNewEmailProfile()
        cy.a11yCheck(null, null)
    })
})

describe("a11y check webshop overview", () => {
    after(() => {
        cy.logOut()
    })
    beforeEach(function() {
        cy.visitMainAsAuthorized()
    });

    it("overview", () => {
        cy.a11yCheck(null, null)
    })

    //Buy a ticket is configured in 'cypress.json'
    if (Cypress.env("withBuyTicket")) {
        it("overview with ticket", () => {
            let order_id = 'ERROR'

            //Only to navigate back to overview after buying a ticket
            menu.myProfile().click()
            verify.verifyHeader("h2", "Profilinformasjon")

            //Buy ticket
            cy
                .buyTicket()
                .then($order_id => {
                    //Set order_id to 'order_id' or ERROR
                    order_id = $order_id
                })
                .wait(5000)

            //Overview
            menu.startPage().click()
            verify.verifyHeader("h3", "Min profil")

            overview.tickets().then($tickets => {
                if (!order_id.includes('ERROR')) {
                    overview.ticket($tickets, order_id)

                    //Check for collapsed tickets
                    cy.a11yCheck(null, null)

                    //Check for open ticket
                    overview.ticket($tickets, order_id).click()
                    overview.ticketDetails(order_id)
                        .should("have.class", "ui-ticketDetails__content--open")
                    cy.a11yCheck(null, null)
                }
            })
        })
    }
})

describe("a11y check webshop my profile", () => {
    after(() => {
        cy.logOut()
    })
    beforeEach(function() {
        cy.visitMainAsAuthorized()
    });

    it("my profile", () => {
        menu.myProfile().click()
        verify.verifyHeader("h2", "Profilinformasjon")

        cy.a11yCheck(null, null)
    })

    it("my profile - change phone number", () => {
        menu.myProfile().click()
        verify.verifyHeader("h2", "Profilinformasjon")
        myprofile.editPhoneNumber()

        //TODO https://github.com/AtB-AS/webshop/issues/249
        cy.a11yCheck(null, {
            rules: {
                'color-contrast': { enabled: false }
            }
        })
    })

    it("my profile - change travel card", () => {
        menu.myProfile().click()
        verify.verifyHeader("h2", "Profilinformasjon")

        //Check on hover
        myprofile.addTravelCard().trigger('mouseover')
        cy.injectAxe().then(() => {
            cy.a11yCheck(null, null)
        })

        //Check on editing mode
        myprofile.addTravelCard().click()
        //TODO https://github.com/AtB-AS/webshop/issues/249
        cy.a11yCheck(null, {
            rules: {
                'color-contrast': { enabled: false }
            }
        })
    })

    it("my profile - change consent", () => {
        cy.intercept("POST", "**/webshop/v1/consent").as("consent")

        menu.myProfile().click()
        verify.verifyHeader("h2", "Profilinformasjon")

        myprofile.consent().check()
        cy.wait("@consent")
        cy.injectAxe().then(() => {
            cy.a11yCheck(null, null)
        })

        myprofile.consent().uncheck()
        cy.wait("@consent")
        cy.injectAxe().then(() => {
            cy.a11yCheck(null, null)
        })
    })
})

describe("a11y check webshop ticket history", () => {
    after(() => {
        cy.logOut()
    })
    beforeEach(function() {
        cy.visitMainAsAuthorized()
    });

    it("ticket history", () => {
        const order_id = "XHUZBHFS"

        menu.history().click()
        verify.verifyHeader("h2", "Kjøpshistorikk")
        history.ticket(order_id)

        cy.a11yCheck(null, null)
    })

    it("ticket history details", () => {
        const order_id = "XHUZBHFS"

        menu.history().click()
        verify.verifyHeader("h2", "Kjøpshistorikk")
        history.ticket(order_id).click()

        cy.a11yCheck(null, null)
    })
})

describe("a11y check webshop buy ticket", () => {
    after(() => {
        cy.logOut()
    })
    beforeEach(function() {
        cy.visitMainAsAuthorized()
    });

    it("period ticket", () => {
        menu.buyPeriodTicket().click()
        verify.verifyHeader("h2", "Kjøp ny periodebillett")

        cy.a11yCheck(null, null)
    })

    it("period ticket - open categories", () => {
        menu.buyPeriodTicket().click()
        verify.verifyHeader("h2", "Kjøp ny periodebillett")

        buy.ticketCategoryDetails().then($categoryDetails => {
            if (!$categoryDetails.hasClass('ui-group__content--open')) {
                buy.ticketCategory().click()
            }
            buy.ticketProduct("30-dagersbillett")
                .should("be.visible")
        })

        cy.a11yCheck(null, null)
    })

    it("period ticket - open travellers", () => {
        menu.buyPeriodTicket().click()
        verify.verifyHeader("h2", "Kjøp ny periodebillett")

        buy.travellerDetails().then($travellerDetails => {
            if (!$travellerDetails.hasClass('ui-group__content--open')) {
                buy.travellers().click()
            }
            buy.traveller("Adult")
                .should("be.visible")
        })

        cy.a11yCheck(null, null)
    })

    it("period ticket - open time picker", () => {
        menu.buyPeriodTicket().click()
        verify.verifyHeader("h2", "Kjøp ny periodebillett")

        //Check travel now
        buy.travelTimeDetails().then($travelTimeDetails => {
            if (!$travelTimeDetails.hasClass('ui-group__content--open')) {
                buy.travelTime().click()
            }
            travelTime.now().should("be.visible")
        })
        cy.a11yCheck(null, null)

        //Check travel in future
        travelTime.inFuture().check({ force: true })
        travelTime.date()
            .should("be.visible")
        cy.a11yCheck(null, null)

        //Check date picker
        travelTime.date().click()
        cy.a11yCheck(null, null)

        //Check time picker
        travelTime.time().click()
        cy.a11yCheck(null, null)
    })

    it("carnet ticket", () => {
        menu.buyCarnetTicket().click()
        verify.verifyHeader("h2", "Kjøp nytt klippekort")

        cy.a11yCheck(null, null)
    })

    it("carnet ticket - open travellers", () => {
        menu.buyCarnetTicket().click()
        verify.verifyHeader("h2", "Kjøp nytt klippekort")

        buy.travellerDetails().then($travellerDetails => {
            if (!$travellerDetails.hasClass('ui-group__content--open')) {
                buy.travellers().click()
            }
            buy.traveller("Adult")
                .should("be.visible")
        })

        cy.a11yCheck(null, null)
    })

})