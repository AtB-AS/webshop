import { menu, verify } from '../pageobjects/common.pageobject';
import {
    newTicket,
    options,
    products,
    summary,
    traveller, travelTime,
    zone
} from '../pageobjects/buyticket.pageobject';
import { myprofile } from '../pageobjects/myprofile.pageobject';

describe('period ticket purchase', () => {
    beforeEach(function () {
        cy.visitMainAsAuthorized();

        cy.intercept("**/ticket/v1/search/zones").as("zones")
        menu.buyPeriodTicket().click();
        cy.wait("@zones")
        verify.verifyHeader('h2', 'Kjøp ny periodebillett');
    });

    it('should display default ticket parameters', () => {
        //Type
        newTicket.travelType().should("contain", "Buss og trikk")

        //Product
        newTicket.productsSection().then($product => {
            options.areVisible($product, true)
            options.value($product).should("contain", "30-dagersbillett")
        })

        //Traveller
        newTicket.travellerSection().then($traveller => {
            options.areVisible($traveller, false)
            options.value($traveller).should("contain", "1 Voksen")
        })

        //Travel time
        newTicket.travelTimeSection().then($time => {
            options.areVisible($time, false)
            options.value($time).should("contain", "Kjøpstidspunkt")
        })

        //Zones
        newTicket.departureZoneSection().should("be.visible")
        zone.departureZone().should("have.value", "ATB:TariffZone:1")
        zone.departureZoneTariff("ATB:TariffZone:1").should("have.text", "A")
        newTicket.arrivalZoneSection().should("be.visible")
        zone.arrivalZone().should("have.value", "ATB:TariffZone:1")
        zone.arrivalZoneTariff("ATB:TariffZone:1").should("have.text", "A")

        //Price
        newTicket.price()
            .should("contain", "860,00")
            .and("contain", "kr")
        newTicket.mva()
            .should("contain", "51,60")
    })

    it('summary should be enabled for existing travel card', () => {
        newTicket.infoText().should("not.contain", "Legg til et t:kort før kjøp av billett")
        newTicket.goToSummaryButton().should("not.have.class", "ui-button--disabled")
        newTicket.goToSummary()
        verify.verifyHeader('h2', 'Oppsummering');
    })

    it('summary should show default ticket parameters', () => {
        newTicket.goToSummary()

        //Verify defaults
        summary.ticketDetails("Billettype").should("contain", "Periodebillett")
        summary.ticketDetails("Reisetype").should("contain", "Buss / trikk")
        summary.ticketDetails("Periode").should("contain", "30-dagersbillett")
        summary.ticketDetails("Reisende").should("contain", "1 Voksen")
        summary.ticketDetails("Sone").should("contain", "Reise i 1 sone (A)")
        summary.ticketDetails("Gyldig fra").should("contain", "Kjøpstidspunkt")
        summary.ticketDetails("Gyldig til").should("contain", getFutureDate(30))
        summary.price().should("contain", "860,00")
    })

    it('valid to date in summary should reflect the product', () => {
        // 30
        products.set("30-dagersbillett")
        newTicket.goToSummary()
        summary.ticketDetails("Periode").should("contain", "30-dagersbillett")
        summary.ticketDetails("Gyldig til").should("contain", getFutureDate(30))

        // 180
        summary.back()
        products.set("180-dagersbillett")
        cy.wait("@zones")
        newTicket.goToSummary()
        summary.ticketDetails("Periode").should("contain", "180-dagersbillett")
        summary.ticketDetails("Gyldig til").should("contain", getFutureDate(180))

        // 7
        summary.back()
        products.set("7-dagersbillett")
        cy.wait("@zones")
        newTicket.goToSummary()
        summary.ticketDetails("Periode").should("contain", "7-dagersbillett")
        summary.ticketDetails("Gyldig til").should("contain", getFutureDate(7))
    })

    it('leaving the summary should remember changed ticket parameters', () => {
        const trav = 'Barn'
        const prod = '7-dagersbillett'

        //Set non-default values
        products.set(prod)
        traveller.showOptions()
        traveller.set(trav)

        //Go to summary and back
        newTicket.goToSummary()
        verify.verifyHeader('h2', 'Oppsummering');
        summary.back()

        //Verify previously set values
        //Product
        newTicket.productsSection().then($product => {
            options.value($product).should("contain", prod)
        })

        //Traveller
        newTicket.travellerSection().then($traveller => {
            options.value($traveller).should("contain", "1 " + trav)
        })
    })

    it('changing ticket product should update the offer and summary', () => {
        let currentOffer = 860
        const prod = '7-dagersbillett'

        //Change
        products.set(prod)
        cy.wait("@zones")

        //Verify
        newTicket.price().then($price => {
            let price = parseInt($price.text().replace(".", "").split(",")[0])
            expect(price).to.not.eq(currentOffer)
            expect(price).to.be.lt(currentOffer)
            currentOffer = price
        })
        newTicket.goToSummary()
        summary.price().then($price => {
            let price = parseInt($price.text().replace(".", "").split(",")[0])
            expect(price).to.be.eq(currentOffer)
        })
        summary.ticketDetails("Periode").should("contain", prod)
        summary.back()
        newTicket.price().then($price => {
            let price = parseInt($price.text().replace(".", "").split(",")[0])
            expect(price).to.be.eq(currentOffer)
        })
    })

    it('changing traveller should update the offer and summary', () => {
        let currentOffer = 860
        const trav = 'Barn'

        //Change
        traveller.showOptions()
        traveller.set(trav)
        cy.wait("@zones")

        //Verify
        newTicket.price().then($price => {
            let price = parseInt($price.text().replace(".", "").split(",")[0])
            expect(price).to.not.eq(currentOffer)
            expect(price).to.be.lt(currentOffer)
            currentOffer = price
        })
        newTicket.goToSummary()
        summary.price().then($price => {
            let price = parseInt($price.text().replace(".", "").split(",")[0])
            expect(price).to.be.eq(currentOffer)
        })
        summary.ticketDetails("Reisende").should("contain", "1 " + trav)
        summary.back()
        newTicket.price().then($price => {
            let price = parseInt($price.text().replace(".", "").split(",")[0])
            expect(price).to.be.eq(currentOffer)
        })
    })

    it('changing zones should update the offer and summary', () => {
        let currentOffer = 860
        const arrZone = 'B1'

        //Change
        zone.arrivalZone().select(arrZone)
        cy.wait("@zones")

        //Verify
        newTicket.price().then($price => {
            let price = parseInt($price.text().replace(".", "").split(",")[0])
            expect(price).to.not.eq(currentOffer)
            expect(price).to.be.gt(currentOffer)
            currentOffer = price
        })
        newTicket.goToSummary()
        summary.price().then($price => {
            let price = parseInt($price.text().replace(".", "").split(",")[0])
            expect(price).to.be.eq(currentOffer)
        })
        summary.ticketDetails("Sone").should("contain", "Reise fra sone A til sone " + arrZone)
        summary.back()
        newTicket.price().then($price => {
            let price = parseInt($price.text().replace(".", "").split(",")[0])
            expect(price).to.be.eq(currentOffer)
        })
    })

    it('changing time to travel should not update the offer nor the summary', () => {
        let currentOffer = ''

        //Pre
        newTicket.price().then($price => {
            currentOffer = $price.text()
        })

        //Change
        travelTime.showOptions()
        travelTime.inFuture().click()
        cy.wait("@zones")

        //Verify
        newTicket.price().then($price => {
            expect($price.text()).to.eq(currentOffer)
        })
        newTicket.goToSummary()
        summary.price().then($price => {
            expect($price.text()).to.eq(currentOffer)
        })
        summary.ticketDetails("Gyldig fra").should("not.contain", "Kjøpstidspunkt")
        summary.ticketDetails("Gyldig fra").should("contain", getFutureDate(0))
        summary.back()
        newTicket.price().then($price => {
            expect($price.text()).to.eq(currentOffer)
        })
    })

    //Existing future ticket is starting at 12:00
    it('should show warning for overlapping tickets', () => {
        const validFrom = Cypress.env('futureTicketStartYear') + "-" + Cypress.env('futureTicketStartMonth') + "-" + Cypress.env('futureTicketStartDay')

        //Change date to Cypress.env.futureTicketStartX
        travelTime.showOptions()
        travelTime.inFuture().click()
        cy.wait("@zones")

        travelTime.date().type(validFrom)
        travelTime.time().type("23:00")

        newTicket.warning()
            .should("contain", "Du har allerede en billett i dette tidsrommet")
    })

    it('should validate time to start', () => {
        travelTime.showOptions()
        travelTime.inFuture().click()
        cy.wait("@zones")

        //Back in time - hours
        travelTime.time().type("00:00")
        travelTime.validityError()
            .should("contain", "Starttidspunkt kan ikke være før nåværende tid og dato")

        //Back in time - days
        travelTime.time().type("23:59")
        travelTime.validityError()
            .should("not.exist")

        travelTime.date().type(getFutureDateInInputFormat(-1))
        travelTime.validityError()
            .should("contain", "Starttidspunkt kan ikke være før nåværende tid og dato")

        //90+ days forward
        travelTime.date().type(getFutureDateInInputFormat(91))
        travelTime.validityError()
            .should("contain", "Starttidspunkt kan ikke være mer enn 90 dager fram i tid")
    })
})

function getFutureDate(incrDays){
    let myDate = new Date();
    myDate.setDate(myDate.getDate() + incrDays);

    let dd = myDate.getDate();
    let mm = myDate.getMonth()+1;
    let yyyy = myDate.getFullYear();

    if(dd<10){dd='0'+dd}
    if(mm<10){mm='0'+mm}

    return dd+'.'+mm+'.'+yyyy
}

function getFutureDateInInputFormat(incrDays){
    let myDate = new Date();
    myDate.setDate(myDate.getDate() + incrDays);

    let dd = myDate.getDate();
    let mm = myDate.getMonth()+1;
    let yyyy = myDate.getFullYear();

    if(dd<10){dd='0'+dd}
    if(mm<10){mm='0'+mm}

    return yyyy +'-' + mm + '-' + dd
}