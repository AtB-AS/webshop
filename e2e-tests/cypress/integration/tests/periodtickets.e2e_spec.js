import { menu, verify } from '../pageobjects/common.pageobject';
import { newTicket, options, traveller, travelTime2, zone } from '../pageobjects/buyticket.pageobject';

describe('period ticket purchase', () => {
    beforeEach(function () {
        cy.visitMainAsAuthorized();

        cy.intercept("**/ticket/v1/search/zones").as("zones")
        menu.buyPeriodTicket().click();
        cy.wait("@zones")
        verify.verifyHeader('h2', 'Kjøp ny periodebillett');
    });

    it.only('should display default values', () => {
        //Type
        newTicket.travelType().should("contain", "Buss og trikk")

        //Product
        newTicket.productSection().then($product => {
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

    //TODO KOM HIT
    it('summary should be disabled for not existing travel card', () => {})

    it('summary should be enabled for existing travel card', () => {})

    it('exiting summary should remembered the values', () => {})

    it('changing ticket type should update the offer and summary', () => {
        //also verify the summary
    })

    it('changing traveller should update the offer and summary', () => {
        //also verify the summary
    })

    it('changing zones should update the offer and summary', () => {
        //also verify the summary
    })

    it('changing time to travel should update the offer and summary', () => {
        //verify the summary
    })
})