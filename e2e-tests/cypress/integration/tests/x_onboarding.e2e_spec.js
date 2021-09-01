import { auth, firebase } from '../pageobjects/authentication.pageobject';
import { email, menu, verify } from '../pageobjects/common.pageobject';
import { mytickets } from '../pageobjects/mytickets.pageobject';
import { myprofile } from '../pageobjects/myprofile.pageobject';
import {
    onboarding,
    onboardingStep1,
    onboardingStep2,
    onboardingStep3
} from '../pageobjects/onboarding.pageobject';

// Visiting another domain necessitates setting 'chromeWebSecurity=false' in 'cypress.json
// ==> 'Only for chrome family of browsers
if (Cypress.isBrowser({ family: 'chromium' })) {
    //Fail all tests if one fails
    //describe('Email confirmation', { failFast: { enabled: true } }, () => {
    describe('onboarding', () => {
        let userEmail;
        let userPassword = 'qwer1234';
        let newUserPassword = 'qwer12345678';

        // get and check the test email only once before the tests
        before(() => {
            cy.task('getUserEmail').then((email) => {
                expect(email).to.be.a('string');
                userEmail = email;
                //for debug
                cy.writeFile('cypress/fixtures/user.txt', userEmail);
            });
        });

        it('should create user and check verification email', () => {
            cy.visitMainAsNotAuthorized();
            auth.useEmailAsLogIn();
            auth.createNewEmailProfile();

            auth.setEmail(userEmail);
            auth.setPassword(userPassword);
            auth.createProfile();

            //wait to ensure email is sent and received
            cy.wait(10000);

            cy.task('getLastEmail')
                .its('subject')
                .then((subject) => {
                    expect(subject).to.include('Verify your email');
                });
            cy.task('getLastEmail')
                .its('html')
                .then((html) => {
                    //Load email as document
                    cy.document({ log: false }).invoke(
                        { log: false },
                        'write',
                        html
                    );
                });

            //verify email and save url for next test case
            email
                .body()
                .should(
                    'contain',
                    'Follow this link to verify your email address.'
                );
            email.verifyUrl().then(($elem) => {
                //https://github.com/cypress-io/cypress/issues/944
                cy.writeFile('cypress/fixtures/verifyUrl.txt', $elem.text());
            });
        });

        it('should confirm the created user', () => {
            cy.intercept(
                'POST',
                '**/identitytoolkit/v3/relyingparty/setAccountInfo**'
            ).as('setAccountInfo');

            //Possibility to catch 'cross origin error' when confirming the email
            /*
            Cypress.on('fail', (error, runnable) => {
                //debugger
                //throw error // throw error to have test still fail
            })
             */

            cy.readFile('cypress/fixtures/verifyUrl.txt').then(($url) => {
                expect($url).to.include('firebaseapp');
                expect($url).to.include('verifyEmail');
                cy.visit($url);
            });

            firebase.confirm();
            cy.wait('@setAccountInfo').wait(500);
            cy.url().should('contain', Cypress.config('baseUrl'));
        });

        it('should onboard new user', () => {
            const step1 = 'Profilinformasjon';
            const step2 = 'Samtykker';
            const step3 = 'Legg til t:kort';
            const firstname = 'Ext';
            const lastname = 'Joe';
            const phoneNumberError = '123456789000';
            const phoneNumber = '12345678';
            const phoneNumberFormatted = '+47 12 34 56 78';
            const travelCardError = '1234567891234567';
            const travelCardError2 = '1616006091234567';
            //const travelCard = '1616006912504173'

            cy.intercept('POST', '**/webshop/v1/register').as('registerReq');
            cy.intercept('POST', '**/webshop/v1/consent').as('consentReq');
            cy.intercept('POST', '**/webshop/v1/travelcard').as(
                'travelcardReq'
            );
            cy.intercept('POST', '**/v1/token**').as('refreshToken');

            cy.visit('');

            //Check maneuvering forward - including a11y
            onboarding.stepTitle().should('contain', step1);
            //TODO https://github.com/AtB-AS/webshop/issues/367
            //cy.a11yCheck(null, null);
            cy.a11yCheck(null, {
                rules: {
                    'aria-progressbar-name': { enabled: false }
                }
            });
            onboarding.back().should('not.exist');
            onboarding.skip().should('exist').and('contain', 'Hopp over');

            onboarding.skip().click();
            onboarding.stepTitle().should('contain', step2);
            //TODO https://github.com/AtB-AS/webshop/issues/367
            //cy.a11yCheck(null, null);
            cy.a11yCheck(null, {
                rules: {
                    'aria-progressbar-name': { enabled: false }
                }
            });
            onboarding.back().should('contain', 'Tilbake');
            onboarding.skip().should('contain', 'Hopp over');

            onboarding.skip().click();
            onboarding.stepTitle().should('contain', step3);
            //TODO https://github.com/AtB-AS/webshop/issues/367
            //cy.a11yCheck(null, null);
            cy.a11yCheck(null, {
                rules: {
                    'aria-progressbar-name': { enabled: false }
                }
            });
            onboarding.back().should('contain', 'Tilbake');
            onboarding.skip().should('contain', 'Hopp over');

            //Check maneuvering backward
            onboarding.back().click();
            onboarding.stepTitle().should('contain', step2);

            onboarding.back().click();
            onboarding.stepTitle().should('contain', step1);

            //Step 1
            onboardingStep1.setFirstname(firstname);
            onboardingStep1.setLastname(lastname);
            // TODO: https://github.com/AtB-AS/webshop/issues/290#issuecomment-877060729
            //onboardingStep1.setPhoneNumber(phoneNumberError);
            onboardingStep1.setPhoneNumber(phoneNumber);
            onboardingStep1.email().should('have.value', userEmail);
            /*
            // TODO: https://github.com/AtB-AS/webshop/issues/290#issuecomment-877060729
            onboardingStep1.nextStep();
            onboardingStep1.emailError(true);
            onboardingStep1
                .emailErrorMsg()
                .should('contain', 'Telefonnummeret må bestå av 8 siffer');
            onboardingStep1.setPhoneNumber('{selectall}{del}' + phoneNumber);
            onboardingStep1.emailError(false);
             */
            onboardingStep1.nextStep();
            cy.wait('@registerReq').then((req) => {
                expect(req.response.statusCode).to.eq(201);
                //expect(req.response.body.email).to.include(userEmail)
            });

            //Step 2
            onboarding.stepTitle().should('contain', step2);
            onboardingStep2
                .emailConsentLabel()
                .should('contain', 'AtB kan kontakte meg på e-post');
            onboardingStep2.emailConsent().should('not.be.checked');
            onboardingStep2.emailConsent().check();
            onboardingStep2
                .notificationConsentLabel()
                .should('contain', 'AtB kan varsle meg om billetter');
            onboardingStep2.notificationConsent().should('not.be.checked');
            onboardingStep2.notificationConsent().check();
            onboardingStep2.saveConsents();
            cy.wait('@consentReq').then((req) => {
                expect(req.response.statusCode).to.eq(201);
                //expect(req.response.body.email).to.eq(userEmail)
                //expect(req.response.body.choice).to.eq(true)
            });
            //Re-thinking the consent
            onboarding.back().click();
            onboardingStep2.emailConsent().should('be.checked');
            onboardingStep2.emailConsent().uncheck();
            onboardingStep2.notificationConsent().should('be.checked');
            onboardingStep2.notificationConsent().uncheck();
            onboardingStep2.saveConsents();
            cy.wait('@consentReq').then((req) => {
                expect(req.response.statusCode).to.eq(201);
                //expect(req.response.body.email).to.not.eq(userEmail)
                //expect(req.response.body.choice).to.eq(false)
            });

            //Step 3
            onboarding.stepTitle().should('contain', step3);
            onboardingStep3
                .travelCard()
                .should('have.attr', 'placeholder', 'Skriv inn t:kortnummer')
                .and('have.value', '');
            //Wrong travel card
            onboardingStep3.setTravelCard(travelCardError);
            onboardingStep3.addTravelCard();
            onboardingStep3.travelCardError(true);
            onboardingStep3
                .travelCardErrorMsg()
                .should('contain', 'må starte på 1616 0060');
            onboardingStep3.setTravelCard('{selectall}{del}' + travelCardError2);
            onboardingStep3.addTravelCard();
            cy.wait('@travelcardReq').then((req) => {
                expect(req.response.statusCode).to.eq(400);
                //expect(req.response.body).to.include("Travel card id is incorrect")
            });
            //Remove travel card error
            onboardingStep3.travelCardError(true);
            onboardingStep3
                .travelCardErrorMsg()
                .should('contain', 'tastet inn feil t:kortnummer');
            onboardingStep3.setTravelCard('{selectall}{del}');
            onboardingStep3.addTravelCard();
            onboardingStep3
                .travelCardErrorMsg()
                .should('contain', 'kan ikke være tomt');
            onboardingStep3.skipTravelCard();

            //Verify
            cy.wait("@refreshToken").wait(500)
            cy.reload()
            verify.verifyHeader('h2', 'Mine billetter');
            mytickets
                .accountInfo()
                .should('contain', firstname + ' ' + lastname)
                .and('contain', phoneNumberFormatted);

            menu.myProfile().click();
            verify.verifyHeader('h2', 'Profilinformasjon');
            myprofile.firstName().should('contain', firstname);
            myprofile.lastName().should('contain', lastname);
            myprofile.phoneNumber().should('contain', phoneNumber);
            myprofile.logInMethod().should('contain', userEmail);
            myprofile.emailConsent().should('not.be.checked');
        });

        it('should be able to log in', () => {
            //Ensure logged out
            cy.visitMainAsNotAuthorized();

            //Log in
            cy.visitMainAsAuthorized(userEmail, userPassword);

            menu.myProfile().click();
            verify.verifyHeader('h2', 'Profilinformasjon');
            myprofile.logInMethod().should('contain', userEmail);
        });

        it('should change password and receive email', () => {
            cy.visitMainAsNotAuthorized();
            auth.useEmailAsLogIn();
            auth.forgotPassword();
            auth.info().should('contain', 'tilbakestille passord');
            auth.setEmail(userEmail);
            auth.resetPassword();

            //wait to ensure email is sent and received
            cy.wait(10000);

            cy.task('getLastEmail')
                .its('subject')
                .then((subject) => {
                    expect(subject).to.include('Reset your password');
                });
            cy.task('getLastEmail')
                .its('html')
                .then((html) => {
                    cy.document({ log: false }).invoke(
                        { log: false },
                        'write',
                        html
                    );
                });

            //verify email and save url for next test case
            email.body().should('contain', 'Follow this link to reset');
            email.verifyUrl().then(($elem) => {
                //https://github.com/cypress-io/cypress/issues/944
                cy.writeFile('cypress/fixtures/resetUrl.txt', $elem.text());
            });
        });

        it('should set new password', () => {
            cy.intercept(
                'POST',
                '**/identitytoolkit/v3/relyingparty/resetPassword**'
            ).as('resetPassword');

            //Possibility to catch 'cross origin error' when confirming the email
            /*
            Cypress.on('fail', (error, runnable) => {
                //debugger
                //throw error // throw error to have test still fail
            })
             */

            cy.readFile('cypress/fixtures/resetUrl.txt').then(($url) => {
                expect($url).to.include('firebaseapp');
                expect($url).to.include('resetPassword');
                cy.visit($url);
            });

            firebase.infoText().should('contain', userEmail);
            firebase.setNewPassword(newUserPassword);
            firebase.savePassword();
            cy.wait('@resetPassword').wait(500);
            firebase.title().should('contain', 'Password changed');
            firebase.confirm();
            cy.url().should('contain', Cypress.config('baseUrl'));
        });

        it('should be able to log in with new password', () => {
            //Ensure logged out
            cy.visitMainAsNotAuthorized();

            //Log in
            cy.visitMainAsAuthorized(userEmail, newUserPassword);

            menu.myProfile().click();
            verify.verifyHeader('h2', 'Profilinformasjon');
            myprofile.logInMethod().should('contain', userEmail);
        });
    });
}
