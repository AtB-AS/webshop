require('./styles/main.scss');

import firebase from 'firebase/app';
import 'firebase/firebase-auth';
import 'firebase/firebase-firestore';
import 'firebase/firebase-remote-config';
import 'wicg-inert';
import { v4 as uuidv4 } from 'uuid';

import { Elm } from '../elm/Main';
import './navigation';

const appConfig = require(`/orgs/${elmFlags.orgId}.json`)
const MAX_RETRY_ATTEMPTS = 3;

if (!elmFlags.isDevelopment && 'serviceWorker' in navigator) {
    window.addEventListener('load', () => {
        navigator.serviceWorker.register('/service-worker.js');
    });
}

// This is silly. Very hacky way to override xhr object prototype
// to always include uuid as AtbRequestID.
// This is to avoid annoying argument passing,
// state management propagation in Elm.
let originalOpen = XMLHttpRequest.prototype.open;
XMLHttpRequest.prototype.open = function (...args) {
    const [, url] = args;
    var res = originalOpen.apply(this, args);

    if (url.includes(elmFlags.baseUrl)) {
        this.setRequestHeader('Atb-Request-Id', uuidv4());
    }
    return res;
};

let installId = localStorage['Atb-Install-Id'];

if (!installId) {
    installId = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(
        /[xy]/g,
        function (c) {
            const r = (Math.random() * 16) | 0;
            const v = c === 'x' ? r : (r & 0x3) | 0x8;
            return v.toString(16);
        }
    );
    localStorage['Atb-Install-Id'] = installId;
}

console.log('Atb-Install-Id:', installId);

firebase.initializeApp(firebaseConfig);

// Closure data for unsubscribing on changes.
let unsubscribeFareContractSnapshot = null;
let unsubscribeFetchUserDataSnapshot = null;

let onboardingUser = null;
const db = firebase.firestore();
const remoteConfig = firebase.remoteConfig();
const app = Elm.Main.init({
    flags: Object.assign(
        {
            installId: installId,
            loggedIn: localStorage['loggedIn'] === 'loggedIn',
            showValidityWarning:
                localStorage.getItem('hideValidityWarning') != 'yes',
            localUrl: window.location.origin,
            orgConf: appConfig
        },
        elmFlags
    )
});

app.ports.bodyClass.subscribe(function (className) {
    document.body.className = className;
});

// Check if we are running on the next release
const isNext = location.hostname.indexOf('production--next-') !== -1;

function getRemoteConfigString(key) {
    // If we're on next, then try to fetch a next remote config property.
    // Otherwise fall back to the default.
    if (isNext) {
        const value = remoteConfig.getValue(key + '_next');

        if (value.getSource() !== 'static') {
            return value.asString();
        }
    }

    return remoteConfig.getString(key);
}

function fetchRemoteConfigData(port, key, prop) {
    if (!port) {
        return;
    }

    const value = getRemoteConfigString(key);

    if (typeof value !== 'string' || value.length < 1) {
        return;
    }

    const data = JSON.parse(value);

    if (typeof data !== 'object' || data === null) {
        return;
    }

    if (typeof prop === 'string' && data.hasOwnProperty(prop)) {
        port.send(data[prop]);
    } else {
        port.send(data);
    }
}

function sendRemoteConfigVatPercent(port) {
    if (!port) {
        return;
    }

    const value = remoteConfig.getNumber('vat_percent');
    if (value == null) {
        return;
    }

    app.ports.remoteConfigVatPercent.send(value);
}

// NOTE: Only change this for testing.
remoteConfig.settings.minimumFetchIntervalMillis = 3600000;
//remoteConfig.settings.minimumFetchIntervalMillis = 60000;
remoteConfig
    .fetchAndActivate()
    .then(() => {
        fetchRemoteConfigData(
            app.ports.remoteConfigFareProducts,
            'preassigned_fare_products_v2'
        );
        fetchRemoteConfigData(
            app.ports.remoteConfigUserProfiles,
            'user_profiles'
        );
        fetchRemoteConfigData(
            app.ports.remoteConfigTariffZones,
            'tariff_zones'
        );
        fetchRemoteConfigData(app.ports.remoteConfigConsents, 'consents');
        fetchRemoteConfigData(app.ports.remoteConfigPaymentTypes, 'payment_types', 'web');
        sendRemoteConfigVatPercent();
    })
    .catch((err) => {
        // ...
        console.log('[ERROR] remoteConfig.fetchAndActivate:', err);
    });

app.ports.signOutHandler.subscribe(async () => {
    clearRefreshToken();

    try {
        unsubscribeFareContractSnapshot && unsubscribeFareContractSnapshot();
        unsubscribeFareContractSnapshot = null;

        unsubscribeFetchUserDataSnapshot && unsubscribeFetchUserDataSnapshot();
        unsubscribeFetchUserDataSnapshot = null;

        // Remove loggedIn before signing out to avoid showing error
        // below on onAuthStateChanged.
        await firebase.auth().signOut();
    } catch (e) {
        console.error('[debug] Unable to logout: ', e);
    }
});

// Use device language
firebase.auth().useDeviceLanguage();

function updateElmWithLoggedOutAndCleanUp() {
    clearRefreshToken();
    localStorage.removeItem('loggedIn');

    app.ports.signInError.send({
        code: -1,
        message: 'Du er utlogget.'
    });
}

// Observer on user info
firebase.auth().onAuthStateChanged((user) => {
    if (user) {
        fetchAuthInfo(user);
    } else if (localStorage.getItem('loggedIn')) {
        // Not logged in remove logged in flag to be safe to avoid infinite loading.
        console.log('[debug] Logged out due to stale state.');
        updateElmWithLoggedOutAndCleanUp();
    }
});

let refreshTokenTimer = null;

function clearRefreshToken() {
    if (refreshTokenTimer !== null) {
        clearTimeout(refreshTokenTimer);
        refreshTokenTimer = null;
    }
}

let retryAttempts = MAX_RETRY_ATTEMPTS;
function enqueueRefreshToken(user, expirationString) {
    const expiration = new Date(expirationString).valueOf();
    const now = new Date().valueOf();
    const diff = expiration - now;

    clearRefreshToken();

    // Token is already expired, but we just got a token. Something is broken
    // with Firebase or our clock.  We could refresh the token, but that could
    // just create an endless loop of errors.  Instead we just bail out.
    if (diff < 0) {
        if (retryAttempts <= 0) {
            // Tried to recover from refresh. Not possible, bail.
            return;
        }
        // Decrement retries.
        retryAttempts--;
    }

    // Refresh one minute before the timeout.
    const refreshTime = diff - 60000;

    // Less than a minute until token times out, let's just refresh the token
    // immediately.
    if (refreshTime < 0) {
        fetchAuthInfo(user);
        return;
    }

    refreshTokenTimer = setTimeout(fetchAuthInfo.bind(null, user), refreshTime);
}

async function fetchAuthInfo(user, stopOnboarding) {
    clearRefreshToken();
    // Unsubscribe previous listener.
    unsubscribeFetchUserDataSnapshot && unsubscribeFetchUserDataSnapshot();
    console.log('[debug] fetching auth info');

    try {
        const idToken = await user.getIdTokenResult(true);

        if (!idToken || !idToken.claims) {
            console.error('No idToken');
            return;
        }
        // Valid login. Reset retries.
        retryAttempts = MAX_RETRY_ATTEMPTS;

        const accountId = idToken.claims['sub'];
        const email = user.email || '';
        const phone = user.phoneNumber || '';
        const provider = idToken.signInProvider || '';

        enqueueRefreshToken(user, idToken.expirationTime);

        localStorage.setItem('loggedIn', 'loggedIn');

        if (idToken.signInProvider == 'password' && !user.emailVerified) {
            // If user is signed in through password, we must ensure verification.
            app.ports.verifyUserStart.send(email);
        } else {
            unsubscribeFetchUserDataSnapshot = db
                .collection('customers')
                .doc(accountId)
                .onSnapshot((doc) => {
                    const profile = doc.data();

                    if (!profile) {
                        onboardingUser = user;
                        app.ports.onboardingStart.send([
                            idToken.token,
                            email,
                            phone
                        ]);
                    } else {
                        app.ports.signedInInfo.send({
                            token: idToken.token,
                            email: email,
                            phone: phone,
                            uid: accountId,
                            provider: provider,
                            stopOnboarding: !!stopOnboarding
                        });

                        if (
                            typeof profile.travelcard === 'object' &&
                            profile.travelcard !== null
                        ) {
                            profile.travelcard.expires = convert_time(
                                profile.travelcard.expires
                            );
                        }

                        app.ports.firestoreReadProfile.send({
                            ...profile,
                            signInMethods: user.providerData
                        });

                        loadFareContracts(accountId);
                    }
                });
        }
    } catch (error) {
        console.log('Error when retrieving cached user', error);
    }
}

// Convert a Firebase Time type to something that's easier to work with.
function convert_time(firebaseTime) {
    if (!firebaseTime) return firebaseTime;

    const timestamp = parseInt(firebaseTime.toMillis(), 10);
    const date = new Date(timestamp);
    const parts = [];

    parts.push(date.getFullYear());
    parts.push(date.getMonth() + 1);
    parts.push(date.getDate());
    parts.push(date.getHours());
    parts.push(date.getMinutes());
    parts.push(date.getSeconds());

    return {
        timestamp,
        parts
    };
}

// TODO: Load tokens?
// TODO: Handle being logged out
function loadFareContracts(accountId) {
    const basePath = `customers/${accountId}`;
    const tokenPath = `${basePath}/fareContracts`;
    unsubscribeFareContractSnapshot && unsubscribeFareContractSnapshot();

    console.log('[debug] fetching fare contracts');

    unsubscribeFareContractSnapshot = db.collection(tokenPath).onSnapshot(
        (docs) => {
            const fareContracts = [];
            docs.forEach((doc) => {
                const payload = doc.data();

                if (payload) {
                    // Transform firebase time fields to something we can use
                    payload.created = convert_time(payload.created);
                    payload.travelRights = payload.travelRights.map((right) => {
                        right.startDateTime = convert_time(right.startDateTime);
                        right.endDateTime = convert_time(right.endDateTime);

                        if (Array.isArray(right.usedAccesses)) {
                            right.usedAccesses = right.usedAccesses.map(
                                (access) => {
                                    access.startDateTime = convert_time(
                                        access.startDateTime
                                    );
                                    access.endDateTime = convert_time(
                                        access.endDateTime
                                    );
                                    return access;
                                }
                            );
                        }
                        return right;
                    });
                    payload.validFrom =
                        Math.min.apply(
                            null,
                            payload.travelRights.map((x) => {
                                if (!x.startDateTime) return 0;
                                return x.startDateTime.timestamp;
                            })
                        ) || 0;
                    payload.validTo =
                        Math.max.apply(
                            null,
                            payload.travelRights.map((x) => {
                                if (!x.endDateTime) return 0;
                                return x.endDateTime.timestamp;
                            })
                        ) || 0;

                    fareContracts.push(payload);
                }
            });

            app.ports.receiveFareContracts.send(fareContracts);
        },
        function (e) {
            console.error('Error when retrieving fare contracts for user', e);

            if (e.message.includes('insufficient permissions')) {
                updateElmWithLoggedOutAndCleanUp();
            }
        }
    );
}

app.ports.onboardingRefreshAuth.subscribe(() => {
    setTimeout(() => {
        fetchAuthInfo(onboardingUser);
    }, 500);
});

app.ports.onboardingDone.subscribe(() => {
    setTimeout(() => {
        fetchAuthInfo(onboardingUser, true);
    }, 500);
});

app.ports.checkVerifyUser.subscribe(async () => {
    const user = firebase.auth().currentUser;

    try {
        if (!user) {
            app.ports.checkVerifyUserResponse.send(false);
        } else {
            await user.reload();
            if (user.emailVerified) {
                app.ports.checkVerifyUserResponse.send(true);
                fetchAuthInfo(user);
            }
        }
    } catch (e) {
        app.ports.checkVerifyUserResponse.send(false);
    }
});

app.ports.navigateTo.subscribe((url) => {
    window.location.assign(url);
});

app.ports.closeValidityWarning.subscribe(() => {
    localStorage.setItem('hideValidityWarning', 'yes');
});

app.ports.reloadPage.subscribe(() => {
    window.location.reload();
});

if (app.ports.convertTime) {
    app.ports.convertTime.subscribe(([date, time]) => {
        try {
            let dt = new Date(date + 'T' + time);

            if (
                app.ports.convertedTime &&
                dt !== null &&
                typeof dt !== 'undefined'
            ) {
                app.ports.convertedTime.send(
                    [dt.toISOString().replace(/\.[0-9]*/, ''), dt.valueOf()]
                );
            }
        } catch {}
    });
}

// Component that integrates with the ReCaptcha mechanism of Firebase Auth.
window.customElements.define(
    'atb-login-recaptcha',
    class extends HTMLElement {
        connectedCallback() {
            const recaptcha = document.createElement('div');
            recaptcha.setAttribute('id', 'atb-login-recaptcha');
            this.appendChild(recaptcha);

            window.recaptchaVerifier = new firebase.auth.RecaptchaVerifier(
                'atb-login-recaptcha',
                {
                    size: 'invisible',
                    callback: (response) => {
                        console.log('[debug] ReCaptcha response', response);
                    }
                }
            );
        }

        attributeChangedCallback() {}

        static get observedAttributes() {
            return [];
        }
    }
);

function handleAuthError(error) {
    if (!error) {
        app.ports.authError.send('En ukjent feil oppstod.');
    }

    switch (error.code) {
        case 'auth/invalid-phone-number':
            app.ports.authError.send('Ugyldig telefonnummer. Sjekk at telefonnummeret består av 8 siffer.');
            break;
        case 'auth/too-many-requests':
            app.ports.authError.send(
                'Du har prøvd å logge inn for mange ganger uten hell. Vent noen minutter og prøv igjen.'
            );
            break;
        case 'auth/captcha-check-failed':
            app.ports.authError.send(
                'Feil i valg i ReCaptcha. Prøv en gang til.'
            );
            break;
        case 'auth/missing-phone-number':
            app.ports.authError.send('Manglende telefonnummer.');
            break;
        case 'auth/user-disabled':
            app.ports.authError.send(
                'Brukeren din ser ut til å være deaktivert. Ta kontakt med kundeservice.'
            );
            break;
        case 'auth/invalid-verification-code':
            app.ports.authError.send(
                'Engangskoden stemmer ikke. Vennligst sjekk at det er riktig kode eller be om en ny engangskode.'
            );
            break;
        case 'auth/code-expired':
            app.ports.authError.send(
                'Engangskoden har utløpt. Vennligst prøv på nytt eller be om et nytt engangskode.'
            );
            break;
        case 'auth/user-not-found':
            app.ports.authError.send(
                'Fant ingen bruker med den informasjonen.'
            );
            break;
        case 'auth/weak-password':
            app.ports.authError.send('Passordet må være minst 6 tegn.');
            break;
        case 'auth/wrong-password':
            app.ports.authError.send(
                'Passordet stemmer ikke, vennligst prøv på nytt.'
            );
            break;
        default:
            app.ports.authError.send('En ukjent feil oppstod. Vent litt og prøv på nytt.');
    }
}

function setCustomAuthError(message) {
    app.ports.authError.send(message)
}

app.ports.loginPhone.subscribe((phone) => {
    if (!phone) {
        return;
    }

    firebase
        .auth()
        .signInWithPhoneNumber(phone, window.recaptchaVerifier)
        .then((confirmationResult) => {
            window.confirmationResult = confirmationResult;
            app.ports.phoneRequestCode.send('');
        })
        .catch((error) => {
            console.log('[debug] phone login error', error);
            console.log(
                '[debug] phone login error json',
                JSON.stringify(error)
            );

            handleAuthError(error);
        });
});

app.ports.confirmPhone.subscribe((code) => {
    if (!code) {
        return;
    }

    window.confirmationResult
        .confirm(code)
        .then((result) => {
            fetchAuthInfo(result.user);
        })
        .catch((error) => {
            console.log('[debug] phone confirm error', error);
            console.log(
                '[debug] phone confirm error json',
                JSON.stringify(error)
            );

            handleAuthError(error);
        });
});

app.ports.registerEmail.subscribe(async ({ email, password }) => {
    const checkEmailResult = await fetch(
        `${
            elmFlags.baseUrl
        }/webshop/v1/available-email?email=${encodeURIComponent(email)}`
    );
    const checkEmailData = await checkEmailResult.json();

    try {
        const signInMethods = await firebase
            .auth()
            .fetchSignInMethodsForEmail(email);

        if (signInMethods.length > 0) {
            return setCustomAuthError(
                'Det er alt registrert en profil på denne e-postadressen. Prøv å logg inn.'
            );
        }
    } catch (e) {
        // Can just ignore if we don't find anything.
    }

    if (!checkEmailData.available) {
        return setCustomAuthError(
            'Denne e-postadressen er registrert i systemet fra før. Kan det være du har logget inn med telefonnummer?'
        );
    }

    firebase
        .auth()
        .createUserWithEmailAndPassword(email, password)
        .then(async (userCredential) => {
            await sendVerificationEmail();
            fetchAuthInfo(userCredential.user);
        })
        .catch((error) => {
            console.log('[debug] email register error', error);
            console.log(
                '[debug] email register error json',
                JSON.stringify(error)
            );

            handleAuthError(error);
        });
});

app.ports.loginEmail.subscribe(({ email, password }) => {
    firebase
        .auth()
        .signInWithEmailAndPassword(email, password)
        .then((userCredential) => {
            fetchAuthInfo(userCredential.user);
        })
        .catch((error) => {
            console.log('[debug] email login error', error);
            console.log(
                '[debug] email login error json',
                JSON.stringify(error)
            );

            handleAuthError(error);
        });
});

async function sendVerificationEmail() {
    const actionCodeSettings = {
        url: window.location.origin
    };

    try {
        await firebase
            .auth()
            .currentUser.sendEmailVerification(actionCodeSettings);
        app.ports.verifyUserRequested.send();
    } catch (error) {
        app.ports.verifyUserRequested.send(error);
    }
}
app.ports.verifyUser.subscribe(sendVerificationEmail);

app.ports.resetPassword.subscribe((email) => {
    const actionCodeSettings = {
        url: window.location.origin
    };
    firebase
        .auth()
        .sendPasswordResetEmail(email, actionCodeSettings)
        .then(function () {
            app.ports.resetPasswordDone.send();
        })
        .catch(function (error) {
            handleAuthError(error);
        });
});

// Component to show maps from MapBox
window.customElements.define(
    'atb-map',
    class extends HTMLElement {
        constructor() {
            super();
        }

        // Callbacks

        connectedCallback() {
            this.innerText = 'Map';
        }

        disconnectedCallback() {}

        attributeChangedCallback() {}

        static get observedAttributes() {
            return [];
        }
    }
);

if (intercomEnabled) {
    // Intercom integration
    const baseSettings = {
        'AtB-Install-Id': installId,
        'AtB-Build-Number': elmFlags.commit,
        'hide_default_launcher': false,
    };

    firebase.auth().onAuthStateChanged((user) => {
        if (user) {
            window.Intercom('update', {
                ...baseSettings,
                'AtB-Firebase-Auth-Id': user.uid
            });
        } else {
            window.Intercom('update', {
                ...baseSettings,
                'AtB-Firebase-Auth-Id': undefined
            });
        }
    });

    window.Intercom('boot', {
        app_id: 'vdemedo2',
        ...baseSettings
    });
}
