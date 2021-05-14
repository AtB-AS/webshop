require('./styles/main.scss');

import * as firebase from 'firebase/app';
import 'firebase/firebase-auth';
import 'firebase/firebase-firestore';
import 'firebase/firebase-remote-config';
import 'wicg-inert';

import { Elm } from '../elm/Main';

if (!elmFlags.isDevelopment && 'serviceWorker' in navigator) {
    window.addEventListener('load', () => {
        navigator.serviceWorker.register('/service-worker.js');
    });
}

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
            localUrl: window.location.origin
        },
        elmFlags
    )
});

function fetchRemoteConfigData(port, key) {
    if (!port) {
        return;
    }

    const value = remoteConfig.getString(key);

    if (typeof value !== 'string' || value.length < 1) {
        return;
    }

    const data = JSON.parse(value);

    if (typeof data !== 'object' || data === null) {
        return;
    }

    port.send(data);
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
            'preassigned_fare_products'
        );
        fetchRemoteConfigData(
            app.ports.remoteConfigUserProfiles,
            'user_profiles'
        );
        fetchRemoteConfigData(
            app.ports.remoteConfigTariffZones,
            'tariff_zones'
        );
        sendRemoteConfigVatPercent();
    })
    .catch((err) => {
        // ...
        console.log('[ERROR] remoteConfig.fetchAndActivate:', err);
    });

app.ports.signInHandler.subscribe((provider_id) => {
    const auth = firebase.auth();
    let provider;

    if (provider_id === 'google.com') {
        provider = auth.signInWithPopup(new firebase.auth.GoogleAuthProvider());
    } else if (provider_id === 'anonymous') {
        provider = auth.signInAnonymously();
    } else {
        console.log('Tried to sign in with unknown provider', provider_id);
        return;
    }

    provider
        .then((result) => {
            fetchAuthInfo(result.user);
        })
        .catch((error) => {
            app.ports.signInError.send({
                code: error.code,
                message: error.message
            });
        });
});

app.ports.signOutHandler.subscribe(async () => {
    clearRefreshToken();

    try {
        // Remove loggedIn before signing out to avoid showing error
        // below on onAuthStateChanged.
        localStorage.removeItem('loggedIn');

        await firebase.auth().signOut();

        unsubscribeFareContractSnapshot && unsubscribeFareContractSnapshot();
        unsubscribeFareContractSnapshot = null;

        unsubscribeFetchUserDataSnapshot && unsubscribeFetchUserDataSnapshot();
        unsubscribeFetchUserDataSnapshot = null;
    } catch (e) {
        console.error('[debug] Unable to logout: ', e);
    }
});

// Use device language
firebase.auth().useDeviceLanguage();

// Observer on user info
firebase.auth().onAuthStateChanged((user) => {
    if (user) {
        fetchAuthInfo(user);
    } else if (localStorage.getItem('loggedIn')) {
        // Not logged in remove logged in flag to be safe to avoid infinite loading.
        console.log('[debug] Logged out due to stale state.');
        clearRefreshToken();
        localStorage.removeItem('loggedIn');

        app.ports.signInError.send({
            code: -1,
            message: 'Du er blitt logget ut.'
        });
    }
});

let refreshTokenTimer = null;

function clearRefreshToken() {
    if (refreshTokenTimer !== null) {
        clearTimeout(refreshTokenTimer);
        refreshTokenTimer = null;
    }
}

function enqueueRefreshToken(user, expirationString) {
    const expiration = new Date(expirationString).valueOf();
    const now = new Date().valueOf();
    const diff = expiration - now;

    clearRefreshToken();

    // Token is already expired, but we just got a token. Something is broken
    // with Firebase or our clock.  We could refresh the token, but that could
    // just create an endless loop of errors.  Instead we just bail out.
    if (diff < 0) {
        return;
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

        const accountId = idToken.claims['sub'];
        const email = user.email || '';
        const phone = user.phoneNumber || '';
        const provider = idToken.signInProvider || '';

        enqueueRefreshToken(user, idToken.expirationTime);

        localStorage.setItem('loggedIn', 'loggedIn');

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
                    app.ports.signInInfo.send({
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

                    app.ports.firestoreReadProfile.send(profile);

                    loadFareContracts(accountId);
                }
            });
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
        }
    );
}

app.ports.onboardingDone.subscribe(() => {
    setTimeout(() => {
        fetchAuthInfo(onboardingUser, true);
    }, 500);
});

app.ports.openWindow.subscribe((url) => {
    window.open(url);
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
                    dt.toISOString().replace(/\.[0-9]*/, '')
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

function handlePhoneError(error) {
    if (!error) {
        app.ports.phoneError.send('En ukjent feil oppstod.');
    }

    switch (error.code) {
        case 'auth/invalid-phone-number':
            app.ports.phoneError.send('Ugyldig telefonnummer.');
            break;
        case 'auth/too-many-requests':
            app.ports.phoneError.send(
                'Du har prøvd å logge inn for mange ganger uten hell. Vent noen minutter og prøv igjen.'
            );
            break;
        case 'auth/captcha-check-failed':
            app.ports.phoneError.send(
                'Feil i valg i ReCaptcha. Prøv en gang til.'
            );
            break;
        case 'auth/missing-phone-number':
            app.ports.phoneError.send('Ugyldig telefonnummer.');
            break;
        case 'auth/user-disabled':
            app.ports.phoneError.send(
                'Brukeren din ser ut til å være deaktivert. Ta kontakt med kundeservice.'
            );
            break;
        case 'auth/invalid-verification-code':
            app.ports.phoneError.send(
                'Passordet stemmer ikke, vennligst prøv på nytt eller be om et nytt engangspassord.'
            );
            break;
        case 'auth/code-expired':
            app.ports.phoneError.send(
                'Engangspassordet har utløpt. Vennligst prøv på nytt eller be om et nytt engangspassord.'
            );
            break;
        default:
            app.ports.phoneError.send('En ukjent feil oppstod.');
    }
}

app.ports.phoneLogin.subscribe((phone) => {
    if (!phone) {
        return;
    }

    firebase
        .auth()
        .signInWithPhoneNumber(phone, window.recaptchaVerifier)
        .then((confirmationResult) => {
            window.confirmationResult = confirmationResult;
            app.ports.phoneRequestCode.send();
        })
        .catch((error) => {
            console.log('[debug] phone login error', error);
            console.log(
                '[debug] phone login error json',
                JSON.stringify(error)
            );

            handlePhoneError(error);
        });
});

app.ports.phoneConfirm.subscribe((code) => {
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

            handlePhoneError(error);
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

// Intercom integration
window.Intercom('boot', {
    app_id: 'vdemedo2'
});
