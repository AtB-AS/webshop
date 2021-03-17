require('./styles/main.scss');

import * as firebase from "firebase/app";
import "firebase/firebase-auth";
import "firebase/firebase-firestore";
import "firebase/firebase-remote-config";

import {Elm} from '../elm/Main';

if (!elmFlags.isDevelopment && 'serviceWorker' in navigator) {
    window.addEventListener('load', () => {
        navigator.serviceWorker.register('/service-worker.js');
    });
}

let installId = localStorage['Atb-Install-Id'];

if (!installId) {
    installId = 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function (c) {
        const r = Math.random() * 16 | 0;
        const v = c === 'x' ? r : (r & 0x3 | 0x8);
        return v.toString(16);
    });
    localStorage['Atb-Install-Id'] = installId;
}

console.log('Atb-Install-Id:', installId);

const firebaseConfig = {
    apiKey: "AIzaSyDoat8ob5tewAXaPEhqbZKzx8e7LC5nuzQ",
    authDomain: "atb-mobility-platform-staging.firebaseapp.com",
    projectId: "atb-mobility-platform-staging",
    storageBucket: "atb-mobility-platform-staging.appspot.com",
    messagingSenderId: "939812594010",
    appId: "1:939812594010:web:0308b2a9cdc80b0d069363"
};

firebase.initializeApp(firebaseConfig);

let fareContractSnapshotCallback = null;
let onboardingUser = null;
const db = firebase.firestore();
const remoteConfig = firebase.remoteConfig();
const app = Elm.Main.init({
    flags: Object.assign({
            installId: installId,
            loggedIn: localStorage["loggedIn"] === 'loggedIn'
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

// NOTE: Only change this for testing.
remoteConfig.settings.minimumFetchIntervalMillis = 3600000;
//remoteConfig.settings.minimumFetchIntervalMillis = 60000;
remoteConfig.fetchAndActivate()
    .then(() => {
        fetchRemoteConfigData(app.ports.remoteConfigFareProducts, 'preassigned_fare_products');
        fetchRemoteConfigData(app.ports.remoteConfigUserProfiles, 'user_profiles');
        fetchRemoteConfigData(app.ports.remoteConfigTariffZones, 'tariff_zones');
        //fetchRemoteConfigData(app.ports.remoteConfigSalesPackages, 'sales_packages');
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
        console.log("Tried to sign in with unknown provider", provider_id);
        return;
    }

    provider
        .then(result => {
            fetchAuthInfo(result.user);
        })
        .catch(error => {
            app.ports.signInError.send({
                code: error.code,
                message: error.message
            });
        });
});

app.ports.signOutHandler.subscribe(() => {
    localStorage["loggedIn"] = '';

    if (typeof fareContractSnapshotCallback === 'function') {
        fareContractSnapshotCallback();
        fareContractSnapshotCallback = null;
    }

    firebase.auth().signOut();
});

// Use device language
firebase.auth().useDeviceLanguage();

// Observer on user info
firebase.auth().onAuthStateChanged(user => {
    if (user) {
        fetchAuthInfo(user);
    }
});

function fetchAuthInfo(user) {
    user
        .getIdTokenResult(true)
        .then(idToken => {
            if (!idToken || !idToken.claims || !idToken.claims["sub"] || typeof idToken.claims["sub"] !== 'string') {
                // Start onboarding process
                onboardingUser = user;
                app.ports.onboardingStart.send(idToken.token);
            } else {
                const accountId = idToken.claims["sub"];
                const email = user.email || '';
                const provider = idToken.signInProvider || '';

                localStorage["loggedIn"] = 'loggedIn';

                db.collection('customers').doc(accountId).onSnapshot(doc => {
                    const profile = doc.data();

                    if (!profile)
                    {
                        onboardingUser = user;
                        app.ports.onboardingStart.send(idToken.token);
                    }
                    else
                    {
                        app.ports.signInInfo.send({
                            token: idToken.token,
                            email: email,
                            uid: accountId,
                            provider: provider
                        });

                        if (typeof profile.travelcard === 'object' && profile.travelcard !== null) {
                            profile.travelcard.expires = convert_time(profile.travelcard.expires);
                        }

                        app.ports.firestoreReadProfile.send(profile);

                        loadFareContracts(accountId);
                    }
                });
            }
        })
        .catch(error => {
            console.log("Error when retrieving cached user", error);
        });
}

// Convert a Firebase Time type to something that's easier to work with.
function convert_time(firebaseTime) {
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
        parts,
    };
}

// TODO: Load tokens?
// TODO: Handle being logged out
function loadFareContracts(accountId) {
    const basePath = `customers/${accountId}`;
    const tokenPath = `${basePath}/fareContracts`;

    try {
        fareContractSnapshotCallback = db.collection(tokenPath).onSnapshot(docs => {
            const fareContracts = [];

            docs.forEach(doc => {
                const payload = doc.data();

                if (payload) {
                    // Transform firebase time fields to something we can use
                    payload.created = convert_time(payload.created);
                    payload.travelRights = payload.travelRights.map(right => {
                        right.startDateTime = convert_time(right.startDateTime);
                        right.endDateTime = convert_time(right.endDateTime);
                        return right;
                    });
                    payload.validFrom = Math.min.apply(null, payload.travelRights.map(x => x.startDateTime.timestamp)) || 0;
                    payload.validTo = Math.max.apply(null, payload.travelRights.map(x => x.endDateTime.timestamp)) || 0;

                    fareContracts.push(payload);
                }
            });

            app.ports.receiveFareContracts.send(fareContracts);
        });
    } catch (e) {
        console.error("Error when retrieving fare contracts for user", e);
    }
}

app.ports.onboardingDone.subscribe(() => {
    setTimeout(() => fetchAuthInfo(onboardingUser), 500);
});

app.ports.openWindow.subscribe((url) => {
    window.open(url);
});

if (app.ports.convertTime) {
    app.ports.convertTime.subscribe(([date, time]) => {
        try {
            let dt = new Date(date + 'T' + time);

            if (app.ports.convertedTime && dt !== null && typeof dt !== 'undefined') {
                app.ports.convertedTime.send((dt.toISOString().replace(/\.[0-9]*/, '')));
            }
        } catch {
        }
    });
}
