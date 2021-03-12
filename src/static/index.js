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

let tokenSnapshotCallback = null;
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

// NOTE: Only change this for testing.
remoteConfig.settings.minimumFetchIntervalMillis = 3600000;
//remoteConfig.settings.minimumFetchIntervalMillis = 60000;
remoteConfig.fetchAndActivate()
    .then(() => {
        // ...
        app.ports.remoteConfigFareProducts.send(JSON.parse(remoteConfig.getString('preassigned_fare_products')));
        app.ports.remoteConfigUserProfiles.send(JSON.parse(remoteConfig.getString('user_profiles')));
        app.ports.remoteConfigTariffZones.send(JSON.parse(remoteConfig.getString('tariff_zones')));
        //app.ports.remoteConfigSalesPackages.send(JSON.parse(remoteConfig.getString('sales_packages')));
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

    if (typeof tokenSnapshotCallback === 'function') {
        tokenSnapshotCallback();
        tokenSnapshotCallback = null;
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

                app.ports.signInInfo.send({
                    token: idToken.token,
                    email: email,
                    uid: accountId,
                    provider: provider
                });

                localStorage["loggedIn"] = 'loggedIn';

                const basePath = `customers/${accountId}`;
                const tokenPath = `${basePath}/tokens`;

                db.collection('customers').doc(accountId).onSnapshot(doc => {
                    console.log('profile', doc.data());
                    app.ports.firestoreReadProfile.send(doc.data());
                });

                try {
                    tokenSnapshotCallback = db.collection(tokenPath).onSnapshot(docs => {
                        const tokens = [];

                        docs.forEach(doc => {
                            const payload = doc.data().payload;

                            if (payload && payload.toBase64) {
                                tokens.push([doc.id, payload.toBase64()]);
                            }
                        });

                        app.ports.receiveTokens.send(tokens);
                    });
                } catch (e) {
                    console.error("Error when retrieving user", e);
                }
            }
        })
        .catch(error => {
            console.log("Error when retrieving cached user", error);
        });
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
