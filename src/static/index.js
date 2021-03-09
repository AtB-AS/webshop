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
    apiKey: "AIzaSyB6zTnl8jKhw8o8rNrd_GEojlHVVAcFn0s",
    authDomain: "pilot-travelcard-webshop.firebaseapp.com",
    databaseURL: "https://pilot-travelcard-webshop.firebaseio.com",
    projectId: "pilot-travelcard-webshop",
    storageBucket: "pilot-travelcard-webshop.appspot.com",
    messagingSenderId: "598155505788",
    appId: "1:598155505788:web:ccddf3940a3e8e41aed9f9",
    measurementId: "G-Y6RSP82WY0"
};

firebase.initializeApp(firebaseConfig);

let tokenSnapshotCallback = null;
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

//  Observer on user info
firebase.auth().onAuthStateChanged(user => {
    if (user) {
        fetchAuthInfo(user);
    }
});

function fetchAuthInfo(user) {
    user
        .getIdTokenResult(true)
        .then(idToken => {
            if (!idToken || !idToken.claims || !idToken.claims["abt_id"] || typeof idToken.claims["abt_id"] !== 'string') {
                setTimeout(() => fetchAuthInfo(user), 500);
            } else {
                const accountId = idToken.claims["abt_id"];
                const email = user.email || '';
                const provider = idToken.signInProvider || '';

                app.ports.signInInfo.send({
                    token: idToken.token,
                    email: email,
                    uid: accountId,
                    provider: provider
                });

                localStorage["loggedIn"] = 'loggedIn';

                const path = `users/${accountId}/tokens`;

                try {
                    tokenSnapshotCallback = db.collection(path).onSnapshot(docs => {
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
