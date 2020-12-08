require('./styles/main.scss');

import * as firebase from "firebase/app";
import "firebase/firebase-auth";
import "firebase/firebase-firestore";

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

const provider = new firebase.auth.GoogleAuthProvider();
const db = firebase.firestore();
const app = Elm.Main.init({
    flags: Object.assign({
            installId: installId,
            loggedIn: localStorage["loggedIn"] === 'loggedIn'
        },
        elmFlags
    )
});

app.ports.signIn.subscribe(() => {
    firebase
        .auth()
        .signInWithPopup(provider)
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

app.ports.signOut.subscribe(() => {
    localStorage["loggedIn"] = '';
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

                app.ports.signInInfo.send({
                    token: idToken.token,
                    email: user.email,
                    uid: accountId
                });

                localStorage["loggedIn"] = 'loggedIn';

                const path = `users/${accountId}/tokens`;

                try {
                    db.collection(path).onSnapshot(docs => {
                        const tokens = [];

                        docs.forEach(doc => {
                            if (doc.data().payload) {
                                tokens.push([doc.id, doc.data().payload.toBase64()]);
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
