"use strict";

const baseEncodeTables = {
    26: "abcdefghijklmnopqrstuvwxyz",
    32: "123456789abcdefghjkmnpqrstuvwxyz", // no 0lio
    36: "0123456789abcdefghijklmnopqrstuvwxyz",
    49: "abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ", // no lIO
    52: "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    58: "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ", // no 0lIO
    62: "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
    64: "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ-_"
};

function encodeBufferToBase(buffer, base) {
    const encodeTable = baseEncodeTables[base];
    if(!encodeTable) throw new Error("Unknown encoding base" + base);

    const readLength = buffer.length;

    const Big = require("big.js");
    Big.RM = Big.DP = 0;
    let b = new Big(0);
    for(let i = readLength - 1; i >= 0; i--) {
        b = b.times(256).plus(buffer[i]);
    }

    let output = "";
    while(b.gt(0)) {
        output = encodeTable[b.mod(base)] + output;
        b = b.div(base);
    }

    Big.DP = 20;
    Big.RM = 1;

    return output;
}

function createHashFunction(hashType, digestType) {
    if (typeof hashType !== 'string') {
        hashType = 'sha256';
    }

    if (typeof digestType !== 'string') {
        digestType = 'hex';
    }

    return class {
        constructor() {
            this.hash = require("crypto").createHash(hashType);
        }

        update(data) {
            this.hash.update(data);
            return this;
        }

        digest() {
            if (digestType === 'base26' || digestType === 'base32' || digestType === 'base36' ||
                digestType === 'base49' || digestType === 'base52' || digestType === 'base58' ||
                digestType === 'base62' || digestType === 'base64') {
                return encodeBufferToBase(this.hash.digest(), digestType.substr(4));
            } else {
                return this.hash.digest(digestType || 'hex');
            }
        }
    };
}

module.exports = createHashFunction;
