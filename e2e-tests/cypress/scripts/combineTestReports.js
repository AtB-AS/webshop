/*
* Mochawesome makes a separate test file (.json) for each cypress spec.
* This script combines them into one single .json file
* Run mochawesome-reporter (marge) after (used in build.gradle)
*
* Based on https://github.com/testdrivenio/cypress-mochawesome-s3
*/

'use strict';
const combine = require('./combine.js');
const uuidv1 = require('uuid');

const testReportName = 'cypressTestReport';

function combineTestReports(name) {
    const data = combine.combineMochaAwesomeReports();
    const uuid = uuidv1.v1();
    combine.writeReport(data, uuid, testReportName);
}

combineTestReports();

