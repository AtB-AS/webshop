/*
* Based on https://github.com/testdrivenio/cypress-mochawesome-s3
* Changed some counters, etc since 1) *.json from spec-test may give failures: -1, 2) aggregated values are wrong in my opinion
*/

const fs = require('fs');
const path = require('path');

function getFiles(dir, ext, fileList = []) {
  const files = fs.readdirSync(dir);
  files.forEach((file) => {
    const filePath = `${dir}/${file}`;
    if (fs.statSync(filePath).isDirectory()) {
      getFiles(filePath, fileList);
    } else if (path.extname(file) === ext) {
      fileList.push(filePath);
    }
  });
  return fileList;
}

function traverseAndModifyTimedOut (target, deep) {
  if (target['tests'] && target['tests'].length) {
    target['tests'].forEach(test => {
      test.timedOut = false;
    });
  }
  if (target['suites']) {
    target['suites'].forEach(suite => {
      traverseAndModifyTimedOut(suite, deep + 1);
    })
  }
}

function combineMochaAwesomeReports() {
  const reportDir = path.join(__dirname, '..', 'results');
  const reports = getFiles(reportDir, '.json', []);
  const results = [];
  let totalSuites = 0;
  let totalTests = 0;
  let totalPasses = 0;
  let totalFailures = 0;
  let totalPending = 0;
  let startTime;
  let endTime;
  let totalSkipped = 0;
  reports.forEach((report, idx) => {
    const rawdata = fs.readFileSync(report);
    const parsedData = JSON.parse(rawdata);
    if (idx === 0) { startTime = parsedData.stats.start; }
    if (idx === (reports.length - 1)) { endTime = parsedData.stats.end; }

    //In case no tests in given report (e.g. all tests are skipped du to check on browser)
    if (parsedData.results.toString() === 'false'){
      return
    }

    //** compute failures since mochawesome gives wrong numbers **
    let failuresNo = 0
    for (let i = 0; i < parsedData.results[0].suites.length; i++){
      failuresNo += parsedData.results[0].suites[i].failures.length
    }

    totalSuites += parseInt(parsedData.stats.suites, 10);
    totalSkipped += parseInt(parsedData.stats.skipped, 10);
    totalPasses += parseInt(parsedData.stats.passes, 10);
    //totalFailures += parseInt(parsedData.stats.failures, 10);
    totalFailures += failuresNo;
    totalPending += parseInt(parsedData.stats.pending, 10);
    totalTests += parseInt(parsedData.stats.testsRegistered, 10);

    if (parsedData && parsedData.results && parsedData.results[0].suites) {
      parsedData.results[0].suites.forEach(result => {
        results.push(result)
      })
    }
  });
  return {
    totalSuites,
    totalTests,
    totalPasses,
    totalFailures,
    totalPending,
    startTime,
    endTime,
    totalSkipped,
    results,
  };
}

function getPercentClass(pct) {
  if (pct <= 50) {
    return 'danger';
  } else if (pct > 50 && pct < 80) {
    return 'warning';
  }
  return 'success';
}

function writeReport(obj, uuid, reportName) {
  const sampleFile = path.join(__dirname, 'sample.json');
  const outFile = path.join(__dirname, '..', 'results', `${reportName}.json`);
  fs.readFile(sampleFile, 'utf8', (err, data) => {
    if (err) throw err;
    const parsedSampleFile = JSON.parse(data);
    const stats = parsedSampleFile.stats;
    stats.suites = obj.totalSuites;
    stats.tests = obj.totalTests;
    stats.passes = obj.totalPasses;
    stats.failures = obj.totalFailures;
    stats.pending = obj.totalPending;
    stats.start = obj.startTime;
    stats.end = obj.endTime;
    stats.duration =  new Date(obj.endTime) - new Date(obj.startTime);
    //PREV stats.testsRegistered = obj.totalTests - obj.totalPending;
    stats.testsRegistered = obj.totalTests
    //PREV: Gave 'null' stats.passPercent = Math.round((stats.passes / (stats.testsRegistered - stats.pending)) * 1000) / 10;
    stats.passPercent = Math.round((stats.passes / (stats.passes + stats.failures)) * 1000) / 10;
    stats.pendingPercent = Math.round((stats.pending / stats.testsRegistered) * 1000) /10;
    //In case no tests (e.g. all tests are skipped du to check on browser)
    if (stats.testsRegistered === 0){
      stats.passPercent = 100; //100 to get a green bar in the report instead of a red bar
      stats.pendingPercent = 0;
    }
    //** Do not know what 'skipped' is here, since 'xit()' in Cypress becomes 'pending'
    stats.skipped = obj.totalSkipped;
    stats.hasSkipped = obj.totalSkipped > 0;

    obj.results.forEach(result => {
      traverseAndModifyTimedOut(result, 0);
    });

    parsedSampleFile.results[0].suites = obj.results;
    parsedSampleFile.results[0].uuid = uuid;
    fs.writeFile(outFile, JSON.stringify(parsedSampleFile), { flag: 'wx' }, (error) => {
      if (error) throw error;
    });
  });
}

module.exports = {
  combineMochaAwesomeReports,
  writeReport,
};