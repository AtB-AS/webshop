import json2table
import json
import re
import os
import shutil

'''
https://github.com/latture/json2table
'''

INIT_TEST_RESULTS_FILE = "init_testresults.json"
TEST_RESULTS_FILE = "currentTestResults.json"

DEBUG = False


def addToTestResults(json_lastTest):

    # init testresults if not exist
    if not os.path.isfile(TEST_RESULTS_FILE):
        shutil.copy(INIT_TEST_RESULTS_FILE, TEST_RESULTS_FILE)

    lastTests = {}
    describes = json_lastTest['results'][0]['suites']

    # loop through all specs
    for describe in describes:
        its = describe['tests']
        for it in its:
            # fail: true -> 0
            if it['fail']:
                lastTests[it['fullTitle']] = 0
            elif it['pending']:
                lastTests[it['fullTitle']] = 2
            else:
                lastTests[it['fullTitle']] = 1

    if (DEBUG):
        for key in lastTests.keys():
            print("TEST: " + str(key) + " --> " + str(lastTests[key]))

    # import test results
    fr_results = open(TEST_RESULTS_FILE)
    json_results = json.load(fr_results)
    fr_results.close()

    previousTests = {}
    testResults = json_results['testresults']
    for result in testResults:
        individualResults = {}
        for i in range(10):
            i += 1
            if '#' + str(i) not in result:
                continue
            individualResults['#' + str(i)] = result['#' + str(i)]

        previousTests[result['test']] = individualResults

    if (DEBUG):
        for key in previousTests.keys():
            print("TEST: " + str(key) + " --> " + str(previousTests[key]))

    # shift results, e.g. #10 -> #9
    for key in previousTests.keys():
      previousTests[key] = shiftResults(previousTests[key])

    if (DEBUG):
        for key in previousTests.keys():
            print("TEST 2: " + str(key) + " --> " + str(previousTests[key]))

    ## add last test to test results
    for key in previousTests.keys():
        # test is not run
        if key not in lastTests.keys():
            previousTests[key] = addNewResult(previousTests[key], '-')
            continue
        # test is run
        previousTests[key] = addNewResult(previousTests[key], lastTests[key])
    # new test is run
    for key in lastTests.keys():
        if key not in previousTests.keys():
            previousTests[key] = addNewTest(lastTests[key])

    if (DEBUG):
        for key in previousTests.keys():
            print("TEST 3: " + str(key) + " --> " + str(previousTests[key]))

    # add test name as first item
    previousTests = addTestName(previousTests)

    if (DEBUG):
        outputJson = createJson(previousTests)
        print("TEST 4: " + str(outputJson))

    # save last test results
    with open(TEST_RESULTS_FILE, 'wt', encoding='utf-8') as f:
        json.dump(createJson(previousTests), f, ensure_ascii=False, indent=2)

    return previousTests


def testResultsToHTML(jsonTestResults, htmlFileName):
    header = "<!DOCTYPE html><html><head><style type='text/css'>table {font-family: calibri, sans-serif;font-size: 12px;width: 100%;border-spacing: 0;border-collapse:collapse;}th, td{border-bottom: 1px solid #000000; border-spacing: 0;border-collapse:collapse;text-align: center;padding: 2px; class='true_false'}th{font-size: 14px;}td:nth-child(1){text-align: left;width: 50%;}tbody tr:nth-child(odd) {background-color: #dddddd;}</style> \
    <script type='text/javascript' src='https://ajax.googleapis.com/ajax/libs/jquery/1.4.4/jquery.js'></script> \
    <script type='text/javascript'> \
    $(document).ready(function(){ \
        $('td.true_false').each(function(){ \
            if ($(this).text() == '0') { \
                $(this).css('background-color','#C39BD3'); \
            } \
            else if ($(this).text() == '2') { \
                $(this).css('background-color','#21DFFA'); \
            } \
        }); \
    }); \
    </script>" \
             "</head><body><h2>E2E test results</h2><p>Lists all tests and the results from the last 10 runs</p><br>" \
             "<div style=\"width:100px;background-color:#C39BD3\">Fail</div>" \
             "<div style=\"width:100px;background-color:#21DFFA\">Pending</div><br>"
    footer = "</body></html>"

    # write table
    build_direction = "TOP_TO_BOTTOM"
    table_attributes = None
    jsonConverted = json2table.convert(createJson(jsonTestResults), build_direction=build_direction, table_attributes=table_attributes)

    fh = open(htmlFileName, 'wt')
    fh.write(header)
    fh.write(prettifyTable(jsonConverted))
    fh.write(footer)
    fh.close()


def addTestName(resultArray):
    for key in resultArray:
        results = resultArray[key]
        newResults = {}
        newResults['test'] = str(key)
        for resultItem in results:
            newResults[resultItem] = results[resultItem]
        resultArray[key] = newResults
    return resultArray

def createJson(resultArray):
    outputJson = {}
    outputJson['testresults'] = []
    for key in resultArray:
        outputJson['testresults'].append(resultArray[key])
    return outputJson

def shiftResults(resultArray):
    individualResults = {}
    for i in range(9):
        i += 1
        individualResults['#' + str(i)] = resultArray['#' + str(i+1)]
    return individualResults

def addNewResult(resultsArray, result):
    resultsArray['#10'] = result
    return resultsArray

def addNewTest(result):
    individualResults = {}
    for i in range(9):
        i += 1
        individualResults['#' + str(i)] = '-'
    individualResults['#10'] = result
    return individualResults

def prettifyTable(table):
    # remove extra table layer
    # NB: dependent on the current json structure
    table_1 = re.sub(r'<table><tr><th>testresults</th></tr><tr><td>', '', table)
    table_2 = re.sub(r'</td></tr></table></body></html>', '</body></html>', table_1)
    # add color to failed test results
    table_3 = re.sub(r'<td>', '<td class="true_false">', table_2)
    return table_3
