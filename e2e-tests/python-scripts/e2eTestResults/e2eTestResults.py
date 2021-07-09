import requests
import os
import sys
import datetime
import conversions
import json
from io import BytesIO
from zipfile import ZipFile

'''
# Fetches the 'test-results-json' artifact from the GH action 'E2E-tests of webshop', and creates a simple HTML to 
illustrate the test results of the last 10 runs. Main goal is to see whether any tests are failing randomly - i.e. flaky
tests. Failed tests (and pending ones) are marked to highlight these.

# Start
$ python3 e2eTestResults.py
    
# Requirement
- Necessitates a 'githubToken.txt' file with a personal github token which allows to read from the action

# Note
- Searches for all not expired artifacts of given name, but created after the date of the global parameter 
'testsNoOlderThan'

# Re-run from scratch
- Remove files listed in `.gitignore`, and run again

# Example
$ python3 buyTicket.py --email test@test.no --password 12345678

# Output
1. e2eTestReport.html: simple illustration of the last 10 runs
2. e2eTestReport.json: if not deleted, any new runs will add to the allready fetched artifacts
'''

# Init parameters
githubTokenFile = "githubToken.txt"
lastCreatedDateFile = "lastCreatedDate.txt"
testsNoOlderThan = '2021-07-02T11:00:00Z'
githubOwner = "AtB-AS"
githubRepo = "webshop"
artifactName = 'test-results-json'
htmlFileName = "e2eTestReport.html"


def main():

    # Init
    githubToken = getGithubToken(githubTokenFile)

    # Get new artifacts
    artifacts = getArtifacts(githubToken)

    testresults = {}
    # If no new artifacts
    if len(artifacts) == 0:
        print("No new artifacts to download")
    else:
        # Get each artifact
        for artifactId in artifacts:
            artifactJson = getArtifact(githubToken, artifactId)

            # Add results to current results
            testresults = conversions.addToTestResults(artifactJson)


        # Print to HTML
        conversions.testResultsToHTML(testresults, htmlFileName)


def getGithubToken(githubTokenFile):
    # Get github token
    if not os.path.isfile(githubTokenFile):
        print("File path {} does not exist. Exiting...".format(githubTokenFile))
        sys.exit()
    with open(githubTokenFile) as readFile:
        for line in readFile:
            return line.split('\n')[0]


def getLastCreatedDate():
    # Get github token
    if not os.path.isfile(lastCreatedDateFile):
        return testsNoOlderThan
    with open(lastCreatedDateFile) as readFile:
        for line in readFile:
            return line.split('\n')[0]


def updateLastCreatedDate(createdDateList):
    myFormat = "%Y-%m-%dT%H:%M:%SZ"
    last = datetime.datetime.strptime(getLastCreatedDate(), myFormat)
    newDate = last
    newDateS = ''
    for createdDate in createdDateList:
        created = datetime.datetime.strptime(createdDate, myFormat)
        if created > last and created > newDate:
            newDate = created
            newDateS = createdDate

    if len(newDateS) > 0:
        fh = open(lastCreatedDateFile, 'wt')
        fh.write(newDateS)
        fh.close()


def isNewArtifact(createdDate):
    myFormat = "%Y-%m-%dT%H:%M:%SZ"
    last = datetime.datetime.strptime(getLastCreatedDate(), myFormat)
    current = datetime.datetime.strptime(createdDate, myFormat)
    if current > last:
        return True
    return False


def getArtifacts(githubToken):
    url = f"https://api.github.com/repos/{githubOwner}/{githubRepo}/actions/artifacts"
    headers = {'Authorization': f'token {githubToken}', 'Accept': 'application/vnd.github.v3+json'}
    validArtifacts = []
    createdDates = []

    artifacts = requests.get(url, headers=headers).json()

    # Check if any
    if int(artifacts['total_count']) == 0:
        return validArtifacts

    # Get artifacts of given name
    for artifact in artifacts['artifacts']:
        if artifact['name'] == artifactName and not artifact['expired']:
            if isNewArtifact(artifact['created_at']):
                validArtifacts.append(artifact['id'])
                createdDates.append(artifact['created_at'])

    # Update last created date
    updateLastCreatedDate(createdDates)

    # Sort artifactIds
    validArtifacts.sort()

    return validArtifacts


def getArtifact(githubToken, artifactId):
    url = f"https://api.github.com/repos/{githubOwner}/{githubRepo}/actions/artifacts/{artifactId}/zip"
    headers = {'Authorization': f'token {githubToken}', 'Accept': 'application/vnd.github.v3+json'}

    artifact = requests.get(url, headers=headers)
    print("Download artifact id {}".format(artifactId))

    # Unzip in memory
    zip = ZipFile(BytesIO(artifact.content))
    byteResponse = zip.open(zip.namelist()[0]).read()
    jsonResponse = json.loads(byteResponse.decode("utf-8"))

    '''
    # Find the first matching json file in the zip:
    match = [s for s in f.namelist() if ".json" in s][0]

    # Save zip
    zipNname = "testresults.zip"
    zfile = open(zipNname, 'wb')
    zfile.write(artifact.content)
    zfile.close()
    '''

    return jsonResponse


if __name__ == '__main__':
    main()


