if [ "$#" -ne 2 ]
then
    echo "Argument error!"
    echo "First argument should be the environment name."
    echo "Available environment names:
 - dev
 - prodstaging
 - staging
 - store"
    echo "Second argument should be the app organisation name."
    echo "Available app variant names:
-atb
-nfk"

    echo "Example:
./override-environment.sh store atb"
    exit 1
else
    APP_ENVIRONMENT=$1
    APP_ORG=$2
    ASSETS_FOLDER=orgs/assets/$APP_ORG
    ORG_FOLDER=env/$APP_ORG

    echo "Copying $APP_ENVIRONMENT assets for $APP_ORG to static folder"
    cp -r $ASSETS_FOLDER/. src/static/
fi