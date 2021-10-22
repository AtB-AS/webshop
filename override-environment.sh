if [ "$#" -ne 1 ]
then
    echo "Argument error!"
    echo "First argument should be the app organisation name."
    echo "Available app organisation names:
-atb
-nfk"

    echo "Example:
./override-environment.sh atb"
    exit 1
else
    APP_ORG=$1
    ASSETS_FOLDER=orgs/assets/$APP_ORG
    ORG_FOLDER=env/$APP_ORG
    DESTINATION_FOLDER=src/static/

    echo "Copying favicon.ico for $APP_ORG to static folder"
    cp $ASSETS_FOLDER/favicon.ico $DESTINATION_FOLDER
    echo "Copying assets for $APP_ORG to static folder"
    cp -r $ASSETS_FOLDER/org $DESTINATION_FOLDER/
fi