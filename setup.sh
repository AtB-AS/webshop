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
./setup.sh dev atb"
    exit 1
else
    sh ./override-environment.sh $1 $2
fi