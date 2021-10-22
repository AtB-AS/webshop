if [ "$#" -ne 1 ]
then
    echo "Argument error!"
    echo "First argument should be the app organisation name."
    echo "Available app organisation names:
-atb
-nfk"

    echo "Example:
yarn setup atb"
    exit 1
else
    sh ./override-environment.sh $1
fi