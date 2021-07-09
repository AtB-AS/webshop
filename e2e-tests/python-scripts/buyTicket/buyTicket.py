import requests
import re
import sys
import getopt

'''
# Buys a single ticket for a given user in staging

# Start
$ python3 buyTicket.py --email <email for login> --password <password for login>
    -> email: email used for log in
    -> password: password used for log in
    
# Note
- Only the staging environment is supported
- Only a single ticket is supported

# Example
$ python3 buyTicket.py --email test@test.no --password 12345678

# Output
1. If ok, prints 'ORDER_ID <order_id>!, if not 'ERROR <possible extra info>'
'''


def main(argv):

    # Init parameters
    email = ""
    password = ""
    abs1 = False
    abs2 = False

    # Input parameters
    try:
        opts, args = getopt.getopt(argv, "", ["email=", "password="])
    except getopt.GetoptError:
        print('python3 buyTicket.py --email <email for login> --password <password for login>')
        sys.exit()
    for opt, arg in opts:
        if opt == '--email':
            email = arg
            abs1 = True
        elif opt == '--password':
            password = arg
            abs2 = True
    # Check that the non optional parameters are set
    if not abs1 or not abs2:
        print('Missing "--username" and/or "--password" parameter')
        print('python3 buyTicket.py --email <email for login> --password <password for login>')
        sys.exit()

    idToken = authorization(email, password)
    # print("idToken: {}".format(idToken))

    offer_id = getOffer(idToken)
    # print("offer_id: {}".format(offer_id))

    reservationObject = reserveTicket(idToken, offer_id)
    # print("order_id: {}, payment_id {}".format(reservationObject['order_id'], reservationObject['payment_id']))

    netsObject = netsStartPayment(reservationObject)
    # print("viewState: {}".format(netsObject['viewState']))
    # print("viewStateGenerator: {}".format(netsObject['viewStateGenerator']))
    # print("eventValidation: {}".format(netsObject['eventValidation']))

    result = netsPayment(reservationObject, netsObject)
    # print("payment result: {}".format(result))
    if result == 'OK':
        print("ORDER_ID: {}".format(reservationObject['order_id']))
    else:
        print("ERROR")


def netsPayment(reservationObject, netsObject):
    merchantId = reservationObject['netsMerchantId']
    transactionId = reservationObject['netsTransactionId']
    data = {
        'Visa$cardNumber': '4925000000000004',
        'Visa$month': '06',
        'Visa$year': '22',
        'Visa$securityCode': 123,
        'okButton': 'Betal',
        '__VIEWSTATE': netsObject['viewState'],
        '__VIEWSTATEGENERATOR': netsObject['viewStateGenerator'],
        '__EVENTVALIDATION': netsObject['eventValidation'],
        '__LASTFOCUS': '',
        '__EVENTTARGET': '',
        '__EVENTARGUMENT': '',
        '__VIEWSTATEENCRYPTED': ''
    }
    nets = requests.post("https://test.epayment.nets.eu/Terminal/default.aspx?merchantId={}&transactionId={}".format(merchantId, transactionId),
                         data=data)

    if nets.status_code == 200:
        nets.encoding = 'utf-8'
        return 'OK'
    else:
        print("ERROR netsStartPayment. Status code {}.".format(nets.status_code))
        sys.exit()


def netsStartPayment(reservationObject):
    headers = {'Accept': '*/*'}
    merchantId = reservationObject['netsMerchantId']
    transactionId = reservationObject['netsTransactionId']
    nets = requests.get("https://test.epayment.nets.eu/Terminal/default.aspx?merchantId={}&transactionId={}".format(merchantId, transactionId),
                        headers=headers)

    if nets.status_code == 200:
        nets.encoding = 'utf-8'
        netsObject = {}
        viewState = re.findall('name="__VIEWSTATE" id="__VIEWSTATE" value="(.*)" />', str(nets.text))[0]
        viewStateGenerator = re.findall('name="__VIEWSTATEGENERATOR" id="__VIEWSTATEGENERATOR" value="(.*)" />', str(nets.text))[0]
        eventValidation = re.findall('name="__EVENTVALIDATION" id="__EVENTVALIDATION" value="(.*)" />', str(nets.text))[0]

        netsObject['viewState'] = viewState
        netsObject['viewStateGenerator'] = viewStateGenerator
        netsObject['eventValidation'] = eventValidation
        return netsObject
    else:
        print("ERROR netsStartPayment. Status code {}.".format(nets.status_code))
        sys.exit()


def reserveTicket(idToken, offer_id):
    headers = {'Accept': 'application/json', 'Authorization': 'Bearer ' + idToken}
    body = {"offers": [{
            "offer_id": offer_id,
            "count": 1
        }],
        "payment_type": 1,
        "payment_redirect_url": "https://atb-webshop-staging.web.app/payment?transaction_id={transaction_id}&payment_id={payment_id}&order_id={order_id}",
        "phone_number": ""
    }
    reservation = requests.post("https://api.staging.mittatb.no/ticket/v2/reserve",
                                json=body,
                                headers=headers)

    if reservation.status_code == 200:
        reservation.encoding = 'utf-8'
        reservationObject = {}
        reservationObject['order_id'] = str(reservation.json()['order_id'])
        reservationObject['payment_id'] = str(reservation.json()['payment_id'])
        reservationObject['transactionId'] = str(reservation.json()['transaction_id'])
        url = str(reservation.json()['url'])
        netsMerchantId = re.findall("merchantId=(.*)&transactionId", url)[0]
        netsTransactionId = re.findall("transactionId=(.*)", url)[0]
        reservationObject['netsMerchantId'] = netsMerchantId
        reservationObject['netsTransactionId'] = netsTransactionId
        return reservationObject
    else:
        print("ERROR reserveTicket. Status code {}.".format(reservation.status_code))
        sys.exit()


def getOffer(idToken):
    headers = {'Accept': 'application/json', 'Authorization': 'Bearer ' + idToken}
    body = {
        "products": ["ATB:PreassignedFareProduct:8808c360"],
        "travellers": [{
            "count": 1,
            "id": "ADULT",
            "user_type": "ADULT"
        }],
        "zones": ["ATB:TariffZone:1"]
    }
    offer = requests.post("https://api.staging.mittatb.no/ticket/v1/search/zones",
                          json=body,
                          headers=headers)

    if offer.status_code == 200:
        offer.encoding = 'utf-8'
        offer_id = str(offer.json()[0]['offer_id'])
        return offer_id
    else:
        print("ERROR getOffer. Status code {}.".format(offer.status_code))
        sys.exit()


# Get one batch of changeset
def authorization(email, password):
    authHeaders = {'Accept': 'application/json'}
    authBody = {
        "email": email,
        "password": password,
        "returnSecureToken": "true"
    }
    authResponse = requests.post("https://www.googleapis.com/identitytoolkit/v3/relyingparty/verifyPassword?key=AIzaSyDoat8ob5tewAXaPEhqbZKzx8e7LC5nuzQ",
                                 data=authBody,
                                 headers=authHeaders)

    if authResponse.status_code == 200:
        authResponse.encoding = 'utf-8'
        idToken = authResponse.json()['idToken']
        return idToken
    else:
        print("ERROR authorization. Status code {}.".format(authResponse.status_code))
        sys.exit()


if __name__ == '__main__':
    main(sys.argv[1:])
