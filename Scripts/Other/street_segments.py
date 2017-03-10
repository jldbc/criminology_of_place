import requests # pip install requests

# generate a token with your client id and client secret
token = requests.post('https://www.arcgis.com/sharing/rest/oauth2/token/', params={
  'f': 'json',
  'client_id': 'gDxBYZbFpUTblc5U',
  'client_secret': 'f20a68b266d844078f88c78564bb6c34',
  'grant_type': 'client_credentials',
  'expiration': '1440'
})

print(token.json()['access_token']);

data = requests.post('http://geoenrich.arcgis.com/arcgis/rest/services/World/GeoenrichmentServer/Geoenrichment/enrich', params={
  'f': 'json',
  'token': token.json()['access_token'],
  'studyAreas': '[{"geometry":{"x":-117.1956,"y":34.0572}}]'
})

print(data.json())



def get_address(lat,lon):
	baseurl = 'http://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/reverseGeocode?location='
	url = baseurl + str(lat) + '%2C' + lon + '&outSR=&f=pjson'
	r = requests.get(url)
	return r

get_address('-123.076807','44.939852')


baseurl = 'http://geocode.arcgis.com/arcgis/rest/services/World/GeocodeServer/findAddressCandidates?addresses='
addrstring = '{"records":[{"attributes":{"OBJECTID":1,"SingleLine":"2110 Eola Dr NW, Salem, Oregon, 97304"}}]}'
addrstring = '2110+eola+dr&City=salem&Region=OR&Postal=97304&outFields=*&forStorage=false&f=pjson'
addrstring = '350+5th+ave&City=New+York&Region=NY&Postal=10118&outFields=*&forStorage=false&f=pjson'

#addrstring = '{"records":[{"attributes":{"OBJECTID":1,"SingleLine":"380 New York St, Redlands, California, 92373"}}]}'

tokenstr = '&token=' + token.json()['access_token']
url = baseurl + addrstring
r = requests.get(url)






#