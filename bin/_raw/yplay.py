import requests, sys, json

argstr = ' '.join(sys.argv[1:])
params = [('q', argstr), ('max-results', '1'), 
          ('alt', 'json'), ('orderby', 'relevance')]

r = requests.get("https://gdata.youtube.com/feeds/api/videos", params=params)
j = json.loads(r.text)

print(j['feed']['entry'][0]['link'][0]['href'])
