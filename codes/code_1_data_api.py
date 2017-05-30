import requests
import pandas as pd
from pprint import pprint
from datetime import datetime
import simplejson as js
import ast

startTime = datetime.now()

print('Start.')

not_done = []

result = open("C:/Users/RayGarraty/Desktop/DataCup/result_user_albums.txt", "w",encoding='utf-8')

i = 0
j = 0
k = 0

#genre_id,ts_listen,media_id,album_id,context_type,release_date,platform_name,platform_family,media_duration,listen_type,user_gender,user_id,artist_id,user_age,is_listened
with open('C:/Users/RayGarraty/Desktop/DataCup/train.csv') as f:
	for line in f:
		if i>0:
			line = line.split(',')
			id=int(line[12])
			not_done.append(id)
		i = i + 1

print("Done with training.")


#sample_id,genre_id,ts_listen,media_id,album_id,context_type,release_date,platform_name,platform_family,media_duration,listen_type,user_gender,user_id,artist_id,user_age
with open('C:/Users/RayGarraty/Desktop/DataCup/test.csv') as g:
	for line in g:
		if k>0:
			line = line.split(',')
			id=int(line[13])
			not_done.append(id)
		k = k + 1
print("Done with test.")

print("Länge Liste total: " + str(len(not_done)))

not_done_set = (set(not_done)))

print("Länge already done: "+str(len(not_done_set)))


for x in not_done:
	j = j + 1
	try:
		data = requests.get("https://api.deezer.com/user/"+str(x)+"/artists").json()
	except BaseException as e:
		print('Failed to upload to ftp: '+ str(e))
		print(data)
	try:
		result.write(str(x)+';'+str(data)+'\n')
	except:
		print(data)
	if (j%1000==0):
		print("done with "+str(j)+", Dauer: "+str(datetime.now() - startTime)+" um "+str(datetime.now()))
print("Anzahl Artist: " + str(j))
			
	
result.close
result = open("C:/Users/RayGarraty/Desktop/DataCup/result_user_albums.txt", "r")
result.close