import requests
import pandas as pd
from pprint import pprint
from datetime import datetime
import simplejson as js
import ast

startTime = datetime.now()

print('Start.')

artist_ids = []
album_ids = []
track_ids = []

artist_result = open("C:/Users/RayGarraty/Desktop/DataCup/result_artist.txt", "w",encoding='utf-8')
track_result = open("C:/Users/RayGarraty/Desktop/DataCup/result_track.txt", "w",encoding='utf-8')
album_result = open("C:/Users/RayGarraty/Desktop/DataCup/result_album.txt", "w",encoding='utf-8')

i = 0
j = 0
k = 0

#genre_id,ts_listen,media_id,album_id,context_type,release_date,platform_name,platform_family,media_duration,listen_type,user_gender,user_id,artist_id,user_age,is_listened
with open('C:/Users/RayGarraty/Desktop/DataCup/train.csv') as f:
	for line in f:
		if i>0:
			line = line.split(',')
			artist_id=int(line[12])
			track_id=int(line[2])
			album_id=int(line[3])
			artist_ids.append(artist_id)
			track_ids.append(track_id)
			album_ids.append(album_id)
		i = i + 1
print("Done with training ids.")


#sample_id,genre_id,ts_listen,media_id,album_id,context_type,release_date,platform_name,platform_family,media_duration,listen_type,user_gender,user_id,artist_id,user_age
with open('C:/Users/RayGarraty/Desktop/DataCup/test.csv') as g:
	for line in g:
		if k>0:
			line = line.split(',')
			artist_id=int(line[13])
			track_id=int(line[3])
			album_id=int(line[4])
			artist_ids.append(artist_id)
			track_ids.append(track_id)
			album_ids.append(album_id)
			k = k + 1
print("Done with test ids.")

artist_set = []
album_set = []
track_set = []

artist_set = (set(artist_ids))
print("Länge Artist-Set: "+str(len(artist_set)))

track_set = (set(track_ids))
print("Länge Tracks-Set: "+str(len(track_set)))

album_set = (set(album_ids))
print("Länge Album-Set: "+str(len(album_set)))

# extract artist information
print("Start to collect artist information.")
for x in artist_set:
	j = j + 1
	try:
		data = requests.get("https://api.deezer.com/artist/"+str(x)+".json()")
	except BaseException as e:
		print('Failed to upload to ftp: '+ str(e))
		print(data)
	try:
		artist_result.write(str(x)+';'+str(data)+'\n')
	except:
		print(data)
	if (j%1000==0):
		print("done with "+str(j)+", Dauer: "+str(datetime.now() - startTime)+" um "+str(datetime.now()))
print("Anzahl Artist: " + str(j))

artist_result.close

# extract track information
print("Start to collect track information.")
j = 0
for x in track_set:
	j = j + 1
	try:
		data = requests.get("https://api.deezer.com/track/"+str(x)+".json()")
	except BaseException as e:
		print('Failed to upload to ftp: '+ str(e))
		print(data)
	try:
		track_result.write(str(x)+';'+str(data)+'\n')
	except:
		print(data)
	if (j%1000==0):
		print("done with "+str(j)+", Dauer: "+str(datetime.now() - startTime)+" um "+str(datetime.now()))
print("Anzahl Artist: " + str(j))

track_result.close

# extract track information
print("Start to collect album information.")
j = 0
for x in album_set:
	j = j + 1
	try:
		data = requests.get("https://api.deezer.com/track/"+str(x)+".json()")
	except BaseException as e:
		print('Failed to upload to ftp: '+ str(e))
		print(data)
	try:
		album_result.write(str(x)+';'+str(data)+'\n')
	except:
		print(data)
	if (j%1000==0):
		print("done with "+str(j)+", Dauer: "+str(datetime.now() - startTime)+" um "+str(datetime.now()))
print("Anzahl Artist: " + str(j))
			
album_result.close

artist_result = open("C:/Users/RayGarraty/Desktop/DataCup/result_artist.txt", "r")
track_result = open("C:/Users/RayGarraty/Desktop/DataCup/result_track.txt", "r")
album_result = open("C:/Users/RayGarraty/Desktop/DataCup/result_album.txt", "r")

artist_result.close
track_result.close
album_result.close