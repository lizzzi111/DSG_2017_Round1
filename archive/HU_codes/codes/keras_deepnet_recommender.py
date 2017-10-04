import rpy2.robjects as robjects

import pandas as pd
import keras
from keras.layers import Input, Dense, Embedding, concatenate, Flatten, Dropout
from keras.layers.normalization import BatchNormalization
from keras.models import Model
from keras.regularizers import l2
from keras.optimizers import Adam

#path = "/Users/hauptjoh/Dropbox/DSG17/data/"
#ratings = pd.read_csv(path+"filtering_data.csv")

path = "/Users/hauptjoh/Dropbox/DSG17/data/"
known = pd.read_csv(path+"data_train.csv")
unknown = pd.read_csv(path+"data_test.csv")
#tr = robjects.r['readRDS'](path + 'data_train.Rds')
#ts = robjects.r['readRDS'](path + 'data_test.Rds')
#tr = pd.DataFrame(tr)
#ts = pd.DataFrame(ts)

# Prepare the data
# Create a placeholder for the IDs new in the test data
newUsers = list(unknown.user_id[~unknown.user_id.isin(known.user_id)])         + list(known.user_id.value_counts().keys()[known.user_id.value_counts() == 1])
newSongs = list(unknown.media_id[~unknown.media_id.isin(known.media_id)])      + list(known.media_id.value_counts().keys()[known.media_id.value_counts() == 1])[-1000:]
newArtists = list(unknown.artist_id[~unknown.artist_id.isin(known.artist_id)]) + list(known.artist_id.value_counts().keys()[known.artist_id.value_counts() == 1])[-1000:]
# In particular, assign IDs from 0 to N for users and songs
# Use enumerate() to create a list of the new and original IDs
users = list(enumerate([i for i in known.user_id.unique() if i not in newUsers]))
songs = list(enumerate([i for i in known.media_id.unique() if i not in newSongs]))
artists = list(enumerate([i for i in known.artist_id.unique() if i not in newArtists]))
# Create a dictionary with old IDs to new IDs
userid2idx = {o:i for i,o in users}
songid2idx = {o:i for i,o in songs}
artistid2idx = {o:i for i,o in artists}
# Update with new/rare entries
userid2idx.update({o:(max(userid2idx.values())+1) for o in newUsers})
songid2idx.update({o:(max(songid2idx.values())+1) for o in newSongs})
artistid2idx.update({o:(max(artistid2idx.values())+1) for o in newArtists})
# Create id variable with the new IDs
known['userIdx'] = known.user_id.apply(lambda x: userid2idx[x])
known['songIdx'] = known.media_id.apply(lambda x: songid2idx[x])
known['artistIdx'] = known.artist_id.apply(lambda x: artistid2idx[x])
unknown['userIdx'] = unknown.user_id.apply(lambda x: userid2idx[x])
unknown['songIdx'] = unknown.media_id.apply(lambda x: songid2idx[x])
unknown['artistIdx'] = unknown.artist_id.apply(lambda x: artistid2idx[x])

# Train/test data
ts = known.groupby(["userIdx"]).tail(3) # last observation by user
tr = known.groupby(["userIdx"], group_keys=False).apply(lambda x: x[:-3])
# Move songs that appear only in ts to tr
strayObs = ts.songIdx.isin(tr.songIdx) & ts.userIdx.isin(tr.userIdx) & ts.artistIdx.isin(tr.artistIdx)
tr = tr.append(ts[~strayObs])
ts = ts[strayObs]

# Create an input layer with one row of IDs
user_in = Input(shape = (1,), dtype='int64', name = "user_in")
song_in = Input(shape = (1,), dtype='int64', name = "song_in")
artist_in = Input(shape = (1,), dtype='int64', name = "artist_in")
# Create an embedding assigning k latent factors to each ID
# These will be optimized
# A regulariztaion is added to avoid very large weights
n_users = tr.userIdx.nunique()
n_songs = tr.songIdx.nunique()
n_artists = tr.artistIdx.nunique()
u = Embedding(n_users, 50, input_length=1, embeddings_regularizer=l2(1e-5))(user_in)
s = Embedding(n_songs, 50, input_length=1, embeddings_regularizer=l2(1e-5))(song_in)
a = Embedding(n_artists, 50, input_length=1, embeddings_regularizer=l2(1e-5))(artist_in)

# Specify what to do with the layers
# We want to multiply them into a 'rating' matrix
x = concatenate([u, s, a])
x = Flatten()(x)
x= Dense(128, activation='relu')(x)
x = Dropout(0.5)(x)
x = BatchNormalization()(x)
#x = Dropout(0.5)(Dense(128, activation='relu')(x))
#x = BatchNormalization()(x)
x = Dense(64, activation='relu')(x) 
x = Dropout(0.5)(x)
x = Dense(1, activation = "sigmoid")(x)
#x = merge([x, ub], mode = 'sum')
#x = merge([x, sb], mode = 'sum') # Can this be included in the line above?
# Then we specify the model that we want to use
model = Model([user_in, song_in, artist_in], x) # 
model.compile(Adam(0.001), loss="binary_crossentropy", metrics = ['accuracy'])

# Then we run the estimations
model.fit([tr.userIdx, tr.songIdx, tr.artistIdx], tr.is_listened,  #
validation_data = ([ts.userIdx, ts.songIdx, ts.artistIdx], ts.is_listened),
batch_size = 262144, epochs = 10,
callbacks = keras.callbacks.ModelCheckpoint(path + 'weights.{epoch:02d}-{val_loss:.2f}.hdf5', monitor='val_loss', verbose=0, save_best_only=True, save_weights_only=False, mode='auto', period=1)
)

# predict on class set
pred = pd.DataFrame(is_listened = model.predict([unknown.userIdx, unknown.songIdx, unknown.artistIdx]))
pred["sample_id"] = unknown.sample_id

