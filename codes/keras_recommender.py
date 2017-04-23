import pandas as pd
import keras
from keras.layers import Input, Dense, Embedding, merge, Flatten
from keras.models import Model
from keras.regularizers import l2
from keras.optimizers import Adam

ratings = pd.read_csv("/Users/maj/Dropbox/DSG17/data/filtering_data.csv")
# Prepare the data
# In particular, assign IDs from 0 to N for users and songs
# Use enumerate() to create a list of the new and original IDs
users = enumerate(ratings.user_id.unique())
songs = enumerate(ratings.media_id.unique())
# Create a dictionary with old IDs to new IDs
userid2idx = {o:i for i,o in users}
songid2idx = {o:i for i,o in songs}
# Create id variable with the new IDs
ratings['userIdx'] = ratings.user_id.apply(lambda x: userid2idx[x])
ratings['songIdx'] = ratings.media_id.apply(lambda x: songid2idx[x])

# Train/test data
ts = ratings.groupby(["user_id"]).tail(3) # last observation by user
tr = ratings.groupby(["user_id"], group_keys=False).apply(lambda x: x[:-3])
# Move songs that appear only in ts to tr
bothSongIdx = ts.songIdx.isin(tr.songIdx)
tr = tr.append(ts[~bothSongIdx])
ts = ts[bothSongIdx]

# Create an input layer with one row of IDs
user_in = Input(shape = (1,), dtype='int64', name = "user_in")
song_in = Input(shape = (1,), dtype='int64', name = "song_in")
# Create an embedding assigning k latent factors to each ID
# These will be optimized
# A regulariztaion is added to avoid very large weights
n_users = tr.userIdx.nunique()
n_songs = tr.songIdx.nunique()
u = Embedding(n_users, 20, input_length=1, embeddings_regularizer=l2(1e-5))(user_in)
s = Embedding(n_songs, 20, input_length=1, embeddings_regularizer=l2(1e-5))(song_in)
# Also create 'biases', i.e. a user and song specific value that is added
ub = Flatten()(Embedding(n_users, 1, input_length = 1)(user_in))
sb = Flatten()(Embedding(n_songs, 1, input_length = 1)(song_in))

# Specify what to do with the layers
# We want to multiply them into a 'rating' matrix
x = merge([u, s], mode='dot')
x = Flatten()(x)
x = merge([x, ub], mode = 'sum')
x = merge([x, sb], mode = 'sum') # Can this be included in the line above?
# Then we specify the model that we want to use
model = Model([user_in, song_in], x)
model.compile(Adam(0.001), loss="binary_crossentropy", metrics =['accuracy'])

# Then we run the estimations
model.fit([tr['userIdx'], tr['songIdx']], tr['is_listened'], 
validation_data = ([ts['userIdx'],ts['songIdx']], ts['is_listened']),
batch_size = 648576, epochs = 10)

model.predict([ratings.userIdx, ratings.songIdx])
model.get_weights()[0]. 
np.save("song_weights.npy", my_matrix)
