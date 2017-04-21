import pandas as pd
import keras
from keras.layers import Input, Dense, Embedding, merge, Flatten, Dropout
from keras.models import Model
from keras.regularizers import l2
from keras.optimizers import Adam

ratings = pd.read_csv("/Users/hauptjoh/Dropbox/DSG17/data/filtering_data.csv")
# Prepare the data
# In particular, assign IDs from 0 to N for users and songs
# Use enumerate() to create a list of the new and original IDs
users = enumerate(ratings.user_id.unique())
songs = enumerate(ratings.media_id.unique())
# Create a dictionary with old IDs to new IDs
userid2idx = {o:i for i,o in users}
songid2idx = {o:i for i,o in songs}
# Create id variable with the new IDs
ratings.userIdx = ratings.user_id.apply(lambda x: userid2idx[x])
ratings.songIdx = ratings.media_id.apply(lambda x: songid2idx[x])


# Create an input layer with one row of IDs
user_in = Input(shape = (1,), dtype='int64', name = "user_in")
song_in = Input(shape = (1,), dtype='int64', name = "song_in")
# Create an embedding assigning k latent factors to each ID
# These will be optimized
# A regulariztaion is added to avoid very large weights
n_users = ratings.userIdx.nunique()
n_songs = ratings.songIdx.nunique()
u = Embedding(n_users, 50, input_length=1, W_regularizer=l2(1e-5))(user_in)
s = Embedding(n_songs, 50, input_length=1, W_regularizer=l2(1e-5))(song_in)
# Also create 'biases', i.e. a user and song specific value that is added
ub = Flatten()(Embedding(n_users, 1, input_length = 1)(user_in))
sb = Flatten()(Embedding(n_songs, 1, input_length = 1)(song_in))

# Specify what to do with the layers
# We want to multiply them into a 'rating' matrix
x = merge([u, s], mode='concat')
x = Flatten()(x)
x = Dense(100, activation='relu')(x)
x = Dropout(0.25)(x)
x = Dense(1)(x)
#x = merge([x, ub], mode = 'sum')
#x = merge([x, sb], mode = 'sum') # Can this be included in the line above?
# Then we specify the model that we want to use
model = Model([user_in, song_in], x)
model.compile(Adam(0.001), loss="mse")

# Then we run the estimations
model.fit([ratings.userIdx, ratings.songIdx], ratings.is_listened, batch_size = 512, nb_epoch = 5)
