library(kerasR)

path = "/Users/maj/Dropbox/DSG17/data/"
ratings = read.csv(file.path(path,"filtering_data.csv"))
# Prepare the data
# # In particular, assign IDs from 0 to N for users and songs
# # Use enumerate() to create a list of the new and original IDs
# users = enumerate(ratings.user_id.unique())
# songs = enumerate(ratings.media_id.unique())
# # Create a dictionary with old IDs to new IDs
# userid2idx = {o:i for i,o in users}
# songid2idx = {o:i for i,o in songs}
# # Create id variable with the new IDs
# ratings['userIdx'] = ratings.user_id.apply(lambda x: userid2idx[x])
# ratings['songIdx'] = ratings.media_id.apply(lambda x: songid2idx[x])

ratings$userIdx <- factor(ratings$user_id)
ratings$songIdx <- factor(ratings$media_id)

# Create train and test set
msk = np.random.rand(len(ratings)) < 0.8
tr = ratings[msk]
ts = ratings[~msk]

# Not sure if the functional model below can be done in R as a sequential model...
mod <- Sequential()

# Create an input layer with one row of IDs
user_in = Input(shape = (1,), dtype='int64', name = "user_in")
song_in = Input(shape = (1,), dtype='int64', name = "song_in")
# Create an embedding assigning k latent factors to each ID
# These will be optimized
# A regulariztaion is added to avoid very large weights
n_users = tr.userIdx.nunique()
n_songs = tr.songIdx.nunique()
u = Embedding(n_users, 50, input_length=1, embeddings_regularizer=l2(1e-5))(user_in)
s = Embedding(n_songs, 50, input_length=1, embeddings_regularizer=l2(1e-5))(song_in)
# Also create 'biases', i.e. a user and song specific value that is added
ub = Flatten()(Embedding(n_users, 1, input_length = 1)(user_in))
sb = Flatten()(Embedding(n_songs, 1, input_length = 1)(song_in))

# Specify what to do with the layers
# We want to multiply them into a 'rating' matrix
x = merge([u, s], mode='concat')
x = Flatten()(x)
x = Dense(100, activation='relu')(x)
x = Dropout(0.4)(x)
x = Dense(1)(x)
#x = merge([x, ub], mode = 'sum')
#x = merge([x, sb], mode = 'sum') # Can this be included in the line above?
# Then we specify the model that we want to use
model = Model([user_in, song_in], x)
model.compile(Adam(0.001), loss="binary_crossentropy")

# Then we run the estimations
model.fit([tr.userIdx, tr.songIdx], tr.is_listened, 
          validation_split = 0.2,
          batch_size = 262144, epochs = 5)
