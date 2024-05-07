import numpy as np
import pandas as pd
import umap
import tensorflow_hub as hub
import tensorflow as tf
import tensorflow_text as text
import torch


def normalization(embeds):
  norms = np.linalg.norm(embeds, 2, axis=1, keepdims=True)
  return embeds/norms

# preprocessor = hub.KerasLayer("https://tfhub.dev/google/universal-sentence-encoder-cmlm/multilingual-preprocess/2")
preprocessor = hub.KerasLayer("keras_models/cmlm/")
# encoder = hub.KerasLayer("https://tfhub.dev/google/LaBSE/2")
encoder = hub.KerasLayer("keras_models/labse/")

print()
print("Start")

# story = "karma"
story = "lpm"

# clean_split = pd.read_csv("../data/text_split/"+story+".clean.split.csv")
# clean_chunk = pd.read_csv("/content/all.clean.split_VChunk.csv")
clean_grain = pd.read_csv("../data/text_split/"+story+".granular_split.csv")
# clean_window = pd.read_csv("../data/text_split/"+story+".window_split.csv")

## Normal stories

# # Normal
# story1 = clean_split.loc[clean_split["story"] == 1]

# story2 = clean_split.loc[clean_split["story"] == 2]

# story3 = clean_split.loc[clean_split["story"] == 3]

# story4 = clean_split.loc[clean_split["story"] == 4]

# s1_sentences = tf.constant(story1["sentences"])
# s1_embeds = encoder(preprocessor(s1_sentences))["default"]
# s1_embeds = normalization(s1_embeds)
# # torch.save(s1_embeds, "s1_embeddings.pt")

# s2_sentences = tf.constant(story2["sentences"])
# s2_embeds = encoder(preprocessor(s2_sentences))["default"]
# s2_embeds = normalization(s2_embeds)
# # torch.save(s2_embeds, "s2_embeddings.pt")

# s3_sentences = tf.constant(story3["sentences"])
# s3_embeds = encoder(preprocessor(s3_sentences))["default"]
# s3_embeds = normalization(s3_embeds)
# # torch.save(s3_embeds, "s3_embeddings.pt")

# s4_sentences = tf.constant(story4["sentences"])
# s4_embeds = encoder(preprocessor(s4_sentences))["default"]
# s4_embeds = normalization(s4_embeds)
# # torch.save(s4_embeds, "s4_embeddings.pt")

# s1_embeds_numpy = pd.DataFrame(s1_embeds.numpy())

# s1_embeds_numpy.to_csv("../data/embeddings/mbert/"+story+"_s1_embeds_raw.csv", index = False)

# s2_embeds_numpy = pd.DataFrame(s2_embeds.numpy())

# s2_embeds_numpy.to_csv("../data/embeddings/mbert/"+story+"_s2_embeds_raw.csv", index = False)

# s3_embeds_numpy = pd.DataFrame(s3_embeds.numpy())

# s3_embeds_numpy.to_csv("../data/embeddings/mbert/"+story+"_s3_embeds_raw.csv", index = False)

# s4_embeds_numpy = pd.DataFrame(s4_embeds.numpy())

# s4_embeds_numpy.to_csv("../data/embeddings/mbert/"+story+"_s4_embeds_raw.csv", index = False)

# # e1e1 = np.matmul(s1_embeds, np.transpose(s1_embeds))
# # e1r2 = np.matmul(s1_embeds, np.transpose(s2_embeds))
# # e1f3 = np.matmul(s1_embeds, np.transpose(s3_embeds))
# # e1e4 = np.matmul(s1_embeds, np.transpose(s4_embeds))

# # r2e1 = np.matmul(s2_embeds, np.transpose(s1_embeds))
# # r2r2 = np.matmul(s2_embeds, np.transpose(s2_embeds))
# # r2f3 = np.matmul(s2_embeds, np.transpose(s3_embeds))
# # r2e4 = np.matmul(s2_embeds, np.transpose(s4_embeds))

# # f3e1 = np.matmul(s3_embeds, np.transpose(s1_embeds))
# # f3r2 = np.matmul(s3_embeds, np.transpose(s2_embeds))
# # f3f3 = np.matmul(s3_embeds, np.transpose(s3_embeds))
# # f3e4 = np.matmul(s3_embeds, np.transpose(s4_embeds))

# # e4e1 = np.matmul(s4_embeds, np.transpose(s1_embeds))
# # e4r2 = np.matmul(s4_embeds, np.transpose(s2_embeds))
# # e4f3 = np.matmul(s4_embeds, np.transpose(s3_embeds))
# # e4e4 = np.matmul(s4_embeds, np.transpose(s4_embeds))

# # full_similarity = pd.DataFrame()

# # ii = 1
# # names = ["english1.english1","english1.russian2","english1.french3","english1.english4",
# #          "russian2.english1","russian2.russian2","russian2.french3","russian2.english4",
# #          "french3.english1","french3.russian2","french3.french3","french3.english4",
# #          "english4.english1","english4.russian2","english4.french3","english4.english4"]
# # for temp in (e1e1, e1r2, e1f3, e1e4, r2e1, r2r2, r2f3, r2e4, f3e1, f3r2, f3f3, f3e4, e4e1, e4r2, e4f3, e4e4):
# #   temp_raw = pd.DataFrame(temp).stack().reset_index()
# #   temp_raw.columns = ['x.num','y.num','value']

# #   stories = names[(ii-1)].split(".")
# #   temp_raw['x.story'] = stories[0]
# #   temp_raw['y.story'] = stories[1]

# #   full_similarity = pd.concat([full_similarity, temp_raw], axis = 0)
# #   ii = ii + 1

# # full_similarity.to_csv("../data/embeddings/similarity_matrices/"+story+".combined_similarity.csv", index = False)

# reducer = umap.UMAP()
# full_embeds = pd.concat([pd.DataFrame(s1_embeds.numpy()), 
#                          pd.DataFrame(s2_embeds.numpy()), 
#                          pd.DataFrame(s3_embeds.numpy()), 
#                          pd.DataFrame(s4_embeds.numpy())], 
#                          ignore_index = True)

# print(full_embeds)

# embedding = reducer.fit_transform(full_embeds)
# index_col = np.repeat([1,2,3,4], repeats = [s1_embeds.shape[0], s2_embeds.shape[0], s3_embeds.shape[0], s4_embeds.shape[0]])

# umap_embeds = pd.DataFrame(embedding, columns = ["umap.x","umap.y"])
# umap_embeds["story"] = index_col

# umap_embeds.to_csv("../data/embeddings/"+story+".umap_embeds.csv", index = False)
# # files.download("karma.umap_embeds.csv")

# print()
# print("Done")
# print()

# """## Granular analysis"""

grain_df = pd.DataFrame()
umap_df = pd.DataFrame()

reducer = umap.UMAP()


print("Grains")
for grains in range(1, 16):
  print(grains)
  grain1 = clean_grain.loc[(clean_grain["story"] == 1) & (clean_grain["grain_size"] == grains)]

  grain2 = clean_grain.loc[(clean_grain["story"] == 2) & (clean_grain["grain_size"] == grains)]

  grain3 = clean_grain.loc[(clean_grain["story"] == 3) & (clean_grain["grain_size"] == grains)]

  grain4 = clean_grain.loc[(clean_grain["story"] == 4) & (clean_grain["grain_size"] == grains)]

  g1_sentences = tf.constant(grain1["sentences"])
  g1_embeds = encoder(preprocessor(g1_sentences))["default"]
  g1_embeds = normalization(g1_embeds)

  g2_sentences = tf.constant(grain2["sentences"])
  g2_embeds = encoder(preprocessor(g2_sentences))["default"]
  g2_embeds = normalization(g2_embeds)

  g3_sentences = tf.constant(grain3["sentences"])
  g3_embeds = encoder(preprocessor(g3_sentences))["default"]
  g3_embeds = normalization(g3_embeds)

  g4_sentences = tf.constant(grain4["sentences"])
  g4_embeds = encoder(preprocessor(g4_sentences))["default"]
  g4_embeds = normalization(g4_embeds)

  # g1_embeds_numpy = pd.DataFrame(g1_embeds.numpy())
  # g1_embeds_numpy.to_csv("../data/embeddings/mbert/"+story+"_s1"+"_g"+str(grains)+"_embeds_raw.csv", index = False)

  # g2_embeds_numpy = pd.DataFrame(g2_embeds.numpy())
  # g2_embeds_numpy.to_csv("../data/embeddings/mbert/"+story+"_s2"+"_g"+str(grains)+"_embeds_raw.csv", index = False)

  # g3_embeds_numpy = pd.DataFrame(g3_embeds.numpy())
  # g3_embeds_numpy.to_csv("../data/embeddings/mbert/"+story+"_s3"+"_g"+str(grains)+"_embeds_raw.csv", index = False)

  # g4_embeds_numpy = pd.DataFrame(g4_embeds.numpy())
  # g4_embeds_numpy.to_csv("../data/embeddings/mbert/"+story+"_s4"+"_g"+str(grains)+"_embeds_raw.csv", index = False)

  # Umap:

  index_col = np.repeat([1,2,3,4], repeats = [g1_embeds.shape[0], g2_embeds.shape[0], g3_embeds.shape[0], g4_embeds.shape[0]])
  
  full_embeds = pd.concat([pd.DataFrame(g1_embeds.numpy()), pd.DataFrame(g2_embeds.numpy()), pd.DataFrame(g3_embeds.numpy()), pd.DataFrame(g4_embeds.numpy())], ignore_index = True)
  
  full_embeds["story"] = index_col
  full_embeds["grain"] = grains
  
  # full_embeds.to_csv("../data/embeddings/mbert/"+story+"_grainsize_"+str(grains)+"_embeds_raw.csv", index = False)
  

  story_info = full_embeds[["story","grain"]].copy()
  print(story_info)
  
  embeddings = full_embeds.drop(["story","grain"], axis = 1)
  print(embeddings)

  embedding = reducer.fit_transform(embeddings)

  umap_embeds = pd.DataFrame(embedding, columns = ["umap.x","umap.y"])
  umap_embeds["story"] = story_info["story"]
  umap_embeds["grain"] = grains

  umap_df = pd.concat([umap_df, umap_embeds], axis = 0)

  # Similarity part:

  # e1e1_grain = np.matmul(g1_embeds, np.transpose(g1_embeds))
  # e1r2_grain = np.matmul(g1_embeds, np.transpose(g2_embeds))
  # e1f3_grain = np.matmul(g1_embeds, np.transpose(g3_embeds))
  # e1e4_grain = np.matmul(g1_embeds, np.transpose(g4_embeds))

  # r2e1_grain = np.matmul(g2_embeds, np.transpose(g1_embeds))
  # r2r2_grain = np.matmul(g2_embeds, np.transpose(g2_embeds))
  # r2f3_grain = np.matmul(g2_embeds, np.transpose(g3_embeds))
  # r2e4_grain = np.matmul(g2_embeds, np.transpose(g4_embeds))

  # f3e1_grain = np.matmul(g3_embeds, np.transpose(g1_embeds))
  # f3r2_grain = np.matmul(g3_embeds, np.transpose(g2_embeds))
  # f3f3_grain = np.matmul(g3_embeds, np.transpose(g3_embeds))
  # f3e4_grain = np.matmul(g3_embeds, np.transpose(g4_embeds))

  # e4e1_grain = np.matmul(g4_embeds, np.transpose(g1_embeds))
  # e4r2_grain = np.matmul(g4_embeds, np.transpose(g2_embeds))
  # e4f3_grain = np.matmul(g4_embeds, np.transpose(g3_embeds))
  # e4e4_grain = np.matmul(g4_embeds, np.transpose(g4_embeds))

  # ii = 1
  # names = ["english1.english1","english1.russian2","english1.french3","english1.english4","russian2.english1","russian2.russian2","russian2.french3","russian2.english4","french3.english1","french3.russian2","french3.french3","french3.english4","english4.english1","english4.russian2","english4.french3","english4.english4"]
  # for temp_mat in (e1e1_grain, e1r2_grain, e1f3_grain, e1e4_grain, r2e1_grain, r2r2_grain, r2f3_grain, r2e4_grain, f3e1_grain, f3r2_grain, f3f3_grain, f3e4_grain, e4e1_grain, e4r2_grain, e4f3_grain, e4e4_grain):
  #   temp = pd.DataFrame(temp_mat).stack().reset_index()
  #   temp.columns = ['x.num','y.num','value']

  #   stories = names[(ii-1)].split(".")

  #   temp['x.story'] = stories[0]
  #   temp['y.story'] = stories[1]
  #   temp['grain'] = grains

  #   grain_df = pd.concat([grain_df, temp], axis = 0)
  #   ii = ii + 1

# grain_df.to_csv("karma.grained_similarity.csv", index = False)

umap_df.to_csv("../data/embeddings/"+story+".grained_umap.csv", index = False)
print("Finished grains\n")

# # """# Window analysis"""

# window_df = pd.DataFrame()
# umap_df = pd.DataFrame()

# reducer = umap.UMAP()

# print("Windows")
# for window in range(1, 21):
#   print(window)
#   window1 = clean_window.loc[(clean_window["story"] == 1) & (clean_window["window"] == window)]

#   window2 = clean_window.loc[(clean_window["story"] == 2) & (clean_window["window"] == window)]

#   window3 = clean_window.loc[(clean_window["story"] == 3) & (clean_window["window"] == window)]

#   window4 = clean_window.loc[(clean_window["story"] == 4) & (clean_window["window"] == window)]

#   g1_sentences = tf.constant(window1["sentences"])
#   g1_embeds = encoder(preprocessor(g1_sentences))["default"]
#   g1_embeds = normalization(g1_embeds)

#   g2_sentences = tf.constant(window2["sentences"])
#   g2_embeds = encoder(preprocessor(g2_sentences))["default"]
#   g2_embeds = normalization(g2_embeds)

#   g3_sentences = tf.constant(window3["sentences"])
#   g3_embeds = encoder(preprocessor(g3_sentences))["default"]
#   g3_embeds = normalization(g3_embeds)

#   g4_sentences = tf.constant(window4["sentences"])
#   g4_embeds = encoder(preprocessor(g4_sentences))["default"]
#   g4_embeds = normalization(g4_embeds)

#   # g1_embeds_numpy = pd.DataFrame(g1_embeds.numpy())
#   # g1_embeds_numpy.to_csv("s1"+"_g"+str(window)+"_embeds_raw.csv", index = False)

#   # g2_embeds_numpy = pd.DataFrame(g2_embeds.numpy())
#   # g2_embeds_numpy.to_csv("s2"+"_g"+str(window)+"_embeds_raw.csv", index = False)

#   # g3_embeds_numpy = pd.DataFrame(g3_embeds.numpy())
#   # g3_embeds_numpy.to_csv("s3"+"_g"+str(window)+"_embeds_raw.csv", index = False)

#   # g4_embeds_numpy = pd.DataFrame(g4_embeds.numpy())
#   # g4_embeds_numpy.to_csv("s4"+"_g"+str(window)+"_embeds_raw.csv", index = False)

#   # Umap:

#   full_embeds = pd.concat([pd.DataFrame(g1_embeds.numpy()), pd.DataFrame(g2_embeds.numpy()), pd.DataFrame(g3_embeds.numpy()), pd.DataFrame(g4_embeds.numpy())], ignore_index = True)
#   embedding = reducer.fit_transform(full_embeds)
#   index_col = np.repeat([1,2,3,4], repeats = [g1_embeds.shape[0], g2_embeds.shape[0], g3_embeds.shape[0], g4_embeds.shape[0]])

#   story_info = full_embeds[["story","window"]].copy()
#   print(story_info)
  
#   embeddings = full_embeds.drop(["story","window"], axis = 1)
#   print(embeddings)

#   embedding = reducer.fit_transform(embeddings)

#   umap_embeds = pd.DataFrame(embedding, columns = ["umap.x","umap.y"])
#   umap_embeds["story"] = story_info["story"]
#   umap_embeds["window"] = window

#   umap_df = pd.concat([umap_df, umap_embeds], axis = 0)


#   # Similarity part:

#   # e1e1_window = np.matmul(g1_embeds, np.transpose(g1_embeds))
#   # e1r2_window = np.matmul(g1_embeds, np.transpose(g2_embeds))
#   # e1f3_window = np.matmul(g1_embeds, np.transpose(g3_embeds))
#   # e1e4_window = np.matmul(g1_embeds, np.transpose(g4_embeds))

#   # r2e1_window = np.matmul(g2_embeds, np.transpose(g1_embeds))
#   # r2r2_window = np.matmul(g2_embeds, np.transpose(g2_embeds))
#   # r2f3_window = np.matmul(g2_embeds, np.transpose(g3_embeds))
#   # r2e4_window = np.matmul(g2_embeds, np.transpose(g4_embeds))

#   # f3e1_window = np.matmul(g3_embeds, np.transpose(g1_embeds))
#   # f3r2_window = np.matmul(g3_embeds, np.transpose(g2_embeds))
#   # f3f3_window = np.matmul(g3_embeds, np.transpose(g3_embeds))
#   # f3e4_window = np.matmul(g3_embeds, np.transpose(g4_embeds))

#   # e4e1_window = np.matmul(g4_embeds, np.transpose(g1_embeds))
#   # e4r2_window = np.matmul(g4_embeds, np.transpose(g2_embeds))
#   # e4f3_window = np.matmul(g4_embeds, np.transpose(g3_embeds))
#   # e4e4_window = np.matmul(g4_embeds, np.transpose(g4_embeds))

#   # ii = 1
#   # names = ["english1.english1","english1.russian2","english1.french3","english1.english4","russian2.english1","russian2.russian2","russian2.french3","russian2.english4","french3.english1","french3.russian2","french3.french3","french3.english4","english4.english1","english4.russian2","english4.french3","english4.english4"]
#   # for temp_mat in (e1e1_window, e1r2_window, e1f3_window, e1e4_window, r2e1_window, r2r2_window, r2f3_window, r2e4_window, f3e1_window, f3r2_window, f3f3_window, f3e4_window, e4e1_window, e4r2_window, e4f3_window, e4e4_window):
#   #   temp = pd.DataFrame(temp_mat).stack().reset_index()
#   #   temp.columns = ['x.num','y.num','value']

#   #   stories = names[(ii-1)].split(".")

#   #   temp['x.story'] = stories[0]
#   #   temp['y.story'] = stories[1]
#   #   temp['window'] = window

#   #   window_df = pd.concat([window_df, temp], axis = 0)
#   #   ii = ii + 1

# # window_df.to_csv("karma.windowed_similarity.csv", index = False)

# umap_df.to_csv("../data/embeddings/"+story+".windowed_umap.csv", index = False)

