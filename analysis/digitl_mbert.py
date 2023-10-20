import numpy as np
import pandas as pd
import umap
## Grains ----

umap_df = pd.DataFrame()

reducer = umap.UMAP()

print("Starting for grains.")

for grains in range(1, 16):
  print(grains)
  file = '../data/embeddings/mbert/grainsize_'
  full_embeds = pd.read_csv(file + str(grains)+'_embeds_raw.csv') 

  story_info = full_embeds[["story","grain"]].copy()
  print(story_info)
  
  embeddings = full_embeds.drop(["story","grain"], axis = 1)
  print(embeddings)

  embedding = reducer.fit_transform(embeddings)

  umap_embeds = pd.DataFrame(embedding, columns = ["umap.x","umap.y"])
  umap_embeds["story"] = story_info["story"]
  umap_embeds["grain"] = grains

  umap_df = pd.concat([umap_df, umap_embeds], axis = 0)

print("Finished:")
print(umap_df)
umap_df.to_csv("../data/embeddings/karma.grained_umap.csv", index = False)
print("Wrote files.")

## Sliding window ----
umap_df = pd.DataFrame()

reducer = umap.UMAP()

print("Starting for sliding window.")

for window in range(1, 16):
  print(window)
  file = '../data/embeddings/mbert/window_'
  full_embeds = pd.read_csv(file + str(window)+'_embeds_raw.csv') 

  story_info = full_embeds[["story","window"]].copy()
  print(story_info)
  
  embeddings = full_embeds.drop(["story","window"], axis = 1)
  print(embeddings)

  embedding = reducer.fit_transform(embeddings)

  umap_embeds = pd.DataFrame(embedding, columns = ["umap.x","umap.y"])
  umap_embeds["story"] = story_info["story"]
  umap_embeds["window"] = window

  umap_df = pd.concat([umap_df, umap_embeds], axis = 0)

print("Finished:")
print(umap_df)
umap_df.to_csv("../data/embeddings/karma.window_umap.csv", index = False)
print("Wrote files.")






# clean_split = pd.read_csv("/content/all.clean.split_V2.csv")
# clean_chunk = pd.read_csv("/content/all.clean.split_VChunk.csv")
# clean_grain = pd.read_csv("/content/granular_split.csv")
# clean_window = pd.read_csv("/content/window_split.csv")

# """## Normal stories"""

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

# s1_embeds_numpy.to_csv("s1_embeds_raw.csv", index = False)

# s2_embeds_numpy = pd.DataFrame(s2_embeds.numpy())

# s2_embeds_numpy.to_csv("s2_embeds_raw.csv", index = False)

# s3_embeds_numpy = pd.DataFrame(s3_embeds.numpy())

# s3_embeds_numpy.to_csv("s3_embeds_raw.csv", index = False)

# s4_embeds_numpy = pd.DataFrame(s4_embeds.numpy())

# s4_embeds_numpy.to_csv("s4_embeds_raw.csv", index = False)

# # files.download("s1_embeds_raw.csv")
# # files.download("s2_embeds_raw.csv")
# # files.download("s3_embeds_raw.csv")
# # files.download("s4_embeds_raw.csv")

# e1e1 = np.matmul(s1_embeds, np.transpose(s1_embeds))
# e1r2 = np.matmul(s1_embeds, np.transpose(s2_embeds))
# e1f3 = np.matmul(s1_embeds, np.transpose(s3_embeds))
# e1e4 = np.matmul(s1_embeds, np.transpose(s4_embeds))

# r2e1 = np.matmul(s2_embeds, np.transpose(s1_embeds))
# r2r2 = np.matmul(s2_embeds, np.transpose(s2_embeds))
# r2f3 = np.matmul(s2_embeds, np.transpose(s3_embeds))
# r2e4 = np.matmul(s2_embeds, np.transpose(s4_embeds))

# f3e1 = np.matmul(s3_embeds, np.transpose(s1_embeds))
# f3r2 = np.matmul(s3_embeds, np.transpose(s2_embeds))
# f3f3 = np.matmul(s3_embeds, np.transpose(s3_embeds))
# f3e4 = np.matmul(s3_embeds, np.transpose(s4_embeds))

# e4e1 = np.matmul(s4_embeds, np.transpose(s1_embeds))
# e4r2 = np.matmul(s4_embeds, np.transpose(s2_embeds))
# e4f3 = np.matmul(s4_embeds, np.transpose(s3_embeds))
# e4e4 = np.matmul(s4_embeds, np.transpose(s4_embeds))

# full_similarity = pd.DataFrame()

# ii = 1
# names = ["english1.english1","english1.russian2","english1.french3","english1.english4","russian2.english1","russian2.russian2","russian2.french3","russian2.english4","french3.english1","french3.russian2","french3.french3","french3.english4","english4.english1","english4.russian2","english4.french3","english4.english4"]
# for temp in (e1e1, e1r2, e1f3, e1e4, r2e1, r2r2, r2f3, r2e4, f3e1, f3r2, f3f3, f3e4, e4e1, e4r2, e4f3, e4e4):
#   temp_raw = pd.DataFrame(temp).stack().reset_index()
#   temp_raw.columns = ['x.num','y.num','value']

#   stories = names[(ii-1)].split(".")
#   temp_raw['x.story'] = stories[0]
#   temp_raw['y.story'] = stories[1]

#   full_similarity = pd.concat([full_similarity, temp_raw], axis = 0)
#   ii = ii + 1

# full_similarity.to_csv("karma.combined_similarity.csv", index = False)
# files.download("karma.combined_similarity.csv")

# reducer = umap.UMAP()
# full_embeds = pd.concat([pd.DataFrame(s1_embeds.numpy()), pd.DataFrame(s2_embeds.numpy()), pd.DataFrame(s3_embeds.numpy()), pd.DataFrame(s4_embeds.numpy())], ignore_index = True)

# embedding = reducer.fit_transform(full_embeds)
# index_col = np.repeat([1,2,3,4], repeats = [s1_embeds.shape[0], s2_embeds.shape[0], s3_embeds.shape[0], s4_embeds.shape[0]])

# umap_embeds = pd.DataFrame(embedding, columns = ["umap.x","umap.y"])
# umap_embeds["story"] = index_col

# umap_embeds.to_csv("karma.umap_embeds.csv", index = False)
# files.download("karma.umap_embeds.csv")

# """## Chunks"""

# whole_df = pd.DataFrame()

# ii = 1
# for story1 in range(1,5):
#   for chunk1 in range(1,6):
#     for story2 in range(1,5):
#       for chunk2 in range(1,6):
#         string_identifier = "chunk"+str(story1)+"."+str(chunk1) + "-chunk"+str(story2)+"."+str(chunk2)
#         print(string_identifier)
#         sA_story = clean_chunk.loc[(clean_chunk["story"] == story1) & (clean_chunk["chunk"] == chunk1)]
#         # print(sA_story)
#         sA_sentences = tf.constant(sA_story["sentences"])
#         sA_embeds = encoder(preprocessor(sA_sentences))["default"]
#         sA_embeds = normalization(sA_embeds)

#         sB_story = clean_chunk.loc[(clean_chunk["story"] == story2) & (clean_chunk["chunk"] == chunk2)]
#         # print(sB_story)
#         sB_sentences = tf.constant(sB_story["sentences"])
#         sB_embeds = encoder(preprocessor(sB_sentences))["default"]
#         sB_embeds = normalization(sB_embeds)

#         ab_matmul = np.matmul(sA_embeds, np.transpose(sB_embeds))
#         ab_df = pd.DataFrame(ab_matmul)

#         temp = ab_df.stack().reset_index()
#         temp.columns = ['x.num','y.num','value']
#         temp["x.num"] = temp["x.num"] + 1
#         temp["y.num"] = temp["y.num"] + 1
#         temp["x.story"] = story1
#         temp["x.chunk"] = chunk1
#         temp["y.story"] = story2
#         temp["y.chunk"] = chunk2
#         # print(temp)
#         whole_df = pd.concat([whole_df, temp], axis = 0)
#         print(ii/400)
#         ii = ii + 1

# whole_df.to_csv("chunk_full_matmult.csv", index = False)
# files.download("chunk_full_matmult.csv")

# """## Granular analysis"""



# """# Window analysis"""

# window_df = pd.DataFrame()
# umap_df = pd.DataFrame()

# reducer = umap.UMAP()

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

#   g1_embeds_numpy = pd.DataFrame(g1_embeds.numpy())
#   g1_embeds_numpy.to_csv("s1"+"_g"+str(window)+"_embeds_raw.csv", index = False)

#   g2_embeds_numpy = pd.DataFrame(g2_embeds.numpy())
#   g2_embeds_numpy.to_csv("s2"+"_g"+str(window)+"_embeds_raw.csv", index = False)

#   g3_embeds_numpy = pd.DataFrame(g3_embeds.numpy())
#   g3_embeds_numpy.to_csv("s3"+"_g"+str(window)+"_embeds_raw.csv", index = False)

#   g4_embeds_numpy = pd.DataFrame(g4_embeds.numpy())
#   g4_embeds_numpy.to_csv("s4"+"_g"+str(window)+"_embeds_raw.csv", index = False)

#   # Umap:

#   full_embeds = pd.concat([pd.DataFrame(g1_embeds.numpy()), pd.DataFrame(g2_embeds.numpy()), pd.DataFrame(g3_embeds.numpy()), pd.DataFrame(g4_embeds.numpy())], ignore_index = True)
#   embedding = reducer.fit_transform(full_embeds)
#   index_col = np.repeat([1,2,3,4], repeats = [g1_embeds.shape[0], g2_embeds.shape[0], g3_embeds.shape[0], g4_embeds.shape[0]])

#   umap_embeds = pd.DataFrame(embedding, columns = ["umap.x","umap.y"])
#   umap_embeds["story"] = index_col
#   umap_embeds["window"] = window

#   umap_df = pd.concat([umap_df, umap_embeds], axis = 0)


#   # Download the window files:

#   # files.download("s1"+"_g"+str(window)+"_embeds_raw.csv")
#   # files.download("s2"+"_g"+str(window)+"_embeds_raw.csv")
#   # files.download("s3"+"_g"+str(window)+"_embeds_raw.csv")
#   # files.download("s4"+"_g"+str(window)+"_embeds_raw.csv")

#   # Similarity part:

# #   e1e1_window = np.matmul(g1_embeds, np.transpose(g1_embeds))
# #   e1r2_window = np.matmul(g1_embeds, np.transpose(g2_embeds))
# #   e1f3_window = np.matmul(g1_embeds, np.transpose(g3_embeds))
# #   e1e4_window = np.matmul(g1_embeds, np.transpose(g4_embeds))

# #   r2e1_window = np.matmul(g2_embeds, np.transpose(g1_embeds))
# #   r2r2_window = np.matmul(g2_embeds, np.transpose(g2_embeds))
# #   r2f3_window = np.matmul(g2_embeds, np.transpose(g3_embeds))
# #   r2e4_window = np.matmul(g2_embeds, np.transpose(g4_embeds))

# #   f3e1_window = np.matmul(g3_embeds, np.transpose(g1_embeds))
# #   f3r2_window = np.matmul(g3_embeds, np.transpose(g2_embeds))
# #   f3f3_window = np.matmul(g3_embeds, np.transpose(g3_embeds))
# #   f3e4_window = np.matmul(g3_embeds, np.transpose(g4_embeds))

# #   e4e1_window = np.matmul(g4_embeds, np.transpose(g1_embeds))
# #   e4r2_window = np.matmul(g4_embeds, np.transpose(g2_embeds))
# #   e4f3_window = np.matmul(g4_embeds, np.transpose(g3_embeds))
# #   e4e4_window = np.matmul(g4_embeds, np.transpose(g4_embeds))

# #   ii = 1
# #   names = ["english1.english1","english1.russian2","english1.french3","english1.english4","russian2.english1","russian2.russian2","russian2.french3","russian2.english4","french3.english1","french3.russian2","french3.french3","french3.english4","english4.english1","english4.russian2","english4.french3","english4.english4"]
# #   for temp_mat in (e1e1_window, e1r2_window, e1f3_window, e1e4_window, r2e1_window, r2r2_window, r2f3_window, r2e4_window, f3e1_window, f3r2_window, f3f3_window, f3e4_window, e4e1_window, e4r2_window, e4f3_window, e4e4_window):
# #     temp = pd.DataFrame(temp_mat).stack().reset_index()
# #     temp.columns = ['x.num','y.num','value']

# #     stories = names[(ii-1)].split(".")

# #     temp['x.story'] = stories[0]
# #     temp['y.story'] = stories[1]
# #     temp['window'] = window

# #     window_df = pd.concat([window_df, temp], axis = 0)
# #     ii = ii + 1

# # window_df.to_csv("karma.windowed_similarity.csv", index = False)
# # files.download("karma.windowed_similarity.csv")
# umap_df.to_csv("karma.windowed_umap_embeds.csv", index = False)
# files.download("karma.windowed_umap_embeds.csv")

# """## Test plotting"""

# # Commented out IPython magic to ensure Python compatibility.
# # %config InlineBackend.figure_format='retina'
# plt.rcParams['figure.figsize'] = (20, 20)

# fig, axes = plt.subplots(4, 4)

# sns.heatmap(e1e1, ax = axes[0,0]).set(title = "Eng 1 vs Eng 1")
# sns.heatmap(e1r2, ax = axes[0,1]).set(title = "Eng 1 vs Rus 2")
# sns.heatmap(e1f3, ax = axes[0,2]).set(title = "Eng 1 vs Fre 3")
# sns.heatmap(e1e4, ax = axes[0,3]).set(title = "Eng 1 vs Eng 4")

# sns.heatmap(r2e1, ax = axes[1,0]).set(title = "Rus 2 vs Eng 1")
# sns.heatmap(r2r2, ax = axes[1,1]).set(title = "Rus 2 vs Rus 2")
# sns.heatmap(r2f3, ax = axes[1,2]).set(title = "Rus 2 vs Fre 3")
# sns.heatmap(r2e4, ax = axes[1,3]).set(title = "Rus 2 vs Eng 4")

# sns.heatmap(f3e1, ax = axes[2,0]).set(title = "Fre 3 vs Eng 1")
# sns.heatmap(f3r2, ax = axes[2,1]).set(title = "Fre 3 vs Rus 2")
# sns.heatmap(f3f3, ax = axes[2,2]).set(title = "Fre 3 vs Fre 3")
# sns.heatmap(f3e4, ax = axes[2,3]).set(title = "Fre 3 vs Eng 4")

# sns.heatmap(e4e1, ax = axes[3,0]).set(title = "Eng 4 vs Eng 1")
# sns.heatmap(e4r2, ax = axes[3,1]).set(title = "Eng 4 vs Rus 2")
# sns.heatmap(e4f3, ax = axes[3,2]).set(title = "Eng 4 vs Fre 3")
# sns.heatmap(e4e4, ax = axes[3,3]).set(title = "Eng 4 vs Eng 4")

# """## tSNE

# Not using.
# """

# plt.rcParams['figure.figsize'] = (12, 12)

# # https://scikit-learn.org/stable/modules/generated/sklearn.manifold.TSNE.html

# X_embedded = TSNE(n_components=2, learning_rate='auto', init='random', perplexity=3).fit_transform(s1_embeds)

# X_embedded.shape

# x_df = pd.DataFrame({"x" : X_embedded[:,0], "y" : X_embedded[:,1], "z" : range(0,165)})

# sns.lineplot(data = x_df, x = "x", y = "y")

# """## UMAP part

# Moved up.

# https://umap-learn.readthedocs.io/en/latest/basic_usage.html

# https://plotly.com/python/t-sne-and-umap-projections/
# """