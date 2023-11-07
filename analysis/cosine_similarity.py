import numpy as np
from numpy.linalg import norm
import pandas as pd

def cosine_similarity(a, b):
    return np.dot(a,b) / (norm(a) * norm(b))


eng1 = pd.read_csv("../data/embeddings/mbert/s1_embeds_raw.csv")
rus2 = pd.read_csv("../data/embeddings/mbert/s2_embeds_raw.csv")
fre3 = pd.read_csv("../data/embeddings/mbert/s3_embeds_raw.csv")
eng4 = pd.read_csv("../data/embeddings/mbert/s4_embeds_raw.csv")

print(cosine_similarity(eng1.loc[1,:], eng1.loc[2,:]))

# d(S_i, lag = k, memory span = n) = || S_i, {S_(i - n - k), ..., S_(i-k)} ||
def stagger_lagger(sentence_index, lag = 1, span = 3, vector_set = pd.DataFrame()):
    ## Get sentence
    sentence_n = vector_set.loc[sentence_index, : ].to_numpy()
    # print(sentence_n)

    ## Average preceding vectors

    ## Going back 1 through 'span' sentences that are 'lag' back
    # || S_i, {S_(i - span - lag), ..., S_(i-lag)} ||
    indA = sentence_index - span - lag + 1
    indB = sentence_index - lag

    # With this implementation, it can handle when there are less than span sentences lag sentences ago
    preceding_vectors = vector_set.loc[indA:indB,:].to_numpy()
    # print(len(preceding_vectors[1]))

    avg_vector = np.mean(preceding_vectors, axis=0)
    # print(len(avg_vector))

    return cosine_similarity(sentence_n, avg_vector)

# Sentence number, story, lang, lag, span, similarity

temp = []
spanA = 3
lagB = 3

print(range(1 + spanA + lagB, eng1.shape[0] - 1))

# for i in range(1 + spanX + lagX, )

# print(stagger_lagger(7, 3, 3, eng1))
# print(stagger_lagger(156, 3, 3, eng1))
# print(stagger_lagger(3, 3, 3, eng1))

  
# lag.trajectory <- function(span.x = 3, lag.x = 3, embed.df, story, lang){
#   temp <- c()
  
#   for (i in (1 + span.x + lag.x):nrow(embed.df)){
#     temp <- append(temp, stagger.lagger(i, lag.x, span.x, embed.df))
#   }
  
#   return(data.frame(similarity = c(rep(NA, length.out = (span.x + lag.x)),
#                                    temp)) |>
#            mutate(sent_num = 1:n(),
#                   story = story,
#                   lang = lang,
#                   lag = lag.x,
#                   span = span.x))
  
# }




# # Memory scope ----
# # General idea: connection between a single sentence and a set of preceding sentences
# # Try to find the distance between sentence x, and the average of x-1, x-2, x-3, etc.




# lag.trajectory <- function(span.x = 3, lag.x = 3, embed.df, story, lang){
#   temp <- c()
  
#   for (i in (1 + span.x + lag.x):nrow(embed.df)){
#     temp <- append(temp, stagger.lagger(i, lag.x, span.x, embed.df))
#   }
  
#   return(data.frame(similarity = c(rep(NA, length.out = (span.x + lag.x)),
#                                    temp)) |>
#            mutate(sent_num = 1:n(),
#                   story = story,
#                   lang = lang,
#                   lag = lag.x,
#                   span = span.x))
  
# }

# lag.df <- data.frame()

# for (n in 1:10){
#   for (j in 1:10){
#     lag.n <- lag.trajectory(n, j, eng1.embeds, 1, "eng")
#     lag.df <- lag.df |> 
#       rbind(lag.n)
    
#     lag.n <- lag.trajectory(n, j, rus2.embeds, 2, "rus")
#     lag.df <- lag.df |> 
#       rbind(lag.n)
    
#     lag.n <- lag.trajectory(n, j, fre3.embeds, 3, "fre")
#     lag.df <- lag.df |> 
#       rbind(lag.n)
    
#     lag.n <- lag.trajectory(n, j, eng4.embeds, 4, "eng")
#     lag.df <- lag.df |> 
#       rbind(lag.n)
#   }
# }