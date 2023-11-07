library(tidyverse)
library(here)
setwd(here())
library(janitor)
library(lsa)
library(tsDyn)
library(dtw)
library(reshape2)

## What is this script for?
# Inputs: preprocessed / split sentences
#         MBERT embeddings
# Function: Vector auto-regression


# Load in the files ----
all.clean.split <- read_csv("../data/text_split/all.clean.split_V2.csv",
                            show_col_type = F)

all.chunk <- read_csv("../data/text_split/all.clean.split_VChunk.csv",
                            show_col_type = F)

eng1.clean.split <- all.chunk |> filter(story == 1)
rus2.clean.split <- all.chunk |> filter(story == 2)
fre3.clean.split <- all.chunk |> filter(story == 3)
eng4.clean.split <- all.chunk |> filter(story == 4)

## M-BERT in Colab ----

# https://colab.research.google.com/drive/1HIGBbRT0cJmpujjl7mhkSM-V_WTKgRxo

## Read in M-BERT embeddings ----

eng1.embeds <- read_csv("../data/embeddings/mbert/s1_embeds_raw.csv", show_col_type = F) |> 
  clean_names() |> 
  mutate(sent_num = 1:n()) 
rus2.embeds <- read_csv("../data/embeddings/mbert/s2_embeds_raw.csv", show_col_type = F)|> 
  clean_names() |> 
  mutate(sent_num = 1:n()) 
fre3.embeds <- read_csv("../data/embeddings/mbert/s3_embeds_raw.csv", show_col_type = F)|> 
  clean_names() |> 
  mutate(sent_num = 1:n()) 
eng4.embeds <- read_csv("../data/embeddings/mbert/s4_embeds_raw.csv", show_col_type = F)|> 
  clean_names() |> 
  mutate(sent_num = 1:n()) 

# Join with sentences.
eng1.clean.split <- eng1.clean.split |> 
  mutate(sent_num = 1:n()) |> 
  left_join(eng1.embeds, by = "sent_num")

rus2.clean.split <- rus2.clean.split |> 
  mutate(sent_num = 1:n()) |> 
  left_join(rus2.embeds, by = "sent_num")

fre3.clean.split <- fre3.clean.split |> 
  mutate(sent_num = 1:n()) |> 
  left_join(fre3.embeds, by = "sent_num")

eng4.clean.split <- eng4.clean.split |> 
  mutate(sent_num = 1:n()) |> 
  left_join(eng4.embeds, by = "sent_num")

all.clean.split_wEmb <- rbind(eng1.clean.split, 
                            rus2.clean.split, 
                            fre3.clean.split,
                            eng4.clean.split)


# Momentum ----

# Compare the change vectors between sentence embeddings
## Example
# Sentences a, b, c
# change vector between a and b
# cos(ab, bc) = cos(b - a, c - b)
# Cosine as 1 through -1, cos(theta) = 1, continuation, 0 as unexpected, -1 as reminiscing
# Reduce each story to a vector of momentum time series

# DTW on the different momentum time series

# Surprisal as abstracting away from the coordinates, quantifying the shape

momentum <- function(original_df){
  diff.df <- diff(as.matrix(original_df |> 
                           select(starts_with("x"))), 
               lag = 1) |> 
    as.data.frame()
  
  story = unique(original_df$story)
  lang = unique(original_df$lang)
  
  temp.vec <- c()
    
    for (ii in 1:(nrow(diff.df) - 1) ){
      print(c(ii, ii+1))
      
      ab <- unlist(diff.df[ii,])
      
      bc <- unlist(diff.df[ii+1,])
      
      temp.vec <- append(temp.vec, cosine(ab, bc))
    }
    
    momentum.df <- data.frame(story = story, lang = lang,
                              momentum = temp.vec) |> 
      mutate(diff_num = 1:n(), diff_prop = diff_num/n())
    
    return(momentum.df)
}

eng1.momentum <- momentum(eng1.clean.split)
rus2.momentum <- momentum(rus2.clean.split)
fre3.momentum <- momentum(fre3.clean.split)
eng4.momentum <- momentum(eng4.clean.split)

momentum.joined <- rbind(eng1.momentum, rus2.momentum, 
                         fre3.momentum, eng4.momentum)

ggplot(momentum.joined,
       aes(x = diff_prop, y = momentum, 
           color = factor(story)))+
  geom_line()+
  theme_bw(base_size = 14)+
  scale_color_brewer(type = "qual", palette = 2, name = "Story")+
  xlab("Story proportion")+
  ylab("Momentum")

ggsave(path = "../figures/",
       filename = "momentum.png",units = "in", 
       width = 30, height = 5, dpi = 300)


# Means ----

rbind(
  eng1.clean.split |> 
    mutate(momentum = c(NA, eng1.momentum$momentum, NA)),
  rus2.clean.split |> 
    mutate(momentum = c(NA, rus2.momentum$momentum, NA)),
  fre3.clean.split |> 
    mutate(momentum = c(NA, fre3.momentum$momentum, NA)),
  eng4.clean.split |> 
    mutate(momentum = c(NA, eng4.momentum$momentum, NA))
) |> 
  group_by(story, chunk) |> 
  summarize(mean.momentum = mean(momentum, na.rm = T),
            sd = sd(momentum, na.rm = T)) |> 
  ggplot(aes(x = chunk, y = mean.momentum, color = factor(story), shape = factor(story)))+
  geom_point(size = 4, 
             position = position_dodge(width = 0.2))+
  geom_errorbar(aes(ymin = mean.momentum - sd, 
                    ymax = mean.momentum + sd), 
                color = "black",
                width = 0.2, 
                position = position_dodge(width = 0.2))+
  theme_bw(base_size = 18)+
  scale_color_brewer(name = "Story", type = "qual", palette = 2)+
  scale_shape_manual(name = "Story", values = c(15, 16, 17, 18))+
  labs(caption = "Bars represent SD")

ggsave(path = "../figures/",
       filename = "momentum-by-chunk.png",units = "in", 
       width = 8, height = 5, dpi = 300)

# DTW ----

## ENG1, RUS2 ----
eng1.rus2 <- dtw(momentum.joined |> filter(story == 1) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 2) |> 
                   select(momentum),
                 keep = TRUE)
eng1.rus2.distance <- eng1.rus2$normalizedDistance

## ENG1, FRE3 ----
eng1.fre3 <- dtw(momentum.joined |> filter(story == 1) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 3) |> 
                   select(momentum),
                 keep = TRUE)
eng1.fre3.distance <- eng1.fre3$normalizedDistance

## ENG1, ENG4 ----
eng1.eng4 <- dtw(momentum.joined |> filter(story == 1) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 4) |> 
                   select(momentum),
                 keep = TRUE)
eng1.eng4.distance <- eng1.eng4$normalizedDistance

## RUS2, ENG1 ----

rus2.eng1 <- dtw(momentum.joined |> filter(story == 2) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 1) |> 
                   select(momentum),
                 keep = TRUE)
rus2.eng1.distance <- rus2.eng1$normalizedDistance

## RUS2, FRE3 ----

rus2.fre3 <- dtw(momentum.joined |> filter(story == 2) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 3) |> 
                   select(momentum),
                 keep = TRUE)
rus2.fre3.distance <- rus2.fre3$normalizedDistance

## RUS2, ENG4 ----

rus2.eng4 <- dtw(momentum.joined |> filter(story == 2) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 4) |> 
                   select(momentum),
                 keep = TRUE)
rus2.eng4.distance <- rus2.eng4$normalizedDistance

## FRE3, ENG1 ----
fre3.eng1 <- dtw(momentum.joined |> filter(story == 3) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 1) |> 
                   select(momentum),
                 keep = TRUE)
fre3.eng1.distance <- fre3.eng1$normalizedDistance

## FRE3, RUS2 ----
fre3.rus2 <- dtw(momentum.joined |> filter(story == 3) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 2) |> 
                   select(momentum),
                 keep = TRUE)
fre3.rus2.distance <- fre3.rus2$normalizedDistance

## FRE3, ENG4 ----
fre3.eng4 <- dtw(momentum.joined |> filter(story == 3) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 4) |> 
                   select(momentum),
                 keep = TRUE)
fre3.eng4.distance <- fre3.eng4$normalizedDistance

## ENG4, RUS2 ----
eng4.rus2 <- dtw(momentum.joined |> filter(story == 4) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 2) |> 
                   select(momentum),
                 keep = TRUE)
eng4.rus2.distance <- eng4.rus2$normalizedDistance

## ENG4, FRE3 ----
eng4.fre3 <- dtw(momentum.joined |> filter(story == 4) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 3) |> 
                   select(momentum),
                 keep = TRUE)
eng4.fre3.distance <- eng4.fre3$normalizedDistance

## ENG4, ENG1 ----
eng4.eng1 <- dtw(momentum.joined |> filter(story == 4) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 1) |> 
                   select(momentum),
                 keep = TRUE)
eng4.eng1.distance <- eng4.eng1$normalizedDistance

eng1.eng1 <- dtw(momentum.joined |> filter(story == 1) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 1) |> 
                   select(momentum),
                 keep = TRUE)
eng1.eng1.distance <- eng1.eng1$normalizedDistance

rus2.rus2 <- dtw(momentum.joined |> filter(story == 2) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 2) |> 
                   select(momentum),
                 keep = TRUE)
rus2.rus2.distance <- rus2.rus2$normalizedDistance

fre3.fre3 <- dtw(momentum.joined |> filter(story == 3) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 3) |> 
                   select(momentum),
                 keep = TRUE)
fre3.fre3.distance <- fre3.fre3$normalizedDistance

eng4.eng4 <- dtw(momentum.joined |> filter(story == 4) |> 
                   select(momentum),
                 momentum.joined |> filter(story == 4) |> 
                   select(momentum),
                 keep = TRUE)
eng4.eng4.distance <- eng4.eng4$normalizedDistance

dtw.df <- data.frame(story = c("English 1", "Russian 2","French 3", "English 4"),
                     `English 1` = c(eng1.eng1.distance, rus2.eng1.distance,
                                     fre3.eng1.distance, eng4.eng1.distance),
                     `Russian 2` = c(eng1.rus2.distance, rus2.rus2.distance,
                                     fre3.rus2.distance, eng4.rus2.distance),
                     `French 3` = c(eng1.fre3.distance, rus2.fre3.distance,
                                    fre3.fre3.distance, eng4.fre3.distance),
                     `English 4` = c(eng1.eng4.distance, rus2.eng4.distance,
                                     fre3.eng4.distance, eng4.eng4.distance)) |> 
  melt()









dtw.df |> 
  rename(x.story = story,
         y.story = variable,
         distance = value) |> 
  mutate(y.story = gsub("\\."," ", y.story)) |> 
  mutate(x.story = factor(x.story,
                          levels = c("English 1","Russian 2", 
                                     "French 3","English 4")),
         y.story = factor(y.story,
                          levels = c("English 1","Russian 2", 
                                     "French 3","English 4"))) |> 
  ggplot(aes(x = x.story, y = y.story, fill = distance))+
  geom_tile()+
  theme_bw(base_size = 14)+
  scale_fill_distiller(type = "seq", palette = 2, name = "Momentum\nDistance")+
  xlab("Story A")+
  ylab("Story B")+
  geom_label(aes(label = round(distance, digits = 4)), fill = "white")

ggsave(path = "../figures/",
       filename = "momentum_dtw.png",units = "in", 
       width = 10, height = 7, dpi = 600)
  

# 
# 
# 
# ## Surprisal analysis ----
# # Predict next-sentence vector based on ‘momentum’ from previous four sentences
# # Cosine similarity between predicted embedding and the real embedding
# # Correlate the "surprise" of a sentence in Story A with the surprise of the paired sentence in Story B — this captures how well the sentence-by-sentence surprise of Story A compares to Story B.
# # Weighted sum of preceding four sentence vectors
# # Sn + 0.7 vec(Sn-1 Sn) + 0.2 vec(Sn-2 Sn) + 0.05 vec(Sn-3 Sn) + 0.05 vec(Sn-4 Sn)
# # Vector auto-regressive function of order N, to predict the next embedding
# 
# # Example:
# get_vector <- function(matrix, row.n){
#   return(matrix[row.n,] |> as.vector())
# }
# 
# var.basic <- function(sn, sn1, sn2, sn3, sn4, weight.vector){
#   sm <- (weight.vector[1]*sn) + 
#     (weight.vector[2]*(sn1 - sn)) + 
#     (weight.vector[3]*(sn2 - sn)) + 
#     (weight.vector[4]*(sn3 - sn)) + 
#     (weight.vector[5]*(sn4 - sn))
#   
#   return(sm)
# }
# 
# var.basic.alt <- function(sn, sn1, sn2, sn3, sn4, weight.vector){
#   sm <- (weight.vector[1]*sn) + 
#     (weight.vector[2]*(sn1)) + 
#     (weight.vector[3]*(sn2)) + 
#     (weight.vector[4]*(sn3)) + 
#     (weight.vector[5]*(sn4))
#   
#   return(sm)
# }
# 
# var.auto <- function(embeddings){
#   surprisals <- vector()
#   momentum <- vector()
#   embedding_matrix <- embeddings |> 
#     data.matrix()
#   
#   weights = c(1, 0.7, 0.2, 0.05, 0.05)
#   
#   for (n in 5:(nrow(embedding_matrix) - 1)){
#     print(n)
#     # sentence n
#     s.n <- get_vector(embedding_matrix, n)
#     s.n1 <- get_vector(embedding_matrix, (n-1))
#     s.n2 <- get_vector(embedding_matrix, (n-2))
#     s.n3 <- get_vector(embedding_matrix, (n-3))
#     s.n4 <- get_vector(embedding_matrix, (n-4))
#     
#     s.m <- var.basic(s.n, s.n1, s.n2, s.n3, s.n4, weights)
#     
#     s.m_true <- get_vector(embedding_matrix, n + 1)
#     
#     sim <- cosine(s.m, s.m_true)
#     # print(sim)
#     surprisals <- append(sim, surprisals)
#     
#     m.diff <- cosine((s.m - s.n), (s.m_true - s.n))
#     momentum <- append(m.diff, momentum)
#   }
#   
#   
#   return(data.frame(prop.index = (1:length(surprisals))/length(surprisals),
#              surprisals = surprisals, 
#              momentums = momentum))
# }
# 
# eng1.surprisals <- var.auto(eng1.embeds) |> 
#   mutate(story = "eng1")
# 
# rus2.surprisals <- var.auto(rus2.embeds) |> 
#   mutate(story = "rus2")
# 
# fre3.surprisals <- var.auto(fre3.embeds) |> 
#   mutate(story = "fre3")
# 
# eng4.surprisals <- var.auto(eng4.embeds) |> 
#   mutate(story = "eng4")
# 
# full.surprisals <- rbind(eng1.surprisals,
#                          rus2.surprisals,
#                          fre3.surprisals,
#                          eng4.surprisals)
# 
# full.surprisals |> 
#   ggplot(aes(x = prop.index, y = surprisals))+
#   geom_line()+
#   theme_bw()+
#   ylab("Surprisals\ncos(predicted, actual)")+
#   xlab("Index (proportional)")+
#   facet_wrap(~story, scales = "free")
# 
# full.surprisals |> 
#   ggplot(aes(x = prop.index, y = momentums))+
#   geom_line()+
#   theme_bw()+
#   ylab("Momentum\ncos(s.m_true - s.n, s.m_pred - s.n)")+
#   xlab("Index (proportional)")+
#   facet_wrap(~story, scales = "free")
# 
# ## Manual test:
# surprisals <- vector()
# embedding_matrix <- eng1.embeds |> data.matrix()
# 
# weights = c(1, 0.7, 0.2, 0.05, 0.05)
# 
# for (n in 5:(nrow(embedding_matrix) - 1)){
#   print(n)
#   # sentence n
#   s.n <- get_vector(embedding_matrix, n)
#   # sentence n-1
#   s.n1 <- get_vector(embedding_matrix, n-1)
#   # sentence n-2
#   s.n2 <- get_vector(embedding_matrix, n-2)
#   # sentence n-3
#   s.n3 <- get_vector(embedding_matrix, n-3)
#   # sentence n-4
#   s.n4 <- get_vector(embedding_matrix, n-4)
#   
#   s.m <- var.basic(s.n, s.n1, s.n2, s.n3, s.n4, weights)
#   
#   s.m_true <- get_vector(embedding_matrix, n + 1)
#   
#   sim <- cosine(s.m, s.m_true)
#   print(sim)
#   surprisals <- append(sim, surprisals)
# }
#   
# 
# embedding_matrix <- eng1.embeds |> data.matrix()
# n = 6
# weights = c(1, 0.7, 0.2, 0.05, 0.05)
# 
# # sentence n
# s.n <- get_vector(embedding_matrix, n)
# # sentence n-1
# s.n1 <- get_vector(embedding_matrix, n-1)
# # sentence n-2
# s.n2 <- get_vector(embedding_matrix, n-2)
# # sentence n-3
# s.n3 <- get_vector(embedding_matrix, n-3)
# # sentence n-4
# s.n4 <- get_vector(embedding_matrix, n-4)
# 
# s.m <- var.basic(s.n, s.n1, s.n2, s.n3, s.n4, weights)
# 
# s.m_true <- get_vector(embedding_matrix, n + 1)
# 
# cosine(s.m, s.m_true)
