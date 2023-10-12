library(tidyverse)
library(here)
setwd(here())
library(janitor)
library(vars)
library(lsa)

## What is this script for?
# Inputs: preprocessed / split sentences
#         MBERT embeddings
# Function: Vector auto-regression


## Load in the files ----
all.clean.split <- read_csv("../data/text_split/all.clean.split_V2.csv",
                            show_col_type = F)

eng1.clean.split <- all.clean.split |> filter(story == 1)
rus2.clean.split <- all.clean.split |> filter(story == 2)
fre3.clean.split <- all.clean.split |> filter(story == 3)
eng4.clean.split <- all.clean.split |> filter(story == 4)

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


## Trying the VAR package ----
nhor <- 12

df.lev <- eng1.clean.split |> 
  dplyr::select(starts_with("x"))
m.lev <-  (as.matrix(df.lev))
nr_lev <- nrow(df.lev)

VARselect(df.lev, type = "both")

library(urca)
data(denmark)
df.lev <- denmark[,c("LRM","LRY","IBO","IDE")]
m.lev  <- as.matrix(df.lev)
nr_lev <- nrow(df.lev)

VARselect(df.lev, type = "both")

var.model_lev <- VAR(df.lev, p = 2, type = "both")


## Surprisal analysis ----
# Predict next-sentence vector based on ‘momentum’ from previous four sentences
# Cosine similarity between predicted embedding and the real embedding
# Correlate the "surprise" of a sentence in Story A with the surprise of the paired sentence in Story B — this captures how well the sentence-by-sentence surprise of Story A compares to Story B.
# Weighted sum of preceding four sentence vectors
# Sn + 0.7 vec(Sn-1 Sn) + 0.2 vec(Sn-2 Sn) + 0.05 vec(Sn-3 Sn) + 0.05 vec(Sn-4 Sn)
# Vector auto-regressive function of order N, to predict the next embedding

# Example:
get_vector <- function(matrix, row.n){
  return(matrix[row.n,] |> as.vector())
}

var.basic <- function(sn, sn1, sn2, sn3, sn4, weight.vector){
  sm <- (weight.vector[1]*sn) + 
    (weight.vector[2]*(sn1 - sn)) + 
    (weight.vector[3]*(sn2 - sn)) + 
    (weight.vector[4]*(sn3 - sn)) + 
    (weight.vector[5]*(sn4 - sn))
  
  return(sm)
}

var.basic.alt <- function(sn, sn1, sn2, sn3, sn4, weight.vector){
  sm <- (weight.vector[1]*sn) + 
    (weight.vector[2]*(sn1)) + 
    (weight.vector[3]*(sn2)) + 
    (weight.vector[4]*(sn3)) + 
    (weight.vector[5]*(sn4))
  
  return(sm)
}

var.auto <- function(embeddings){
  surprisals <- vector()
  momentum <- vector()
  embedding_matrix <- embeddings |> 
    data.matrix()
  
  weights = c(1, 0.7, 0.2, 0.05, 0.05)
  
  for (n in 5:(nrow(embedding_matrix) - 1)){
    print(n)
    # sentence n
    s.n <- get_vector(embedding_matrix, n)
    s.n1 <- get_vector(embedding_matrix, (n-1))
    s.n2 <- get_vector(embedding_matrix, (n-2))
    s.n3 <- get_vector(embedding_matrix, (n-3))
    s.n4 <- get_vector(embedding_matrix, (n-4))
    
    s.m <- var.basic(s.n, s.n1, s.n2, s.n3, s.n4, weights)
    
    s.m_true <- get_vector(embedding_matrix, n + 1)
    
    sim <- cosine(s.m, s.m_true)
    # print(sim)
    surprisals <- append(sim, surprisals)
    
    m.diff <- cosine((s.m - s.n), (s.m_true - s.n))
    momentum <- append(m.diff, momentum)
  }
  
  
  return(data.frame(prop.index = (1:length(surprisals))/length(surprisals),
             surprisals = surprisals, 
             momentums = momentum))
}

eng1.surprisals <- var.auto(eng1.embeds) |> 
  mutate(story = "eng1")

rus2.surprisals <- var.auto(rus2.embeds) |> 
  mutate(story = "rus2")

fre3.surprisals <- var.auto(fre3.embeds) |> 
  mutate(story = "fre3")

eng4.surprisals <- var.auto(eng4.embeds) |> 
  mutate(story = "eng4")

full.surprisals <- rbind(eng1.surprisals,
                         rus2.surprisals,
                         fre3.surprisals,
                         eng4.surprisals)

full.surprisals |> 
  ggplot(aes(x = prop.index, y = surprisals))+
  geom_line()+
  theme_bw()+
  ylab("Surprisals\ncos(predicted, actual)")+
  xlab("Index (proportional)")+
  facet_wrap(~story, scales = "free")

full.surprisals |> 
  ggplot(aes(x = prop.index, y = momentums))+
  geom_line()+
  theme_bw()+
  ylab("Momentum\ncos(s.m_true - s.n, s.m_pred - s.n)")+
  xlab("Index (proportional)")+
  facet_wrap(~story, scales = "free")

## Manual test:
surprisals <- vector()
embedding_matrix <- eng1.embeds |> data.matrix()

weights = c(1, 0.7, 0.2, 0.05, 0.05)

for (n in 5:(nrow(embedding_matrix) - 1)){
  print(n)
  # sentence n
  s.n <- get_vector(embedding_matrix, n)
  # sentence n-1
  s.n1 <- get_vector(embedding_matrix, n-1)
  # sentence n-2
  s.n2 <- get_vector(embedding_matrix, n-2)
  # sentence n-3
  s.n3 <- get_vector(embedding_matrix, n-3)
  # sentence n-4
  s.n4 <- get_vector(embedding_matrix, n-4)
  
  s.m <- var.basic(s.n, s.n1, s.n2, s.n3, s.n4, weights)
  
  s.m_true <- get_vector(embedding_matrix, n + 1)
  
  sim <- cosine(s.m, s.m_true)
  print(sim)
  surprisals <- append(sim, surprisals)
}
  

embedding_matrix <- eng1.embeds |> data.matrix()
n = 6
weights = c(1, 0.7, 0.2, 0.05, 0.05)

# sentence n
s.n <- get_vector(embedding_matrix, n)
# sentence n-1
s.n1 <- get_vector(embedding_matrix, n-1)
# sentence n-2
s.n2 <- get_vector(embedding_matrix, n-2)
# sentence n-3
s.n3 <- get_vector(embedding_matrix, n-3)
# sentence n-4
s.n4 <- get_vector(embedding_matrix, n-4)

s.m <- var.basic(s.n, s.n1, s.n2, s.n3, s.n4, weights)

s.m_true <- get_vector(embedding_matrix, n + 1)

cosine(s.m, s.m_true)
