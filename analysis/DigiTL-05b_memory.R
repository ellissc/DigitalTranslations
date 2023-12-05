library(tidyverse)
library(here)
setwd(here())
library(janitor)
library(lsa)
library(tsDyn)
library(dtw)
library(reshape2)
library(ggdist)
library(broom)
# library(reticulate)
# 
# use_condaenv("linc")
# source_python(here("cosine_similarity.py"))


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
  clean_names() 

rus2.embeds <- read_csv("../data/embeddings/mbert/s2_embeds_raw.csv", show_col_type = F)|> 
  clean_names() 

fre3.embeds <- read_csv("../data/embeddings/mbert/s3_embeds_raw.csv", show_col_type = F)|> 
  clean_names() 

eng4.embeds <- read_csv("../data/embeddings/mbert/s4_embeds_raw.csv", show_col_type = F)|> 
  clean_names() 

# Join with sentences.
# eng1.clean.split <- eng1.clean.split |> 
#   mutate(sent_num = 1:n()) |> 
#   left_join(eng1.embeds, by = "sent_num")
# 
# rus2.clean.split <- rus2.clean.split |> 
#   mutate(sent_num = 1:n()) |> 
#   left_join(rus2.embeds, by = "sent_num")
# 
# fre3.clean.split <- fre3.clean.split |> 
#   mutate(sent_num = 1:n()) |> 
#   left_join(fre3.embeds, by = "sent_num")
# 
# eng4.clean.split <- eng4.clean.split |> 
#   mutate(sent_num = 1:n()) |> 
#   left_join(eng4.embeds, by = "sent_num")

# all.clean.split_wEmb <- rbind(eng1.clean.split, 
#                             rus2.clean.split, 
#                             fre3.clean.split,
#                             eng4.clean.split)

# Memory scope ----
# General idea: connection between a single sentence and a set of preceding sentences
# Try to find the distance between sentence x, and the average of x-1, x-2, x-3, etc.

# lag.sim <- function(sentence_index, lag = 3, df){
#   ## Get sentence
#   sentence.n = as.vector(unlist(df[sentence_index,]))
#   
#   ## Average preceding vectors
#   avg.vector = rep(0, length.out = length(sentence.n))
#   
#   # Looping between 1 back to 'lag' back sentences
#   for (i in 1:lag){
#     sentence.tmp = as.vector(unlist(df[sentence_index - i,]))
#     avg.vector = avg.vector + sentence.tmp
#   }
#   
#   avg.vector = avg.vector / lag
# 
#   # Calculating similarity
#   similarity <- cosine(sentence.n, avg.vector)
#   
#   # Euclidean distance:
#   # similarity <- sqrt(sum((sentence.n - avg.vector)^2 ) )
#   
#   return(similarity)
# }

# d(S_i, lag = k, memory span = n) = || S_i, {S_(i - n - k), ..., S_(i-k)} ||
stagger.lagger <- function(sentence_index, lag = 3, span = 3, df){
  ## Get sentence
  sentence.n = as.vector(unlist(df[sentence_index,]))
  
  ## Average preceding vectors
  avg.vector = rep(0, length.out = length(sentence.n))
  
  # Going back 1 through 'span' sentences, that are 'lag' back
  for (i in 1:span){
    new.index = sentence_index - i - lag + 1
    sentence.tmp = as.vector(unlist(df[new.index,]))
    avg.vector = avg.vector + sentence.tmp
  }
  
  avg.vector = avg.vector / span
  
  # Calculating similarity
  similarity <- cosine(sentence.n, avg.vector)[1,1]
  
  # Euclidean distance:
  # similarity <- sqrt(sum((sentence.n - avg.vector)^2 ) )
  
  return(similarity)
}

index.test <- function(sentence_index, lag = 3, span = 3){
  print(paste("Starting with sentence", sentence_index))
  
  # Going back 1 through 'span' sentences, that are 'lag' back
  for (i in 1:span){
    print(paste("Checking sentence", (sentence_index - i - lag + 1)))
  }
}

index.test(93, 1, 3)


lag.trajectory <- function(span.x = 3, lag.x = 3, embed.df, story, lang){
  temp <- c()
  
  for (i in (1 + span.x + lag.x):nrow(embed.df)){
    temp <- append(temp, stagger.lagger(i, lag.x, span.x, embed.df))
  }
  
  return(data.frame(similarity = c(rep(NA, length.out = (span.x + lag.x)),
                                   temp)) |>
           mutate(sent_num = 1:n(),
                  story = story,
                  lang = lang,
                  lag = lag.x,
                  span = span.x))
  
}

lag.df <- data.frame()

for (span.n in 1:60){
  for (lag.j in 1:1){
    lag.n <- lag.trajectory(span.n, lag.j, eng1.embeds, 1, "eng")
    lag.df <- lag.df |> 
      rbind(lag.n)
    
    lag.n <- lag.trajectory(span.n, lag.j, rus2.embeds, 2, "rus")
    lag.df <- lag.df |> 
      rbind(lag.n)
    
    lag.n <- lag.trajectory(span.n, lag.j, fre3.embeds, 3, "fre")
    lag.df <- lag.df |> 
      rbind(lag.n)
    
    lag.n <- lag.trajectory(span.n, lag.j, eng4.embeds, 4, "eng")
    lag.df <- lag.df |> 
      rbind(lag.n)
  }
}

write_csv(lag.df, here("../data/processed_data/memory_span_analysis.csv"))


# Using pregen data ----

lag.df <- read_csv(here("../data/processed_data/memory_span_analysis.csv"))

ggplot(lag.df |> drop_na() |> 
         filter(lag == 1), 
       aes(x = sent_num, y = similarity, 
           color = as.factor(story)))+
  geom_line(alpha = 0.5)+
  theme_bw()+
  xlab("Sentence number")+
  ylab("Cosine similarity")+
  scale_color_brewer(name = "Story", type = "qual", palette = 2,
                     direction = -1)+
  # scale_linetype_manual(values = c("solid","dashed",
  #                                  "dotted","dotdash"),
  #                       name = "Story")+
  # scale_shape_manual(name = "Lag", values = c(1,2,3,4))+
  ggtitle("Story memory", subtitle = "lag == 1")+
  facet_wrap(~span, scales = "free")+
  geom_smooth(method = "lm")

ggsave(path = "../figures/",
       filename = "memory_scope.png",units = "in", 
       width = 18, height = 12, dpi = 300)


ggplot(lag.df |> drop_na() |> 
         filter(span <= 7 & lag <= 7), 
       aes(x = sent_num, y = similarity, 
           color = as.factor(story)))+
  geom_line(alpha = 0.25)+
  theme_bw()+
  xlab("Sentence number")+
  ylab("Cosine similarity")+
  scale_color_brewer(name = "Story", type = "qual", palette = 2,
                     direction = -1)+
  # scale_linetype_manual(values = c("solid","dashed",
  #                                  "dotted","dotdash"),
  #                       name = "Story")+
  # scale_shape_manual(name = "Lag", values = c(1,2,3,4))+
  ggtitle("Story memory", subtitle = "lag ~ span")+
  facet_grid(lag~span)+
  geom_smooth(method = "lm")

ggsave(path = "../figures/",
       filename = "memory_scope_miniplot.png",units = "in", 
       width = 18, height = 12, dpi = 300)


## Similarity distribution ----

ggplot(lag.df |> drop_na(), 
       aes(x = similarity, 
           fill = as.factor(story)))+
  geom_density(alpha = 0.75)+
  theme_bw()+
  xlab("Cosine similarity")+
  scale_fill_brewer(name = "Story", type = "qual", palette = 2,
                     direction = -1)+
  ggtitle("Story memory", subtitle = "lag ~ span")+
  facet_grid(lag~span)

ggsave(path = "../figures/",
       filename = "memory_scope_similarity_distribution.png",units = "in", 
       width = 22, height = 12, dpi = 300)


## Slope between lag v cosine similarity ----

ggplot(lag.df |> drop_na() |> 
         filter(sent_num <= 50), 
       aes(x = lag, y = similarity, 
           color = as.factor(story)))+
  # geom_line(alpha = 0.25)+
  geom_point(alpha = 0.025)+
  theme_bw()+
  xlab("lag")+
  ylab("Cosine similarity")+
  scale_color_brewer(name = "Story", type = "qual", palette = 2,
                     direction = -1)+
  ggtitle("Story memory", subtitle = "faceted by span")+
  facet_wrap(~span, scales = "free")+
  geom_smooth(method = "lm")

ggsave(path = "../figures/",
       filename = "memory_scope_by_lag_first50.png",units = "in", 
       width = 18, height = 12, dpi = 300)



ggplot(lag.df |> drop_na(), 
       aes(x = lag, y = similarity, 
           color = as.factor(story)))+
  # geom_line(alpha = 0.25)+
  geom_point(alpha = 0.025)+
  theme_bw()+
  xlab("lag")+
  ylab("Cosine similarity")+
  scale_color_brewer(name = "Story", type = "qual", palette = 2,
                     direction = -1)+
  ggtitle("Story memory", subtitle = "faceted by span")+
  facet_wrap(~span, scales = "free")+
  geom_smooth(method = "lm")

ggsave(path = "../figures/",
       filename = "memory_scope_by_lag_all.png",units = "in", 
       width = 18, height = 12, dpi = 300)


## By-sentence regressions? ----

ggplot(lag.df |> drop_na() |> 
         filter(sent_num <= 50 & lag == 1), 
       aes(x = span, y = similarity))+
  # geom_line(alpha = 0.25)+
  geom_point()+
  theme_bw()+
  xlab("lag")+
  ylab("Cosine similarity")+
  scale_color_brewer(name = "Story", type = "qual", palette = 2,
                     direction = -1)+
  ggtitle("Story memory", subtitle = "faceted by story")+
  geom_smooth(aes(group = sent_num), se = F)+
  facet_wrap(~story)

## Calculation ----

calc.df <- lag.df |> drop_na() |> 
  filter(sent_num <= 50 & span == 6) |> 
  group_by(sent_num, story) |>
  group_modify(~ broom::tidy(lm(similarity ~ lag, data = .x))) |> 
  drop_na() |> 
  select(-c(std.error, statistic, p.value)) |> 
  pivot_wider(values_from = "estimate",
              names_from = "term") |> 
  rename(intercept = `(Intercept)`) |> 
  ungroup()

calc.df |> 
  ggplot(aes(x = sent_num, y = lag))+
  geom_point()+
  facet_wrap(~story)+
  ylab("Lag slope")+
  theme_bw(base_size = 18)
  



## DTW part ----

lag.df |> 
  drop_na() |> 
  head()

