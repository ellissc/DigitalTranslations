library(tidyverse)
library(here)
setwd(here())
library(janitor)
library(Rtsne)
library(dimRed)
library(patchwork)
library(slider)
library(lsa)
library(dtw)
library(reshape2)
library(ggrepel)

## What is this script for?
# Inputs: preprocessed / split sentences
#         MBERT embeddings
# Function: t-SNE and UMAP trajectory plots
#           DTW
#           Basic LM plot


## Load in the files ----
all.clean.split <- read_csv("../data/text_split/all.clean.split_VChunk.csv",
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

## Get chunk indexes ----
# Read in all.clean.split_VChunk instead of all.cl...it_V2, which has the story, chunk, sentence instead of just story, sentence

## tSNE portion ----
set.seed(1234)

embeds.tsne <- all.clean.split_wEmb |>
  select(starts_with("x")) |>
  Rtsne(dims = 2, pca = F, perplexity = 30, theta = 0.5, check_duplicates = F)

embeds.tsne_df <- embeds.tsne$Y |>
  as.data.frame() |>
  rename(tSNE1 = "V1", tSNE2 = "V2") |>
  mutate(ID2 = row_number())


embeds3 <- all.clean.split_wEmb |>
  mutate(ID2 = row_number()) |>
  left_join(embeds.tsne_df, by = "ID2") |>
  select(-ID2) |>
  group_by(story) |>
  mutate(sent_num.prop = sent_num/n()) |>
  ungroup() |>
  select(story, lang, sentences, sent_num, sent_num.prop, tSNE1, tSNE2, everything())

# tSNE graphing ----

embeds3 |>
  mutate(graph_label = paste(story, lang)) |>
  ggplot(aes(x=tSNE1, y = tSNE2, color=factor(graph_label), group=graph_label,
             alpha = sent_num.prop)) +
  geom_path() +
  facet_wrap(~graph_label) +
  # scale_color_manual(values = wes_palette("Darjeeling1"), name = "Story") +
  scale_color_brewer(type = "qual", palette = 3, name = "Story")+
  scale_alpha(guide="none") +
  theme_bw()
# 
# a <- embeds3 |>  
#   mutate(graph_label = paste(lang, story)) |> 
#   ggplot(aes(x=sent_num.prop, y = tSNE1, color=factor(graph_label), 
#              group=graph_label)) +
#   geom_path() +
#   # facet_wrap(~graph_label) + 
#   scale_color_manual(values = wes_palette("Darjeeling1"), name = "Story") +
#   theme_minimal() +
#   theme(legend.position = "none")+
#   ggtitle("Story paths", subtitle = "tSNE1 only")+
#   xlab("Proportional sentence number (within the story)")
# 
# b <- embeds3 |>  
#   mutate(graph_label = paste(lang, story)) |> 
#   ggplot(aes(x=sent_num.prop, y = tSNE2, color=factor(graph_label), 
#              group=graph_label)) +
#   geom_path() +
#   # facet_wrap(~graph_label) + 
#   scale_color_manual(values = wes_palette("Darjeeling1"), name = "Story") +
#   theme_minimal() +
#   ggtitle("Story paths", subtitle = "tSNE2 only")+
#   xlab("Proportional sentence number (within the story)")
# 
# a / b
# 
# 
# ## Smoothing ----
# before = 30
# after = 0
# 
# xtabs(~story, embeds3)
# 
# embeds4 <- embeds3 |>  
#   group_by(story) |> 
#   mutate(across(tSNE1:tSNE2, ~ slide_dbl(.x, ~mean(.x), 
#                                          .before = before, 
#                                          .after = after, 
#                                          .complete = T)))
# 
# paths.1 <- embeds4 |>  
#   mutate(graph_label = paste(story, lang)) |> 
#   ggplot(aes(x=tSNE1, y = tSNE2, group=graph_label)) +
#   geom_path(color="grey50", size=1) +
#   geom_point(aes(color=sent_num), size=1) +
#   facet_grid(~graph_label) + 
#   scale_color_gradient2(guide='none', midpoint = .5, mid="grey50") + 
#   scale_alpha(guide=F) + 
#   theme_bw(base_size = 15)
# 
# paths.A <- embeds4 |>  
#   mutate(graph_label = paste(story, lang)) |> 
#   ggplot(aes(x=tSNE1, y = tSNE2, color=as.factor(story), group=graph_label)) +
#   geom_path(size=1) +
#   geom_point(size=1) +
#   scale_color_discrete(guide="none") + 
#   scale_alpha(guide="none") + 
#   theme_bw(base_size = 15)
# 
# paths.1 + paths.A
# 
# 
# # Normalized smoothing
# 
# normalized_bin_size = .05
# 
# embeds_normalized_smooth <- embeds3 |>  
#   group_by(story) |> 
#   mutate(sent_num_bin_n = normalized_bin_size * 
#            ceiling((sent_num / normalized_bin_size))) %>%
#   group_by(story, sent_num_bin_n) %>%
#   summarize(across(tSNE1:tSNE2, ~ mean(.x)), 
#             n=n(),
#             .groups="drop")
# 
# 
# embeds_normalized_smooth |>  
#   ggplot(aes(x=tSNE1, y = tSNE2, group=story, color=sent_num_bin_n)) +
#   geom_path(size=1) +
#   geom_point(size=2) +
#   facet_grid(~story) + 
#   scale_color_gradient2(guide=F, midpoint = .5, mid="grey50") + 
#   scale_alpha(guide=F) + 
#   theme_bw(base_size = 15)




## UMAP part ----

umap.embeds <- read_csv("../data/embeddings/karma.umap_embeds.csv", show_col_type = F) |> 
  select(-story) |> 
  cbind(embeds3 |> select(-c(starts_with("x"))))

umap.embeds |>  
  mutate(graph_label = paste(story, lang)) |> 
  ggplot(aes(x=umap.x, y = umap.y, color=factor(graph_label), group=graph_label,
             alpha = sent_num.prop)) +
  geom_path() +
  facet_wrap(~graph_label) + 
  scale_color_brewer(type = "qual", palette = 3, name = "Story")+
  scale_alpha(guide="none") +
  theme_bw()

a <- umap.embeds |>  
  mutate(graph_label = paste(story, lang)) |> 
  ggplot(aes(x=sent_num.prop, y = umap.x, color=factor(graph_label), 
             group=graph_label)) +
  geom_path() +
  scale_color_brewer(type = "qual", palette = 3, name = "Story")+
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank())

b <- umap.embeds |>  
  mutate(graph_label = paste(story, lang)) |> 
  ggplot(aes(x=sent_num.prop, y = umap.y, color=factor(graph_label), 
             group=graph_label)) +
  geom_path() +
  # facet_wrap(~graph_label) + 
  scale_color_brewer(type = "qual", palette = 3, name = "Story")+
  theme_minimal() +
  xlab("Proportional sentence number (within the story)")

a / b


## UMAP: Smoothing ----
before = 30
after = 0

xtabs(~story, umap.embeds)

umap.embeds.4 <- umap.embeds |>  
  group_by(story) |> 
  mutate(across(umap.x:umap.y, ~ slide_dbl(.x, ~mean(.x), 
                                         .before = before, 
                                         .after = after, 
                                         .complete = T)))

paths.1 <- umap.embeds.4 |>  
  mutate(graph_label = paste(story, lang)) |> 
  ggplot(aes(x=umap.x, y = umap.y, group=graph_label)) +
  geom_path(color="grey50", linewidth =1) +
  geom_point(aes(color=factor(sent_num.prop)), size=2) +
  facet_grid(~graph_label) + 
  # scale_color_gradient2(guide='none', midpoint = .5, mid="grey50") + 
  scale_color_brewer(type = "qual", palette = 3)+
  scale_alpha(guide=F) + 
  theme_bw(base_size = 15)+
  ggtitle("UMAP story paths")+
  theme(legend.position = "none")

paths.1

umap.extremes <- umap.embeds.4 |> 
  filter(sent_num == 31 | sent_num.prop == 1) |>  
  mutate(graph_label = paste(story, lang)) 

paths.A <- umap.embeds.4 |>  
  mutate(graph_label = paste(story, lang)) |> 
  ggplot(aes(x=umap.x, y = umap.y, color= graph_label, 
             group=graph_label, alpha = sent_num.prop,
             shape = graph_label)) +
  geom_path(size = 0.75) +
  geom_point(data = umap.extremes, aes(x=umap.x, y = umap.y,
                                       color= graph_label, 
                                       group=graph_label, 
                                       alpha = sent_num.prop,
                                       shape = graph_label),
             size = 5) +
  scale_color_brewer(type = "qual", palette = 3, name = "Story")+
  scale_shape_manual(values = c(15, 16, 17, 18),
                     name = "Story")+
  scale_alpha(guide="none") +
  theme_bw(base_size = 15)+
  labs(caption = "Alpha as proportional sentence number")

paths.A

umap.embeds |>  
  mutate(graph_label = paste(story, lang)) |> 
  ggplot(aes(x=umap.x, y = umap.y, color= graph_label, 
             group=graph_label, alpha = chunk_prop_num,
             shape = graph_label)) +
  geom_path(size = 0.75) +
  scale_color_brewer(type = "qual", palette = 3, name = "Story")+
  scale_alpha(guide="none") +
  theme_bw(base_size = 15)+
  labs(caption = "Alpha as proportional sentence number")+
  facet_wrap(~chunk)

umap.embeds.4 |>  
  mutate(graph_label = paste(story, lang)) |> 
  ggplot(aes(x=umap.x, y = umap.y, color= graph_label, 
             group=graph_label, alpha = sent_num.prop,
             shape = graph_label)) +
  geom_path(size = 0.75) +
  geom_point(data = umap.extremes, aes(x=umap.x, y = umap.y,
                                       color= graph_label, 
                                       group=graph_label, 
                                       alpha = sent_num.prop,
                                       shape = graph_label),
             size = 5) +
  scale_color_brewer(type = "qual", palette = 3, name = "Story")+
  scale_shape_manual(values = c(15, 16, 17, 18),
                     name = "Story")+
  scale_alpha(guide="none") +
  theme_bw(base_size = 15)+
  labs(caption = "Alpha as proportional sentence number")+
  facet_wrap(~graph_label)+
  theme(legend.position = "none")


# Normalized smoothing

normalized_bin_size = .05

embeds_normalized_smooth <- umap.embeds |>  
  group_by(story) |> 
  mutate(sent_num_bin_n = normalized_bin_size * 
           ceiling((sent_num / normalized_bin_size))) %>%
  group_by(story, sent_num_bin_n) %>%
  summarize(across(umap.x:umap.y, ~ mean(.x)), 
            n=n(),
            .groups="drop")


# embeds_normalized_smooth |>  
#   ggplot(aes(x=umap.x, y = umap.y, color= story, 
#              group=story, 
#              alpha = as.numeric(sent_num.prop))) +
#   geom_path(size=1) +
#   # geom_point() +
#   scale_color_brewer(type = "qual", palette = 3, name = "Story")+
#   # scale_shape_manual(values = c(21, 23, 24, 22), 
#   # name = "Story")+
#   # scale_alpha(guide="none") + 
#   theme_bw(base_size = 15)+
#   ggtitle("Overlayed story paths")


## DTW ----

umap.embeds |> colnames()


## ENG1, RUS2 ----
eng1.rus2 <- dtw(umap.embeds |> filter(story == 1) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 2) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
eng1.rus2.distance <- eng1.rus2$normalizedDistance

## ENG1, FRE3 ----
eng1.fre3 <- dtw(umap.embeds |> filter(story == 1) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 3) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
eng1.fre3.distance <- eng1.fre3$normalizedDistance

## ENG1, ENG4 ----
eng1.eng4 <- dtw(umap.embeds |> filter(story == 1) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 4) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
eng1.eng4.distance <- eng1.eng4$normalizedDistance

## RUS2, ENG1 ----

rus2.eng1 <- dtw(umap.embeds |> filter(story == 2) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 1) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
rus2.eng1.distance <- rus2.eng1$normalizedDistance

## RUS2, FRE3 ----

rus2.fre3 <- dtw(umap.embeds |> filter(story == 2) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 3) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
rus2.fre3.distance <- rus2.fre3$normalizedDistance

## RUS2, ENG4 ----

rus2.eng4 <- dtw(umap.embeds |> filter(story == 2) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 4) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
rus2.eng4.distance <- rus2.eng4$normalizedDistance

## FRE3, ENG1 ----
fre3.eng1 <- dtw(umap.embeds |> filter(story == 3) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 1) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
fre3.eng1.distance <- fre3.eng1$normalizedDistance

## FRE3, RUS2 ----
fre3.rus2 <- dtw(umap.embeds |> filter(story == 3) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 2) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
fre3.rus2.distance <- fre3.rus2$normalizedDistance

## FRE3, ENG4 ----
fre3.eng4 <- dtw(umap.embeds |> filter(story == 3) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 4) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
fre3.eng4.distance <- fre3.eng4$normalizedDistance

## ENG4, RUS2 ----
eng4.rus2 <- dtw(umap.embeds |> filter(story == 4) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 2) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
eng4.rus2.distance <- eng4.rus2$normalizedDistance

## ENG4, FRE3 ----
eng4.fre3 <- dtw(umap.embeds |> filter(story == 4) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 3) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
eng4.fre3.distance <- eng4.fre3$normalizedDistance

## ENG4, ENG1 ----
eng4.eng1 <- dtw(umap.embeds |> filter(story == 4) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 1) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
eng4.eng1.distance <- eng4.eng1$normalizedDistance

eng1.eng1 <- dtw(umap.embeds |> filter(story == 1) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 1) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
eng1.eng1.distance <- eng1.eng1$normalizedDistance

rus2.rus2 <- dtw(umap.embeds |> filter(story == 2) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 2) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
rus2.rus2.distance <- rus2.rus2$normalizedDistance

fre3.fre3 <- dtw(umap.embeds |> filter(story == 3) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 3) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
fre3.fre3.distance <- fre3.fre3$normalizedDistance

eng4.eng4 <- dtw(umap.embeds |> filter(story == 4) |> 
                   select(umap.x, umap.y),
                 umap.embeds |> filter(story == 4) |> 
                   select(umap.x, umap.y),
                 keep = TRUE)
eng4.eng4.distance <- eng4.eng4$normalizedDistance

## Final heatmap ----



dtw.df <- data.frame(story = c("eng1", "rus2","fre3", "eng4"),
                     eng1 = c(eng1.eng1.distance, rus2.eng1.distance, fre3.eng1.distance, eng4.eng1.distance),
                     rus2 = c(eng1.rus2.distance, rus2.rus2.distance, fre3.rus2.distance, eng4.rus2.distance),
                     fre3 = c(eng1.fre3.distance, rus2.fre3.distance, fre3.fre3.distance, eng4.fre3.distance),
                     eng4 = c(eng1.eng4.distance, rus2.eng4.distance, fre3.eng4.distance, eng4.eng4.distance)) |> 
  melt()

write_csv(dtw.df, file = "../data/processed_data/karma_dtw_values.csv")

ggplot(dtw.df, aes(x = factor(story,
                              levels = c("eng1", "rus2","fre3", "eng4")), 
                   y = factor(variable,
                              levels = c("eng1", "rus2","fre3", "eng4")), 
                   fill = value))+
  geom_tile()+
  scale_fill_distiller(name = "DTW")+
  xlab("Story")+
  ylab("Story")+
  theme_bw()


## Chunk DTW ----

chunk.dtw <- data.frame()
for (storyA in 1:4){
  for (chunkA in 1:5){
    for (storyB in 1:4){
      for (chunkB in 1:5){
        print(paste(storyA, chunkA, storyB, chunkB))
        subsetA <- umap.embeds |> 
          filter(story == storyA & chunk == chunkA)|> 
          select(umap.x, umap.y)
        
        subsetB <- umap.embeds |> 
          filter(story == storyB & chunk == chunkB)|> 
          select(umap.x, umap.y)
        
        comparison <- dtw(subsetA, subsetB, keep = T)
        
        distance <- comparison$normalizedDistance
        
        new_row <- data.frame(x.story = storyA,
                              x.chunk = chunkA,
                              y.story = storyB,
                              y.chunk = chunkB,
                              dtw = distance)
        
        chunk.dtw <- rbind(chunk.dtw, new_row)
      }
    }
  }
}
rm(subsetA, subsetB)

chunk.dtw <- chunk.dtw |> 
  mutate(x = paste(x.story, x.chunk, sep = "."),
         y = paste(y.story, y.chunk, sep = "."))

ggplot(chunk.dtw, aes(x = x, y = y, fill = dtw))+
  geom_tile()+
  scale_fill_distiller(name = "DTW")+
  xlab("Story.chunk")+
  ylab("Story.chunk")+
  geom_vline(xintercept = c(5.5, 10.5, 15.5))+
  geom_hline(yintercept = c(5.5, 10.5, 15.5))+
  theme_bw()


## Plot x against translation distance, use color/shape to show that DTW increases as translation distance increases

lm.df <- data.frame(dtw.distance = c(eng1.rus2.distance,
                                     eng1.fre3.distance,
                                     eng1.eng4.distance,
                                     rus2.fre3.distance,
                                     rus2.eng4.distance,
                                     fre3.eng4.distance),
                    translation_steps = c(1,
                                          2,
                                          3,
                                          1,
                                          2,
                                          1),
                    label = c("e1r2",
                              "e1f3",
                              "e1e4",
                              "r2f3",
                              "r2e4",
                              "f3e4"))


ggplot(lm.df, aes(x = translation_steps, y = dtw.distance))+
  geom_smooth(method = "lm", se = F, color = "darkblue")+
  geom_point(shape = 21, fill = "lightblue", color = "black")+
  geom_label_repel(aes(label = label), min.segment.length = 0.1)+
  xlab("Translation steps")+
  ylab("DTW distance")+
  theme_bw()


## Chunk equivalent plot ----

chunk.dtw |> 
  mutate(general_distance = abs(x.story - y.story) + abs(x.chunk - y.chunk),
         translation_steps = abs(x.story - y.story),
         chunk_steps = abs(x.chunk - y.chunk),
         label = paste(x, y, sep = "-")) |> 
  ggplot(aes(x = general_distance, y = dtw))+
  geom_smooth(method = "lm", se = F, color = "darkblue")+
  geom_point(shape = 21, fill = "lightblue", color = "black")+
  xlab("General distance\nabs(x.story - y.story) + abs(x.chunk - y.chunk)")+
  ylab("DTW Distance")+
  theme_bw()

chunk.dtw |> 
  mutate(general_distance = abs(x.story - y.story) + abs(x.chunk - y.chunk),
         translation_steps = abs(x.story - y.story),
         chunk_steps = abs(x.chunk - y.chunk),
         label = paste(x, y, sep = "-")) |> 
  ggplot(aes(x = chunk_steps, y = dtw))+
  geom_smooth(method = "lm", se = F, color = "darkblue")+
  geom_point(shape = 21, fill = "lightblue", color = "black")+
  xlab("Chunk steps")+
  ylab("DTW Distance")+
  theme_bw()



write_csv(chunk.dtw, "../data/processed_data/karma.chunk_dtw.csv")

