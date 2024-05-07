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

umap.embeds <- read_csv("../data/embeddings/karma.umap_embeds.csv", 
                        show_col_type = F) |> 
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
before = 10
after = 0

xtabs(~story, umap.embeds)

umap.embeds.4 <- umap.embeds |>  
  group_by(story) |> 
  mutate(across(umap.x:umap.y, ~ slide_dbl(.x, ~mean(.x), 
                                         .before = before, 
                                         .after = after, 
                                         .complete = T)))


paths.A <- umap.embeds.4 |>  
  mutate(lang = if_else(lang == 'eng', 'English',
                        if_else(lang == 'rus','Russian','French')),
         graph_label = paste(story, lang)) |> 
  drop_na() |> 
  group_by(story) |> 
  mutate(index = 1:n(),
         path_label = if_else(index == 1, 'Start',
                              if_else(index == max(index), 'End',NA))) |> 
  ggplot(aes(x=umap.x, y = umap.y, color= graph_label, 
             group=graph_label, shape = graph_label)) +
  geom_path(linewidth = 1) +
  geom_label(aes(label = path_label))+
  scale_color_brewer(type = "qual", palette = 3, name = "Story")+
  scale_shape_manual(values = c(15, 16, 17, 18),
                     name = "Story")+
  scale_alpha(guide="none") +
  theme_bw(base_size = 15)

paths.A

ggsave(paths.A,
       filename = here("../figures/karma.umap-smoothed_full.png"),
       units = "in",
       dpi = 300,
       width = 10, height = 7)

sole.track <- umap.embeds.4 |>  
  filter(lang == "eng" & story == 1) |> 
  mutate(lang = if_else(lang == 'eng', 'English',
                        if_else(lang == 'rus','Russian','French')),
         graph_label = paste(story, lang)) |> 
  drop_na() |> 
  group_by(story) |> 
  mutate(index = 1:n(),
         path_label = if_else(index == 1, 'Start',
                              if_else(index == max(index), 'End',NA))) |> 
  ggplot(aes(x=umap.x, y = umap.y, color= graph_label, 
             group=graph_label, shape = graph_label)) +
  geom_path(linewidth = 1) +
  geom_label(aes(label = path_label))+
  scale_color_brewer(type = "qual", palette = 3, name = "Story")+
  scale_shape_manual(values = c(15, 16, 17, 18),
                     name = "Story")+
  scale_alpha(guide="none") +
  theme_bw(base_size = 15)+
  theme(legend.position = 'none')

# sole.track
# 
# ggsave(sole.track,
#        filename = here("../figures/karma.umap-smoothed_sole.png"),
#        units = "in",
#        dpi = 300,
#        width = 10, height = 7)

sole.track + paths.A +
  plot_annotation(tag_levels = 'A')


ggsave(filename = here("../figures/karma.trajectories.png"),
       units = "in",
       dpi = 600,
       width = 12, height = 5)


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

# normalized_bin_size = .05
# 
# embeds_normalized_smooth <- umap.embeds |>  
#   group_by(story) |> 
#   mutate(sent_num_bin_n = normalized_bin_size * 
#            ceiling((sent_num / normalized_bin_size))) %>%
#   group_by(story, sent_num_bin_n) %>%
#   summarize(across(umap.x:umap.y, ~ mean(.x)), 
#             n=n(),
#             .groups="drop")
# 
# 
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
  melt() |> 
  mutate(x.story = as.numeric(substr(story, 4,5)),
         y.story = as.numeric(substr(variable, 4,5)),
         translation.steps = (x.story - y.story),
         pair = paste(story, variable)) |> 
  rename(dtw.distance = value)

write_csv(dtw.df, file = "../data/processed_data/karma_dtw_values.csv")

dtw.plot <- ggplot(dtw.df, aes(x = factor(story,
                              levels = c("eng1", "rus2","fre3", "eng4")), 
                   y = factor(variable,
                              levels = c("eng1", "rus2","fre3", "eng4")), 
                   fill = value))+
  geom_tile()+
  scale_fill_distiller(name = "DTW", palette = 2)+
  xlab("Story")+
  ylab("Story")+
  theme_bw()+
  scale_x_discrete(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  geom_label(aes(label = round(value, digits = 3)), fill = "white")

dtw.plot

ggsave(dtw.plot,
       filename = here("../figures/karma.dtw-standard.png"),
       units = "in",
       dpi = 300,
       width = 10, height = 7)

dtw.df |> 
  mutate(x.story = as.numeric(substr(story, 4,5)),
         y.story = as.numeric(substr(variable, 4,5)),
         translation.steps = (x.story - y.story),
         pair = paste(story, variable)) |> 
  filter(translation.steps < 0) |> 
  ggplot(aes(x = abs(translation.steps), y = value))+
  geom_point(size = 2, shape = 21, fill = "lightblue")+
  theme_bw(base_size = 14)+
  geom_smooth(method = "lm", color = "darkblue", se = F)+
  geom_label_repel(aes(label = pair), min.segment.length = 0.1)+
  xlab("Translation steps")+
  ylab("DTW Distance")+
  scale_x_continuous(breaks = 1:3)+
  theme(panel.grid.minor.x = element_blank())



## Testing stat model for now:

lmdf <- dtw.df |> 
  filter(translation.steps >= 0)

lm1 <- lm(translation.steps ~ 1 + dtw.distance, data = lmdf)
summary(lm1)


# ## Trying VAR on the UMAP reduced data ----
# library(vars) ## Not good practice, but loading it here since it masks SELECT
# 
# original.data <- umap.embeds |> 
#   filter(story == 1) |> 
#   dplyr::select(umap.x, umap.y)
# 
# VARselect(original.data, type = "both")
# 
# var.model <- VAR(original.data, p = 5, type = "both")
# summary(var.model)
# 
# var.pred <- predict(var.model, n.ahead = 12)
# par(mai=rep(0.4, 4)); plot(var.pred)
# par(mai=rep(0.4, 4)); fanchart(var.pred)
# 
# predict.x <- function(model, subset.df){
#   subset
#   
#   
# }
# 
# coef.var <- coef(var.model)
# 
# coef.var$umap.x
# 
# 
# preds <- predict(var.model)
# 
# pred.df <- data.frame()
# 
# for (i in 6:nrow(original.data)){
#   subset <- original.data[(i-5):i,]
#   print(subset)
#   # temp.pred <- predict(var.model,
#   #                      subset, n.ahead = 1)
#   # temp.df <- data.frame(pred.x = data.frame(temp.pred$fcst)$umap.x.fcst,
#   #                       pred.y = data.frame(temp.pred$fcst)$umap.y.fcst)
#   # pred.df <- rbind(pred.df,
#   #                  temp.df)
# }


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

