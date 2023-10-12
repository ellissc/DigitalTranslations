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

# load_granular <- function(grains){
#   ## Load in the files ----
#   all.clean.split <- read_csv("../data/text_split/granular_split.csv",
#                               show_col_type = F)
#   
#   eng1.clean.split <- all.clean.split |> 
#     filter(story == 1 & grain_size == grains)
#   rus2.clean.split <- all.clean.split |> 
#     filter(story == 2 & grain_size == grains)
#   fre3.clean.split <- all.clean.split |> 
#     filter(story == 3 & grain_size == grains)
#   eng4.clean.split <- all.clean.split |> 
#     filter(story == 4 & grain_size == grains)
#   
#   ## M-BERT in Colab ----
#   
#   eng1.embeds <- read_csv(paste0("../data/embeddings/mbert/s1_",
#                                  "g",grains,
#                                  "_embeds_raw.csv"), show_col_type = F) |> 
#     clean_names() |> 
#     mutate(sent_num = 1:n()) 
#   
#   rus2.embeds <- read_csv(paste0("../data/embeddings/mbert/s2_",
#                                  "g",grains,
#                                  "_embeds_raw.csv"), show_col_type = F)|> 
#     clean_names() |> 
#     mutate(sent_num = 1:n()) 
#   
#   fre3.embeds <- read_csv(paste0("../data/embeddings/mbert/s3_",
#                                  "g",grains,
#                                  "_embeds_raw.csv"), show_col_type = F)|> 
#     clean_names() |> 
#     mutate(sent_num = 1:n()) 
#   
#   eng4.embeds <- read_csv(paste0("../data/embeddings/mbert/s4_",
#                                  "g",grains,
#                                  "_embeds_raw.csv"), show_col_type = F)|> 
#     clean_names() |> 
#     mutate(sent_num = 1:n()) 
#   
#   # Join with sentences.
#   eng1.clean.split <- eng1.clean.split |> 
#     mutate(sent_num = 1:n()) |> 
#     left_join(eng1.embeds, by = "sent_num")
#   
#   rus2.clean.split <- rus2.clean.split |> 
#     mutate(sent_num = 1:n()) |> 
#     left_join(rus2.embeds, by = "sent_num")
#   
#   fre3.clean.split <- fre3.clean.split |> 
#     mutate(sent_num = 1:n()) |> 
#     left_join(fre3.embeds, by = "sent_num")
#   
#   eng4.clean.split <- eng4.clean.split |> 
#     mutate(sent_num = 1:n()) |> 
#     left_join(eng4.embeds, by = "sent_num")
#   
#   all.clean.split_wEmb <- rbind(eng1.clean.split, 
#                                 rus2.clean.split, 
#                                 fre3.clean.split,
#                                 eng4.clean.split) |>
#     group_by(story) |>
#     mutate(sent_num.prop = sent_num/n()) |>
#     ungroup()
#   
#   return(all.clean.split_wEmb)
# }
# 
# 
# grains.5 <- load_granular(5)
# grains.10 <- load_granular(10)
# grains.20 <- load_granular(20)

grain.df <- read_csv("../data/embeddings/karma.grained_umap_embeds.csv",
                     show_col_type = F) |> 
  group_by(story, grain) |> 
  mutate(index = 1:n(),
         max = max(index),
         prop.index = index/max) |> 
  select(-max) |> 
  mutate(story = if_else(story == 1 | story == 4,
                         paste("english", story),
                         if_else(story == 2,
                                 paste("russian", story),
                                 if_else(story == 3,
                                         paste("french", story),
                                         "other")))) |> 
  mutate(story = factor(story,
                        levels = c("english 1", "russian 2", "french 3","english 4"))) |> 
  ungroup()

# Raw plot

# grain.df |>  
#   ggplot(aes(x=umap.x, y = umap.y, color=prop.index)) +
#   geom_point(color = "black")+
#   geom_path(size = 1)+
#   facet_wrap(story~grain, ncol = 20, scales = "free")+
#   scale_color_distiller(type = "div", palette = 4, 
#                         name = "Sentence\nnumber")+
#   theme_bw()+
#   ggtitle("Raw story trajectories")


# Smoothing ----
before = 10
after = 0

grain.smoothed <- grain.df |>  
  group_by(story, grain) |> 
  mutate(across(umap.x:umap.y, ~ slide_dbl(.x, ~mean(.x), 
                                           .before = before, 
                                           .after = after, 
                                           .complete = F))) |> 
  ungroup()

umap.extremes <- grain.smoothed |> 
  filter(prop.index == 1) |> 
  mutate(label = factor(paste(grain, "sentences"),
                        levels = paste(1:20, "sentences")))


paths.A <- grain.smoothed |> 
  mutate(label = factor(paste(grain, "sentences"),
                        levels = paste(1:n(), "sentences"))) |>  
  filter(grain <= 6) |> 
  ggplot(aes(x=umap.x, y = umap.y, color= story, 
             group= story, alpha = prop.index,
             shape = story)) +
  geom_path(size = 0.75) +
  geom_point(data = umap.extremes |> filter(grain <= 6),
             aes(x=umap.x, y = umap.y,
                                       color= story, 
                                       group=story, 
                                       alpha = prop.index,
                                       shape = story),
             size = 5) +
  scale_color_brewer(type = "qual", palette = 3, name = "Story")+
  scale_shape_manual(values = c(15, 16, 17, 18),
                     name = "Story")+
  scale_alpha(guide="none") +
  theme_bw(base_size = 15)+
  labs(caption = "Alpha as proportional sentence number")+
  ggtitle("Scale of preservation", subtitle = "Adjusting number of sentences per embedding")+
  facet_wrap(~label, scales = "free")

paths.A

ggsave(paths.A,
       filename = here("../figures/karma.umap-set.png"),
       units = "in",
       dpi = 450,
       width = 12, height = 7)


## DTW part ----
iterative_dtw <- function(umap.embeds, grains){
  ## ENG1, RUS2 ----
  eng1.rus2 <- dtw(umap.embeds |> filter(story == "english 1" & grain == grains) |> 
                     select(umap.x, umap.y),
                   umap.embeds |> filter(story == "russian 2" & grain == grains) |> 
                     select(umap.x, umap.y),
                   keep = TRUE)
  eng1.rus2.distance <- eng1.rus2$normalizedDistance
  
  ## RUS2, FRE3 ----
  
  rus2.fre3 <- dtw(umap.embeds |> filter(story == "russian 2"  & grain == grains) |> 
                     select(umap.x, umap.y),
                   umap.embeds |> filter(story == "french 3" & grain == grains) |> 
                     select(umap.x, umap.y),
                   keep = TRUE)
  rus2.fre3.distance <- rus2.fre3$normalizedDistance
  
  ## FRE3, ENG4 ----
  fre3.eng4 <- dtw(umap.embeds |> filter(story == "french 3" & grain == grains) |> 
                     select(umap.x, umap.y),
                   umap.embeds |> filter(story == "english 4" & grain == grains) |> 
                     select(umap.x, umap.y),
                   keep = TRUE)
  fre3.eng4.distance <- fre3.eng4$normalizedDistance
  
  ## ENG4, ENG1 ----
  eng4.eng1 <- dtw(umap.embeds |> filter(story == "english 4" & grain == grains) |> 
                     select(umap.x, umap.y),
                   umap.embeds |> filter(story == "english 1" & grain == grains) |> 
                     select(umap.x, umap.y),
                   keep = TRUE)
  eng4.eng1.distance <- eng4.eng1$normalizedDistance
  
  dtw.df <- data.frame(x.story = c("english 1","russian 2", "french 3","english 4"),
                       y.story = c("russian 2", "french 3","english 4","english 1"),
                       distance = c(eng1.rus2.distance,rus2.fre3.distance,fre3.eng4.distance, eng4.eng1.distance)) |> 
    mutate(grains = grains)
  
  return(dtw.df)
  
}

dtw.df <- data.frame()

for (grain in 1:20){
  dtw.df <- rbind(dtw.df,
                   iterative_dtw(grain.df, grain))
}

dtw.df <- dtw.df |> 
  mutate(pair = paste(x.story, "->", y.story)) |> 
  mutate(pair = factor(pair,
                       levels = c("english 1 -> russian 2",
                                  "russian 2 -> french 3",
                                  "french 3 -> english 4",
                                  "english 4 -> english 1")))

dtw.plot <- ggplot(dtw.df,
       aes(x = grains, y = distance, color = pair, shape = pair, group = pair))+
  geom_smooth(se = F, linetype = "dashed", method = "lm")+
  geom_point(size = 2)+
  scale_color_brewer(type = "qual", palette = 3, name = "Pair")+
  scale_shape_manual(name = "Pair", values = c(15, 16, 17, 22))+
  theme_bw(base_size = 14)+
  xlab("Tile size (sentences per tile)")+
  ylab("DTW Distance")

dtw.plot
  
ggsave(dtw.plot,
       filename = here("../figures/karma.dtw-grainsize.png"),
       units = "in",
       dpi = 300,
       width = 12, height = 6)


dtw.plot2 <- dtw.df |> 
  filter(grains < 4) |> 
  ggplot(aes(x = grains, y = distance, color = pair, 
             shape = pair, group = pair))+
  geom_smooth(se = F, method = "lm")+
  geom_point(size = 2)+
  scale_color_brewer(type = "qual", palette = 3, name = "Pair")+
  scale_shape_manual(name = "Pair", values = c(15, 16, 17, 22))+
  theme_bw(base_size = 14)+
  xlab("Tile size (sentences per tile)")+
  ylab("DTW Distance")

dtw.plot2

ggsave(dtw.plot2,
       filename = here("../figures/karma.dtw-grainsize-small.png"),
       units = "in",
       dpi = 300,
       width = 12, height = 6)












## DTW, old ----

dtw_analysis <- function(umap.embeds, grains){
  
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
                       eng1 = c(eng1.eng1.distance, rus2.eng1.distance,
                                fre3.eng1.distance, eng4.eng1.distance),
                       rus2 = c(eng1.rus2.distance, rus2.rus2.distance,
                                fre3.rus2.distance, eng4.rus2.distance),
                       fre3 = c(eng1.fre3.distance, rus2.fre3.distance,
                                fre3.fre3.distance, eng4.fre3.distance),
                       eng4 = c(eng1.eng4.distance, rus2.eng4.distance,
                                fre3.eng4.distance, eng4.eng4.distance)) |> 
    melt() |> 
    mutate(grains = grains)
  
  return(dtw.df)
}

dtw.1 <- read_csv(here("../data/processed_data/karma_dtw_values.csv"), show_col_types = F) |> 
  mutate(grains = 1)
dtw.5 <- dtw_analysis(umap.embeds.5, 5)
dtw.10 <- dtw_analysis(umap.embeds.10, 10)
dtw.20 <- dtw_analysis(umap.embeds.20, 20)

dtw.all <- rbind(dtw.1,
                 dtw.5, 
                 dtw.10,
                 dtw.20) |> 
  mutate(story_pair = paste(story, variable, sep = "."))

dtw.full.plot <- ggplot(dtw.all, aes(x = grains, y = value, 
                    color = story, shape = variable))+
  geom_point(size = 2, position = position_jitter(width = 0.5))+
  scale_color_brewer(type = "qual", palette = 3, name = "Story1")+
  scale_shape_manual(name = "Story2", values = c(15, 22, 16, 17))+
  theme_bw()+
  xlab("Grain size (sentences per group)")+
  ylab("DTW")+
  ggtitle("Relationship between grain size and DTW")+
  geom_smooth(data = dtw.all |> filter(story != variable),
              aes(x = grains, y = value, 
                  color = story, group = story),
              se = F)

ggsave(dtw.full.plot,
       filename = here("../figures/karma.dtw-grainsize.png"),
       units = "in",
       dpi = 300,
       width = 11, height = 7)


dtw.hmp <- ggplot(dtw.all, aes(x = factor(story,
                              levels = c("eng1", "rus2","fre3", "eng4")), 
                   y = factor(variable,
                              levels = c("eng1", "rus2","fre3", "eng4")), 
                   fill = value))+
  geom_tile()+
  scale_fill_distiller(name = "DTW")+
  xlab("Story")+
  ylab("Story")+
  theme_bw()+
  facet_wrap(~grains)+
  ggtitle("Heatmap of DTW by grain size")

ggsave(dtw.hmp,
       filename = here("../figures/karma.dtw-grainsize.heatmap.png"),
       units = "in",
       dpi = 300,
       width = 11, height = 7)

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

