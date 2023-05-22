library(tidyverse)
library(here)
setwd(here())
library(janitor)
library(patchwork)
library(reshape2)
theme_set(theme_bw() + theme(text =  element_text(size = 14)))
library(ggh4x)

## What is this script for?
# Input: similarity matrices for full and chunks
# Function: story reconstruction based on other stories
#
# To do: story reconstruction based on chunks -- check MBERT code, very low reconstruction scores.


## Load in the files ----

## Chunks first
chunk_melted <- read_csv("../data/embeddings/similarity_matrices/chunk_full_matmult.csv",
                         show_col_types = F)

# Eng1: 158 sentences
# Rus2: 142 sentences
# Fre3: 161 sentences
# Eng4: 172 sentences

## Nrows of Ncols
eng1.eng1 <-
  read_csv("../data/embeddings/similarity_matrices/english1.english1.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("eng1.", seq(1:n())))
colnames(eng1.eng1) = paste0("eng1.", seq(1:ncol(eng1.eng1)))

eng1.rus2 <-
  read_csv("../data/embeddings/similarity_matrices/english1.russian2.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("eng1.", seq(1:n())))
colnames(eng1.rus2) = paste0("rus2.", seq(1:ncol(eng1.rus2)))

eng1.fre3 <-
  read_csv("../data/embeddings/similarity_matrices/english1.french3.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("eng1.", seq(1:n())))
colnames(eng1.fre3) = paste0("fre3.", seq(1:ncol(eng1.fre3)))

eng1.eng4 <-
  read_csv("../data/embeddings/similarity_matrices/english1.english4.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("eng1.", seq(1:n())))
colnames(eng1.eng4) = paste0("eng4.", seq(1:ncol(eng1.eng4)))

rus2.eng1 <-
  read_csv("../data/embeddings/similarity_matrices/russian2.english1.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("rus2.", seq(1:n())))
colnames(rus2.eng1) = paste0("eng1.", seq(1:ncol(rus2.eng1)))

rus2.rus2 <-
  read_csv("../data/embeddings/similarity_matrices/russian2.russian2.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("rus2.", seq(1:n())))
colnames(rus2.rus2) = paste0("rus2.", seq(1:ncol(rus2.rus2)))

rus2.fre3 <-
  read_csv("../data/embeddings/similarity_matrices/russian2.french3.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("rus2.", seq(1:n())))
colnames(rus2.fre3) = paste0("fre3.", seq(1:ncol(rus2.fre3)))

rus2.eng4 <-
  read_csv("../data/embeddings/similarity_matrices/russian2.english4.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("rus2.", seq(1:n())))
colnames(rus2.eng4) = paste0("eng4.", seq(1:ncol(rus2.eng4)))

fre3.eng1 <-
  read_csv("../data/embeddings/similarity_matrices/french3.english1.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("fre3.", seq(1:n())))
colnames(fre3.eng1) = paste0("eng1.", seq(1:ncol(fre3.eng1)))

fre3.rus2 <-
  read_csv("../data/embeddings/similarity_matrices/french3.russian2.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("fre3.", seq(1:n())))
colnames(fre3.rus2) = paste0("rus2.", seq(1:ncol(fre3.rus2)))

fre3.fre3 <-
  read_csv("../data/embeddings/similarity_matrices/french3.french3.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("fre3.", seq(1:n())))
colnames(fre3.fre3) = paste0("fre3.", seq(1:ncol(fre3.fre3)))

fre3.eng4 <-
  read_csv("../data/embeddings/similarity_matrices/french3.english4.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("fre3.", seq(1:n())))
colnames(fre3.eng4) = paste0("eng4.", seq(1:ncol(fre3.eng4)))

eng4.eng1 <-
  read_csv("../data/embeddings/similarity_matrices/english4.english1.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("eng4.", seq(1:n())))
colnames(eng4.eng1) = paste0("eng1.", seq(1:ncol(eng4.eng1)))

eng4.rus2 <-
  read_csv("../data/embeddings/similarity_matrices/english4.russian2.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("eng4.", seq(1:n())))
colnames(eng4.rus2) = paste0("rus2.", seq(1:ncol(eng4.rus2)))

eng4.fre3 <-
  read_csv("../data/embeddings/similarity_matrices/english4.french3.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("eng4.", seq(1:n())))
colnames(eng4.fre3) = paste0("fre3.", seq(1:ncol(eng4.fre3)))

eng4.eng4 <-
  read_csv("../data/embeddings/similarity_matrices/english4.english4.csv",
           show_col_type = F) |> 
  mutate(id_var = paste0("eng4.", seq(1:n())))
colnames(eng4.eng4) = paste0("eng4.", seq(1:ncol(eng4.eng4)))

## Melting ----


eng1.eng1 <- eng1.eng1 |> 
  melt(id.vars = "eng1.159", variable.name = "y") |>  # ncols
  rename(x = eng1.159) |> 
  mutate(x.num = as.numeric(gsub("eng1.","",x)),
         y.num = as.numeric(gsub("eng1.","",y)),
         x.story = "eng1", y.story = "eng1")


eng1.rus2 <- eng1.rus2 |> 
  melt(id.vars = "rus2.145", variable.name = "y") |> 
  rename(x = rus2.145) |> 
  mutate(x.num = as.numeric(gsub("eng1.","",x)),
         y.num = as.numeric(gsub("rus2.","",y)),
         x.story = "eng1", y.story = "rus2")

eng1.fre3 <- eng1.fre3 |> 
  melt(id.vars = "fre3.159", variable.name = "y") |> 
  rename(x = fre3.159) |> 
  mutate(x.num = as.numeric(gsub("eng1.","",x)),
         y.num = as.numeric(gsub("fre3.","",y)),
         x.story = "eng1", y.story = "fre3")

eng1.eng4 <- eng1.eng4 |> 
  melt(id.vars = "eng4.173", variable.name = "y") |> 
  rename(x = eng4.173) |> 
  mutate(x.num = as.numeric(gsub("eng1.","",x)),
         y.num = as.numeric(gsub("eng4.","",y)),
         x.story = "eng1", y.story = "eng4")

rus2.eng1 <- rus2.eng1 |> 
  melt(id.vars = "eng1.159", variable.name = "y") |> 
  rename(x = eng1.159) |> 
  mutate(x.num = as.numeric(gsub("rus2.","",x)),
         y.num = as.numeric(gsub("eng1.","",y)),
         x.story = "rus2", y.story = "eng1")

rus2.rus2 <- rus2.rus2 |> 
  melt(id.vars = "rus2.145", variable.name = "y") |> 
  rename(x = rus2.145) |> 
  mutate(x.num = as.numeric(gsub("rus2.","",x)),
         y.num = as.numeric(gsub("rus2.","",y)),
         x.story = "rus2", y.story = "rus2")

rus2.fre3 <- rus2.fre3 |> 
  melt(id.vars = "fre3.159", variable.name = "y") |> 
  rename(x = fre3.159) |> 
  mutate(x.num = as.numeric(gsub("rus2.","",x)),
         y.num = as.numeric(gsub("fre3.","",y)),
         x.story = "rus2", y.story = "fre3")

rus2.eng4 <- rus2.eng4 |> 
  melt(id.vars = "eng4.173", variable.name = "y") |> 
  rename(x = eng4.173) |> 
  mutate(x.num = as.numeric(gsub("rus2.","",x)),
         y.num = as.numeric(gsub("eng4.","",y)),
         x.story = "rus2", y.story = "eng4")

fre3.eng1 <- fre3.eng1 |> 
  melt(id.vars = "eng1.159", variable.name = "y") |> 
  rename(x = eng1.159) |> 
  mutate(x.num = as.numeric(gsub("fre3.","",x)),
         y.num = as.numeric(gsub("eng1.","",y)),
         x.story = "fre3", y.story = "eng1")

fre3.rus2 <- fre3.rus2 |> 
  melt(id.vars = "rus2.145", variable.name = "y") |> 
  rename(x = rus2.145) |> 
  mutate(x.num = as.numeric(gsub("fre3.","",x)),
         y.num = as.numeric(gsub("rus2.","",y)),
         x.story = "fre3", y.story = "rus2")

fre3.fre3 <- fre3.fre3 |> 
  melt(id.vars = "fre3.159", variable.name = "y") |> 
  rename(x = fre3.159) |> 
  mutate(x.num = as.numeric(gsub("fre3.","",x)),
         y.num = as.numeric(gsub("fre3.","",y)),
         x.story = "fre3", y.story = "fre3")

fre3.eng4 <- fre3.eng4 |> 
  melt(id.vars = "eng4.173", variable.name = "y") |> 
  rename(x = eng4.173) |> 
  mutate(x.num = as.numeric(gsub("fre3.","",x)),
         y.num = as.numeric(gsub("eng4.","",y)),
         x.story = "fre3", y.story = "eng4")

eng4.eng1 <- eng4.eng1 |> 
  melt(id.vars = "eng1.159", variable.name = "y") |> 
  rename(x = eng1.159) |> 
  mutate(x.num = as.numeric(gsub("eng4.","",x)),
         y.num = as.numeric(gsub("eng1.","",y)),
         x.story = "eng4", y.story = "eng1")

eng4.rus2 <- eng4.rus2 |> 
  melt(id.vars = "rus2.145", variable.name = "y") |> 
  rename(x = rus2.145) |> 
  mutate(x.num = as.numeric(gsub("eng4.","",x)),
         y.num = as.numeric(gsub("rus2.","",y)),
         x.story = "eng4", y.story = "rus2")

eng4.fre3 <- eng4.fre3 |> 
  melt(id.vars = "fre3.159", variable.name = "y") |> 
  rename(x = fre3.159) |> 
  mutate(x.num = as.numeric(gsub("eng4.","",x)),
         y.num = as.numeric(gsub("fre3.","",y)),
         x.story = "eng4", y.story = "fre3")

eng4.eng4 <- eng4.eng4 |> 
  melt(id.vars = "eng4.173", variable.name = "y") |> 
  rename(x = eng4.173) |> 
  mutate(x.num = as.numeric(gsub("eng4.","",x)),
         y.num = as.numeric(gsub("eng4.","",y)),
         x.story = "eng4", y.story = "eng4")

## Binding ----

full.melted.df <- rbind(eng1.eng1,
                        eng1.rus2,
                        eng1.fre3,
                        eng1.eng4,
                        rus2.eng1,
                        rus2.rus2,
                        rus2.fre3,
                        rus2.eng4,
                        fre3.eng1,
                        fre3.rus2,
                        fre3.fre3,
                        fre3.eng4,
                        eng4.eng1,
                        eng4.rus2,
                        eng4.fre3,
                        eng4.eng4)

# full.melted.df <- full.melted.df |> 
#   group_by(x.story) |> 
#   summarize(x.max = max(x.num)) |> 
#   right_join(full.melted.df, by = "x.story") |> 
#   mutate(x.prop = x.num / x.max)
# 
# full.melted.df <- full.melted.df |> 
#   group_by(y.story) |> 
#   summarize(y.max = max(y.num)) |> 
#   right_join(full.melted.df, by = "y.story") |> 
#   mutate(y.prop = y.num / y.max)

full.melted.df <- full.melted.df |> 
  group_by(x.story, y.story) |> 
  mutate(x.max = max(x.num),
         y.max = max(y.num),
         x.num.prop = x.num/x.max,
         y.num.prop = y.num/y.max)



## Plotting ----


grid.plot <- full.melted.df |> 
  ggplot(aes(x = x.num, y = y.num, fill = value))+
  geom_tile()+
  facet_wrap(factor(x.story,
                    levels = c("eng1","rus2","fre3","eng4"))~factor(y.story,levels = c("eng1","rus2","fre3","eng4")), 
             scales = "free", ncol = 4)+
  scale_fill_distiller(type = "seq",
                    name = "Similarity")+
  theme_bw()

grid.plot

ggsave(grid.plot, 
       path = "../figures/",
       filename = "full_mega_plot.png",units = "in", 
       width = 15, height = 15, dpi = 450)


# Chunk plot
num2lang <- function(x){
  x = as.numeric(x)
  if (x == 1){
    return("eng1")
  } else if(x == 2){
    return("rus2")
  } else if(x == 3){
    return("fre3")
  } else if(x == 4){
    return("eng4")
  } else {
    return("null")
  }
}

# x as true lang, y as pred lang

chunk_melted <- chunk_melted |> 
  rowwise() |> 
  mutate(x.id = paste(num2lang(x.story), x.chunk, sep = "."),
         y.id = paste(num2lang(y.story), y.chunk, sep = ".")) |> 
  group_by(x.story, y.story) |> 
  mutate(x.max = max(x.num),
         y.max = max(y.num),
         x.num.prop = x.num/x.max,
         y.num.prop = y.num/y.max) |> 
  select(-c(x.max, y.max))

grid.levels = c(paste("eng1", rep(1:5),sep = "."),
                paste("rus2", rep(1:5),sep = "."),
                paste("fre3", rep(1:5),sep = "."),
                paste("eng4", rep(1:5),sep = "."))

strip <- strip_themed(background_x = elem_list_rect(fill = c(rep("lightblue", 5),
                                                             rep("red",5),
                                                             rep("darkgreen",5),
                                                             rep("darkorange",5)
                                                             )
                                                    ),
                      background_y = elem_list_rect(fill = c(rep("lightblue", 5),
                                                             rep("red",5),
                                                             rep("darkgreen",5),
                                                             rep("darkorange",5)
                                                             )
                                                    )
                      )


chunk_plot <- ggplot(chunk_melted, aes(x = x.num, y = y.num, fill = value))+
  geom_tile()+
  xlab("true lang")+
  ylab("pred lang")+
  facet_grid2(factor(x.id, levels = grid.levels) ~ factor(y.id, levels = grid.levels),
              strip = strip)+
  scale_fill_distiller(type = "seq",
                       name = "Similarity")+
  theme_bw()
  
chunk_plot

ggsave(chunk_plot, 
       path = "../figures/",
       filename = "karma.chunk-similarities.png",units = "in", 
       width = 30, height = 30)

# # Read in the stories
# all.clean.split <-
#   read_csv("../data/text_split/all.clean.split_V2.csv",
#            show_col_type = F)
# eng1.clean.split <- all.clean.split |> filter(story == 1)
# rus2.clean.split <- all.clean.split |> filter(story == 2)
# fre3.clean.split <- all.clean.split |> filter(story == 3)
# eng4.clean.split <- all.clean.split |> filter(story == 4)


## Story reconstruction ----



# Not working fully at the moment?
# reconstruct_quick <- function(temp_melted){
#   temp_melted |> 
#     group_by(x) |> 
#     mutate(max.sim = max(value)) |> 
#     ungroup() |> 
#     filter(value == max.sim)
# }

reconstruct_chunk <- function(temp_melted){
  temp_melted |> 
    group_by(x.num, x.id, y.id) |> # x as source, y as reconstruction targets
    mutate(max.sim = max(value)) |> 
    ungroup() |> 
    filter(value == max.sim)
}

chunk_scores <- chunk_melted |> 
  reconstruct_chunk() |> 
  mutate(num.prop.diff = y.num.prop - x.num.prop,
         num.prop.diff.abs = abs(num.prop.diff))

reconstruct_full <- function(temp_melted){
  temp_melted |> 
    group_by(x, x.story, y.story) |> # x as source, y as reconstruction targets
    mutate(max.sim = max(value)) |> 
    ungroup() |> 
    filter(value == max.sim)
}

test_full <- full.melted.df |> 
  reconstruct_full() |> 
  mutate(num.prop.diff = y.num.prop - x.num.prop,
         num.prop.diff.abs = abs(num.prop.diff))



ggplot(test_full, aes(x = x.num.prop, y = y.num.prop))+
  # geom_point(aes(size = as.numeric(max.sim)))+
  geom_line()+
  theme(legend.position = "none")+
  facet_grid(factor(x.story, levels = c("eng1","rus2","fre3","eng4")) ~ 
               factor(y.story, levels = c("eng1","rus2","fre3","eng4")))+
  xlab("Source sentence (prop.)")+
  ylab("Reconstruction sentence (prop.)")

ggsave(path = "../figures/",
       filename = "karma.full-reconstruction.png",units = "in", 
       width = 10, height = 10)

ggplot(chunk_scores, aes(x = x.num.prop, y = y.num.prop))+
  geom_line()+
  xlab("Source sentence (prop.)")+
  ylab("Reconstruction sentence (prop.)")+
  facet_grid2(factor(x.id, levels = grid.levels) ~ factor(y.id, levels = grid.levels),
              strip = strip)+
  theme_bw()

ggsave(path = "../figures/",
       filename = "karma.chunk-reconstruction.png",units = "in", 
       width = 30, height = 30)


chunk_scores <- chunk_scores |> 
  mutate(translation_steps = abs(x.story - y.story),
         chunk_steps = abs(x.chunk - y.chunk),
         different_chunks = x.chunk == y.chunk)


reconstruction_chunk_scores <- chunk_scores |> 
  group_by(x.id, y.id) |> 
  summarize(rscore.pred.diff = mean(num.prop.diff),
            rscore.pred.diff.abs = mean(num.prop.diff.abs),
            rscore.avg.cos = mean(max.sim),
            rscore.avg.cos.med = median(max.sim)) |> 
  ungroup()

chunk_scores <- chunk_scores |> 
  left_join(reconstruction_chunk_scores, by = c("x.id","y.id"))



translation_steps <- data.frame(x.story = c("eng1","eng1","eng1","eng1",
                                            "rus2","rus2","rus2","rus2",
                                            "fre3","fre3","fre3","fre3",
                                            "eng4","eng4","eng4","eng4"), 
                                y.story = c("eng1","rus2","fre3","eng4",
                                            "eng1","rus2","fre3","eng4",
                                            "eng1","rus2","fre3","eng4",
                                            "eng1","rus2","fre3","eng4"), 
                                translation_steps = c(0,1,2,3,
                                                      1,0,1,2,
                                                      2,1,0,1,
                                                      3,2,1,0))

test_full <- test_full |> 
  left_join(translation_steps, by = c("x.story","y.story"))

reconstruction_scores <- test_full |> 
  group_by(x.story, y.story) |> 
  summarize(rscore.pred.diff = mean(num.prop.diff),
            rscore.pred.diff.abs = mean(num.prop.diff.abs),
            rscore.avg.cos = mean(max.sim),
            rscore.avg.cos.med = median(max.sim)) |> 
  ungroup()

test_full <- test_full |> 
  left_join(reconstruction_scores, by = c("x.story","y.story"))

# x.story = true lang
# x.num.prop = true_sentence_order (normalized)
# y.story = pred lang
# y.num.prop = pred_order (normalized)
# max.sim = sentence_similarity (individual)
# num.prop.diff = prediction_diff
# translation_steps (per story)
# reconstruction_score (average prediction difference)
# reconstruction_score (average cosine similarity)

## todo
# dtw (per story)


write_csv(test_full, "../data/processed_data/karma_reconstruction_df.csv")
write_csv(chunk_scores, "../data/processed_data/karma_chunk_reconstruction_df.csv")



