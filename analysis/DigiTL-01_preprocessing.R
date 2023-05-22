library(tidyverse)
library(here)
setwd(here())
library(tokenizers)
library(janitor)


## Load in the files ----
eng1 <- read_file("../data/Karma texts/final/English_1_1894.final.txt")
rus2 <- read_file("../data/Karma texts/final/Russian_2_1894.final.txt")
fre3 <- read_file("../data/Karma texts/final/French_3_1895.final.txt")
eng4 <- read_file("../data/Karma texts/final/English_4_1896.final.txt")

## Remove carriage returns ----

eng1.clean <- eng1 |> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") 

rus2.clean <- rus2 |> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") 

fre3.clean <- fre3 |> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") 

eng4.clean <- eng4  |>  
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") 

## Split the sentences ----

eng1.clean.split <- eng1.clean |> 
  tokenize_sentences() |> 
  unlist() 
eng1.clean.split <- tibble(story = 1, lang = "eng", sentences = eng1.clean.split) 

rus2.clean.split <- rus2.clean |> 
  tokenize_sentences() |> 
  unlist() 
rus2.clean.split <- tibble(story = 2, lang = "rus", sentences = rus2.clean.split)

fre3.clean.split <- fre3.clean |> 
  tokenize_sentences() |> 
  unlist() 
fre3.clean.split <- tibble(story = 3, lang = "fre", sentences = fre3.clean.split)

eng4.clean.split <- eng4.clean |> 
  tokenize_sentences() |> 
  unlist()
eng4.clean.split <- tibble(story = 4, lang = "eng", sentences = eng4.clean.split)

## Load in the chunks ----

#Eng1
eng1.1 <- read_file("../data/Karma texts/final.split/English_1_1894.split-1.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()
eng1.2 <- read_file("../data/Karma texts/final.split/English_1_1894.split-2.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()
eng1.3 <- read_file("../data/Karma texts/final.split/English_1_1894.split-3.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()
eng1.4 <- read_file("../data/Karma texts/final.split/English_1_1894.split-4.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()
eng1.5 <- read_file("../data/Karma texts/final.split/English_1_1894.split-5.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()

eng1.1 <- tibble(story = 1,
                 chunk = 1,
                 lang = "eng",
                 sentences = eng1.1) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
eng1.2 <- tibble(story = 1,
                 chunk = 2,
                 lang = "eng",
                 sentences = eng1.2) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
eng1.3 <- tibble(story = 1,
                 chunk = 3,
                 lang = "eng",
                 sentences = eng1.3) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
eng1.4 <- tibble(story = 1,
                 chunk = 4,
                 lang = "eng",
                 sentences = eng1.4) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
eng1.5 <- tibble(story = 1,
                 chunk = 5,
                 lang = "eng",
                 sentences = eng1.5) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())

# Rus2
rus2.1 <- read_file("../data/Karma texts/final.split/Russian_2_1894.split-1.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()
rus2.2 <- read_file("../data/Karma texts/final.split/Russian_2_1894.split-2.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()
rus2.3 <- read_file("../data/Karma texts/final.split/Russian_2_1894.split-3.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()
rus2.4 <- read_file("../data/Karma texts/final.split/Russian_2_1894.split-4.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()
rus2.5 <- read_file("../data/Karma texts/final.split/Russian_2_1894.split-5.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()

rus2.1 <- tibble(story = 2,
                 chunk = 1,
                 lang = "rus",
                 sentences = rus2.1) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
rus2.2 <- tibble(story = 2,
                 chunk = 2,
                 lang = "rus",
                 sentences = rus2.2) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
rus2.3 <- tibble(story = 2,
                 chunk = 3,
                 lang = "rus",
                 sentences = rus2.3) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
rus2.4 <- tibble(story = 2,
                 chunk = 4,
                 lang = "rus",
                 sentences = rus2.4) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
rus2.5 <- tibble(story = 2,
                 chunk = 5,
                 lang = "rus",
                 sentences = rus2.5) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())


# Fre3
fre3.1 <- read_file("../data/Karma texts/final.split/French_3_1895.split-1.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()
fre3.2 <- read_file("../data/Karma texts/final.split/French_3_1895.split-2.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()
fre3.3 <- read_file("../data/Karma texts/final.split/French_3_1895.split-3.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()
fre3.4 <- read_file("../data/Karma texts/final.split/French_3_1895.split-4.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()
fre3.5 <- read_file("../data/Karma texts/final.split/French_3_1895.split-5.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()

fre3.1 <- tibble(story = 3,
                 chunk = 1,
                 lang = "fre",
                 sentences = fre3.1) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
fre3.2 <- tibble(story = 3,
                 chunk = 2,
                 lang = "fre",
                 sentences = fre3.2) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
fre3.3 <- tibble(story = 3,
                 chunk = 3,
                 lang = "fre",
                 sentences = fre3.3) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
fre3.4 <- tibble(story = 3,
                 chunk = 4,
                 lang = "fre",
                 sentences = fre3.4) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
fre3.5 <- tibble(story = 3,
                 chunk = 5,
                 lang = "fre",
                 sentences = fre3.5) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())


#Eng4
eng4.1 <- read_file("../data/Karma texts/final.split/English_4_1896.split-1.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()

eng4.2 <- read_file("../data/Karma texts/final.split/English_4_1896.split-2.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()

eng4.3 <- read_file("../data/Karma texts/final.split/English_4_1896.split-3.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()

eng4.4 <- read_file("../data/Karma texts/final.split/English_4_1896.split-4.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()

eng4.5 <- read_file("../data/Karma texts/final.split/English_4_1896.split-5.txt")|> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") |> 
  tokenize_sentences() |> 
  unlist()

eng4.1 <- tibble(story = 4,
                 chunk = 1,
                 lang = "eng",
                 sentences = eng4.1) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
eng4.2 <- tibble(story = 4,
                 chunk = 2,
                 lang = "eng",
                 sentences = eng4.2) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
eng4.3 <- tibble(story = 4,
                 chunk = 3,
                 lang = "eng",
                 sentences = eng4.3) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
eng4.4 <- tibble(story = 4,
                 chunk = 4,
                 lang = "eng",
                 sentences = eng4.4) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())
eng4.5 <- tibble(story = 4,
                 chunk = 5,
                 lang = "eng",
                 sentences = eng4.5) |> 
  mutate(chunk_sent_num = 1:n(),
         chunk_prop_num = chunk_sent_num/n())


all.clean.chunk.split <- rbind(eng1.1, eng1.2, eng1.3, eng1.4, eng1.5,
                               rus2.1, rus2.2, rus2.3, rus2.4, rus2.5,
                               fre3.1, fre3.2, fre3.3, fre3.4, fre3.5,
                               eng4.1, eng4.2, eng4.3, eng4.4, eng4.5) |> 
  group_by(story) |> 
  mutate(sent_num = 1:n(),
         prop_num = sent_num/n()) |> 
  ungroup()



all.clean.split <- bind_rows(eng1.clean.split, 
                             rus2.clean.split, 
                             fre3.clean.split,
                             eng4.clean.split) |> 
  group_by(story) |> 
  mutate(sentence_num = 1:n()) |> 
  ungroup()

write_csv(all.clean.split, "../data/text_split/all.clean.split_V2.csv")

write_csv(all.clean.chunk.split, "../data/text_split/all.clean.split_VChunk.csv")










## Moved to another script ----
# 
# ## M-BERT in Colab ----
# 
# # https://colab.research.google.com/drive/1HIGBbRT0cJmpujjl7mhkSM-V_WTKgRxo
# 
# ## Read in M-BERT embeddings ----
# 
# eng1.embeds <- read_csv("../data/embeddings/mbert/s1_embeds_raw.csv", show_col_type = F) |> 
#   clean_names() |> 
#   mutate(sent_num = 1:n()) 
# rus2.embeds <- read_csv("../data/embeddings/mbert/s2_embeds_raw.csv", show_col_type = F)|> 
#   clean_names() |> 
#   mutate(sent_num = 1:n()) 
# fre3.embeds <- read_csv("../data/embeddings/mbert/s3_embeds_raw.csv", show_col_type = F)|> 
#   clean_names() |> 
#   mutate(sent_num = 1:n()) 
# eng4.embeds <- read_csv("../data/embeddings/mbert/s4_embeds_raw.csv", show_col_type = F)|> 
#   clean_names() |> 
#   mutate(sent_num = 1:n()) 
# 
# # Join with sentences.
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
# 
# all.clean.split_wEmb <- rbind(eng1.clean.split, 
#                             rus2.clean.split, 
#                             fre3.clean.split,
#                             eng4.clean.split)
# set.seed(1234)
# 
# ## tSNE portion ----
# embeds.tsne <- all.clean.split_wEmb |>
#   select(starts_with("x")) |>
#   Rtsne(dims = 2, pca = F, perplexity = 30, theta = 0.5, check_duplicates = F)
# 
# embeds.tsne_df <- embeds.tsne$Y |>
#   as.data.frame() |>
#   rename(tSNE1 = "V1", tSNE2 = "V2") |>
#   mutate(ID2 = row_number())
# 
# 
# embeds3 <- all.clean.split_wEmb |>
#   mutate(ID2 = row_number()) |>
#   left_join(embeds.tsne_df, by = "ID2") |>
#   select(-ID2) |>
#   group_by(story) |>
#   mutate(sent_num.prop = sent_num/n()) |>
#   ungroup() |>
#   select(story, lang, sentences, sent_num, sent_num.prop, tSNE1, tSNE2, everything())
# # 
# # 
# # # tSNE graphing
# # 
# # embeds3 |>
# #   mutate(graph_label = paste(lang, story)) |>
# #   ggplot(aes(x=tSNE1, y = tSNE2, color=factor(graph_label), group=graph_label)) +
# #   geom_path() +
# #   facet_wrap(~graph_label) +
# #   scale_color_manual(values = wes_palette("Darjeeling1"), name = "Story") +
# #   theme_minimal() +
# #   ggtitle("Story paths")
# # 
# # a <- embeds3 |>  
# #   mutate(graph_label = paste(lang, story)) |> 
# #   ggplot(aes(x=sent_num.prop, y = tSNE1, color=factor(graph_label), 
# #              group=graph_label)) +
# #   geom_path() +
# #   # facet_wrap(~graph_label) + 
# #   scale_color_manual(values = wes_palette("Darjeeling1"), name = "Story") +
# #   theme_minimal() +
# #   theme(legend.position = "none")+
# #   ggtitle("Story paths", subtitle = "tSNE1 only")+
# #   xlab("Proportional sentence number (within the story)")
# # 
# # b <- embeds3 |>  
# #   mutate(graph_label = paste(lang, story)) |> 
# #   ggplot(aes(x=sent_num.prop, y = tSNE2, color=factor(graph_label), 
# #              group=graph_label)) +
# #   geom_path() +
# #   # facet_wrap(~graph_label) + 
# #   scale_color_manual(values = wes_palette("Darjeeling1"), name = "Story") +
# #   theme_minimal() +
# #   ggtitle("Story paths", subtitle = "tSNE2 only")+
# #   xlab("Proportional sentence number (within the story)")
# # 
# # a / b
# # 
# # 
# # ## Smoothing ----
# # before = 30
# # after = 0
# # 
# # xtabs(~story, embeds3)
# # 
# # embeds4 <- embeds3 |>  
# #   group_by(story) |> 
# #   mutate(across(tSNE1:tSNE2, ~ slide_dbl(.x, ~mean(.x), 
# #                                          .before = before, 
# #                                          .after = after, 
# #                                          .complete = T)))
# # 
# # paths.1 <- embeds4 |>  
# #   mutate(graph_label = paste(story, lang)) |> 
# #   ggplot(aes(x=tSNE1, y = tSNE2, group=graph_label)) +
# #   geom_path(color="grey50", size=1) +
# #   geom_point(aes(color=sent_num), size=1) +
# #   facet_grid(~graph_label) + 
# #   scale_color_gradient2(guide='none', midpoint = .5, mid="grey50") + 
# #   scale_alpha(guide=F) + 
# #   theme_bw(base_size = 15)
# # 
# # paths.A <- embeds4 |>  
# #   mutate(graph_label = paste(story, lang)) |> 
# #   ggplot(aes(x=tSNE1, y = tSNE2, color=as.factor(story), group=graph_label)) +
# #   geom_path(size=1) +
# #   geom_point(size=1) +
# #   scale_color_discrete(guide="none") + 
# #   scale_alpha(guide="none") + 
# #   theme_bw(base_size = 15)
# # 
# # paths.1 + paths.A
# # 
# # 
# # # Normalized smoothing
# # 
# # normalized_bin_size = .05
# # 
# # embeds_normalized_smooth <- embeds3 |>  
# #   group_by(story) |> 
# #   mutate(sent_num_bin_n = normalized_bin_size * 
# #            ceiling((sent_num / normalized_bin_size))) %>%
# #   group_by(story, sent_num_bin_n) %>%
# #   summarize(across(tSNE1:tSNE2, ~ mean(.x)), 
# #             n=n(),
# #             .groups="drop")
# # 
# # 
# # embeds_normalized_smooth |>  
# #   ggplot(aes(x=tSNE1, y = tSNE2, group=story, color=sent_num_bin_n)) +
# #   geom_path(size=1) +
# #   geom_point(size=2) +
# #   facet_grid(~story) + 
# #   scale_color_gradient2(guide=F, midpoint = .5, mid="grey50") + 
# #   scale_alpha(guide=F) + 
# #   theme_bw(base_size = 15)
# 
# 
# ## Sentence matching: ENG4 as ground truth ----
# 
# ## UPDATE: use the matmult from colab
# # 
# # story.reconstruction <- function(storyA, storyB){
# #   reconstruction.tmp <- data.frame()
# #   for (sent_i in 1:nrow(storyA)){
# #     current_sentence <- storyA[sent_i,]
# #     current_embedding <- current_sentence |> 
# #       select(starts_with("x")) |> 
# #       as.numeric()
# #     current_info <- current_sentence |> 
# #       select(c(story, lang, sentences, sent_num, sent_num.prop))
# #     
# #     max.similarity <- 0
# #     for (sent_j in 1:nrow(storyB)) {
# #       current_sentence.2 <- storyB[sent_j,]
# #       current_embedding.2 <- current_sentence.2 |> 
# #         select(starts_with("x")) |> 
# #         as.numeric()
# #       
# #       similarity <- cosine(current_embedding, current_embedding.2)[1]
# #       if (similarity >= max.similarity) {
# #         max.similarity <- similarity
# #         max.sentence <- current_sentence.2 |> 
# #           select(c(story, lang, sentences, sent_num, sent_num.prop)) |> 
# #           rename(story.1 = story, lang.1 = lang, sentences.1 = sentences,
# #                  sent_num.1 = sent_num, 
# #                  sent_num.prop.1 = sent_num.prop) |> 
# #           mutate(similarity = similarity)
# #       }
# #     }
# #     reconstruction.tmp <- reconstruction.tmp |> 
# #       rbind(cbind(current_info, max.sentence))
# #   }
# #   return(reconstruction.tmp)
# # }
# # 
# # 
# # eng1.data <- embeds3 |> 
# #   filter(story == 1)
# # rus2.data <- embeds3 |> 
# #   filter(story == 2)
# # fre3.data <- embeds3 |> 
# #   filter(story == 3)
# # eng4.data <- embeds3 |> 
# #   filter(story == 4)
# # 
# # reconstruction <- story.reconstruction(eng4.data, eng1.data) #Reconstruct story1 using story4 as ground truth
# # 
# # rc.plot1 <- ggplot(reconstruction, aes(x = sent_num.prop, y = sent_num.prop.1))+
# #   geom_line()+
# #   theme_bw()+
# #   xlab("ENG4 Sentence number (prop.)")+
# #   ylab("ENG1 Sentence number (prop.)")+
# #   ggtitle("ENG4 as ground truth")+
# #   geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5)
# # 
# # ## Sentence matching: ENG1 as ground truth ----
# # tictoc::tic()
# # reconstruction2 <- story.reconstruction(eng1.data, eng4.data) #Reconstruct story4 using story1 as ground truth
# # tictoc::toc()
# # 
# # rc.plot2 <- ggplot(reconstruction2, aes(x = sent_num.prop, y = sent_num.prop.1))+
# #   geom_line()+
# #   theme_bw()+
# #   xlab("ENG1 Sentence number (prop.)")+
# #   ylab("ENG4 Sentence number (prop.)")+
# #   ggtitle("ENG1 as ground truth")+
# #   geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5)
# # 
# # rc.plot1 + rc.plot2 +
# #   plot_annotation(title = "Reconstructed stories")
# # 
# # ## Reconstruct Eng1 from Rus2 ----
# # tictoc::tic()
# # reconstruction.12 <- story.reconstruction(rus2.data, eng1.data) #Reconstruct story1 using story2 as ground truth
# # tictoc::toc()
# # 
# # rc.plot12 <- ggplot(reconstruction.12, aes(x = sent_num.prop, y = sent_num.prop.1))+
# #   geom_line(size = 1, color = "red")+
# #   theme_bw()+
# #   xlab("Rus2 proportional sentence")+
# #   ylab("Eng1 proportional sentence")+
# #   ggtitle("Rus2 as ground truth")+
# #   geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5)
# # 
# # 
# # ## Reconstruct Rus2 from Fre3
# # tictoc::tic()
# # reconstruction.23 <- story.reconstruction(fre3.data, rus2.data) #Reconstruct story2 using story3 as ground truth
# # tictoc::toc()
# # 
# # rc.plot23 <- ggplot(reconstruction.23, aes(x = sent_num.prop, y = sent_num.prop.1))+
# #   geom_line(size = 1, color = "blue")+
# #   theme_bw()+
# #   xlab("Fre3 proportional sentence")+
# #   ylab("Rus2 proportional sentence")+
# #   ggtitle("Fre3 as ground truth")+
# #   geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5)
# # 
# # 
# # ## Reconstruct Fre3 from Eng4
# # tictoc::tic()
# # reconstruction.34 <- story.reconstruction(eng4.data, fre3.data) #Reconstruct story3 using story4 as ground truth
# # tictoc::toc()
# # 
# # rc.plot34 <- ggplot(reconstruction.34, aes(x = sent_num.prop, y = sent_num.prop.1))+
# #   geom_line(size = 1, color = "darkgreen")+
# #   theme_bw()+
# #   xlab("Eng4 proportional sentence")+
# #   ylab("Fre3 proportional sentence")+
# #   ggtitle("Eng4 as ground truth")+
# #   geom_abline(intercept = 0, slope = 1, linetype = "dashed", alpha = 0.5)
# # 
# # rc.plot12 + rc.plot23 + rc.plot34 +
# #   plot_annotation(title = "Iterative reconstruction")
# 
# 
# 
# ## UMAP part ----
# 
# umap.embeds <- read_csv("../data/embeddings/umap_embeds.csv", show_col_type = F) |> 
#   select(-story) |> 
#   cbind(embeds3 |> select(-c(starts_with("x"))))
# 
# umap.embeds |>  
#   mutate(graph_label = paste(story, lang)) |> 
#   ggplot(aes(x=umap.x, y = umap.y, color=factor(graph_label), group=graph_label)) +
#   geom_path() +
#   facet_wrap(~graph_label) + 
#   scale_color_manual(values = wes_palette("Darjeeling1"), name = "Story") +
#   theme_minimal() +
#   ggtitle("Story paths")
# 
# a <- umap.embeds |>  
#   mutate(graph_label = paste(story, lang)) |> 
#   ggplot(aes(x=sent_num.prop, y = umap.x, color=factor(graph_label), 
#              group=graph_label)) +
#   geom_path() +
#   scale_color_manual(values = c("red", "darkgreen","blue","lightblue"), name = "Story") +
#   theme_minimal() +
#   theme(legend.position = "none")+
#   ggtitle("Story paths", subtitle = "umap.x only")+
#   xlab("Proportional sentence number (within the story)")
# 
# b <- umap.embeds |>  
#   mutate(graph_label = paste(story, lang)) |> 
#   ggplot(aes(x=sent_num.prop, y = umap.y, color=factor(graph_label), 
#              group=graph_label)) +
#   geom_path() +
#   # facet_wrap(~graph_label) + 
#   scale_color_manual(values = c("red", "darkgreen","blue","lightblue"), name = "Story") +
#   theme_minimal() +
#   ggtitle("Story paths", subtitle = "umap.y only")+
#   xlab("Proportional sentence number (within the story)")
# 
# a / b
# 
# 
# ## UMAP: Smoothing ----
# before = 30
# after = 0
# 
# xtabs(~story, umap.embeds)
# 
# umap.embeds.4 <- umap.embeds |>  
#   group_by(story) |> 
#   mutate(across(umap.x:umap.y, ~ slide_dbl(.x, ~mean(.x), 
#                                          .before = before, 
#                                          .after = after, 
#                                          .complete = T)))
# 
# paths.1 <- umap.embeds.4 |>  
#   mutate(graph_label = paste(story, lang)) |> 
#   ggplot(aes(x=umap.x, y = umap.y, group=graph_label)) +
#   geom_path(color="grey50", linewidth =1) +
#   geom_point(aes(color=factor(sent_num.prop)), size=2) +
#   facet_grid(~graph_label) + 
#   # scale_color_gradient2(guide='none', midpoint = .5, mid="grey50") + 
#   scale_color_brewer(type = "qual", palette = 3)+
#   scale_alpha(guide=F) + 
#   theme_bw(base_size = 15)+
#   ggtitle("UMAP story paths")+
#   theme(legend.position = "none")
# 
# paths.1
# 
# paths.A <- umap.embeds.4 |>  
#   mutate(graph_label = paste(story, lang)) |> 
#   ggplot(aes(x=umap.x, y = umap.y, color=as.factor(story), 
#              group=graph_label)) +
#   geom_path(size=1) +
#   geom_point(size=3, aes(fill=sent_num.prop)) +
#   scale_color_brewer(type = "qual", palette = 3)+
#   scale_shape_manual(values = c(21, 23, 24, 22), name = "Story")+
#   scale_alpha(guide="none") + 
#   theme_bw(base_size = 15)+
#   ggtitle("Overlayed story paths")
# 
# paths.A
# 
# 
# # Normalized smoothing
# 
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
#   ggplot(aes(x=umap.x, y = umap.y, group=story, color=sent_num_bin_n)) +
#   geom_path(size=1) +
#   geom_point(size=2) +
#   facet_grid(~story) + 
#   scale_color_viridis_b()+
#   # scale_color_gradient2(guide=F, midpoint = .5, mid="grey50") + 
#   scale_alpha(guide=F) + 
#   theme_bw(base_size = 15)+
#   ggtitle("Normalized smoothing UMAP")
# 
# 
# ## DTW ----
# 
# umap.embeds |> colnames()
# 
# 
# ## ENG1, RUS2 ----
# eng1.rus2 <- dtw(umap.embeds |> filter(story == 1) |> 
#                    select(umap.x, umap.y),
#                  umap.embeds |> filter(story == 2) |> 
#                    select(umap.x, umap.y),
#                  keep = TRUE)
# eng1.rus2.distance <- eng1.rus2$normalizedDistance
# 
# ## ENG1, FRE3 ----
# eng1.fre3 <- dtw(umap.embeds |> filter(story == 1) |> 
#                    select(umap.x, umap.y),
#                  umap.embeds |> filter(story == 3) |> 
#                    select(umap.x, umap.y),
#                  keep = TRUE)
# eng1.fre3.distance <- eng1.fre3$normalizedDistance
# 
# ## ENG1, ENG4 ----
# eng1.eng4 <- dtw(umap.embeds |> filter(story == 1) |> 
#                    select(umap.x, umap.y),
#                  umap.embeds |> filter(story == 4) |> 
#                    select(umap.x, umap.y),
#                  keep = TRUE)
# eng1.eng4.distance <- eng1.eng4$normalizedDistance
# 
# ## RUS2, ENG1 ----
# 
# rus2.eng1 <- dtw(umap.embeds |> filter(story == 2) |> 
#                    select(umap.x, umap.y),
#                  umap.embeds |> filter(story == 1) |> 
#                    select(umap.x, umap.y),
#                  keep = TRUE)
# rus2.eng1.distance <- rus2.eng1$normalizedDistance
# 
# ## RUS2, FRE3 ----
# 
# rus2.fre3 <- dtw(umap.embeds |> filter(story == 2) |> 
#                    select(umap.x, umap.y),
#                  umap.embeds |> filter(story == 3) |> 
#                    select(umap.x, umap.y),
#                  keep = TRUE)
# rus2.fre3.distance <- rus2.fre3$normalizedDistance
# 
# ## RUS2, ENG4 ----
# 
# rus2.eng4 <- dtw(umap.embeds |> filter(story == 2) |> 
#                    select(umap.x, umap.y),
#                  umap.embeds |> filter(story == 4) |> 
#                    select(umap.x, umap.y),
#                  keep = TRUE)
# rus2.eng4.distance <- rus2.eng4$normalizedDistance
# 
# ## FRE3, ENG1 ----
# fre3.eng1 <- dtw(umap.embeds |> filter(story == 3) |> 
#                    select(umap.x, umap.y),
#                  umap.embeds |> filter(story == 1) |> 
#                    select(umap.x, umap.y),
#                  keep = TRUE)
# fre3.eng1.distance <- fre3.eng1$normalizedDistance
# 
# ## FRE3, RUS2 ----
# fre3.rus2 <- dtw(umap.embeds |> filter(story == 3) |> 
#                    select(umap.x, umap.y),
#                  umap.embeds |> filter(story == 2) |> 
#                    select(umap.x, umap.y),
#                  keep = TRUE)
# fre3.rus2.distance <- fre3.rus2$normalizedDistance
# 
# ## FRE3, ENG4 ----
# fre3.eng4 <- dtw(umap.embeds |> filter(story == 3) |> 
#                    select(umap.x, umap.y),
#                  umap.embeds |> filter(story == 4) |> 
#                    select(umap.x, umap.y),
#                  keep = TRUE)
# fre3.eng4.distance <- fre3.eng4$normalizedDistance
# 
# ## ENG4, RUS2 ----
# eng4.rus2 <- dtw(umap.embeds |> filter(story == 4) |> 
#                    select(umap.x, umap.y),
#                  umap.embeds |> filter(story == 2) |> 
#                    select(umap.x, umap.y),
#                  keep = TRUE)
# eng4.rus2.distance <- eng4.rus2$normalizedDistance
# 
# ## ENG4, FRE3 ----
# eng4.fre3 <- dtw(umap.embeds |> filter(story == 4) |> 
#                    select(umap.x, umap.y),
#                  umap.embeds |> filter(story == 3) |> 
#                    select(umap.x, umap.y),
#                  keep = TRUE)
# eng4.fre3.distance <- eng4.fre3$normalizedDistance
# 
# ## ENG4, ENG1 ----
# eng4.eng1 <- dtw(umap.embeds |> filter(story == 4) |> 
#                    select(umap.x, umap.y),
#                  umap.embeds |> filter(story == 1) |> 
#                    select(umap.x, umap.y),
#                  keep = TRUE)
# eng4.eng1.distance <- eng4.eng1$normalizedDistance
# 
# ## Final heatmap ----
# 
# dtw.df <- data.frame(story = c("eng1", "rus2","fre3", "eng4"),
#                      eng1 = c(NA, rus2.eng1.distance, fre3.eng1.distance, eng4.eng1.distance),
#                      rus2 = c(eng1.rus2.distance, NA, fre3.rus2.distance, eng4.rus2.distance),
#                      fre3 = c(eng1.fre3.distance, rus2.fre3.distance, NA, eng4.fre3.distance),
#                      eng4 = c(eng1.eng4.distance, rus2.eng4.distance, fre3.eng4.distance, NA)) |> 
#   melt()
# 
# ggplot(dtw.df, aes(x = factor(story,
#                               levels = c("eng1", "rus2","fre3", "eng4")), 
#                    y = factor(variable,
#                               levels = c("eng1", "rus2","fre3", "eng4")), 
#                    fill = value))+
#   geom_tile()+
#   scale_fill_continuous(low = "darkgreen",
#                         high = "lightgreen",
#                         na.value = "transparent",
#                         name = "DTW\ndistance")+
#   xlab("Story")+
#   ylab("Story")+
#   theme_bw()+
#   geom_text(aes(label = round(value, digits = 3)))
# 
# 
# ## Plot DTW against translation distance, use color/shape to show that DTW increases as translation distance increases
# # Lexical distance: https://en.wikipedia.org/wiki/Lexical_similarity
# 
# lm.df <- data.frame(dtw.distance = c(eng1.rus2.distance,
#                                      eng1.fre3.distance,
#                                      eng1.eng4.distance,
#                                      rus2.fre3.distance,
#                                      rus2.eng4.distance,
#                                      fre3.eng4.distance),
#                     lexical.distance = c(0.24,
#                                          0.27,
#                                          1,
#                                          0,
#                                          0.24,
#                                          0.27),
#                     translation.steps = c(1,
#                                           2,
#                                           3,
#                                           1,
#                                           2,
#                                           1),
#                     label = c("e1r2",
#                               "e1f3",
#                               "e1e4",
#                               "r2f3",
#                               "r2e4",
#                               "f3e4"))
# 
# ggplot(lm.df, aes(x = translation.steps, y = dtw.distance))+
#   geom_smooth(method = "lm", se = F, color = "darkblue")+
#   geom_point(shape = 21, fill = "lightblue", color = "black")+
#   geom_label_repel(aes(label = label), min.segment.length = 0.1)+
#   theme_bw()+
#   stat_cor()
# 
# ggplot(lm.df, aes(x = lexical.distance, y = dtw.distance))+
#   geom_smooth(method = "lm", se = F, color = "darkblue")+
#   geom_point(shape = 21, fill = "lightblue", color = "black")+
#   geom_label_repel(aes(label = label), min.segment.length = 0.1)+
#   theme_bw()+
#   stat_cor()
# 
# ## Train lm
# 
# 
# lm.fit <- lm(dtw.distance ~ lexical.distance + translation.steps,
#                   data = lm.df)
# 
# summary(lm.fit)
# 
# lm.fit2 <- lm(dtw.distance ~ lexical.distance * translation.steps,
#              data = lm.df)
# 
# summary(lm.fit2)
# 
# 
# 
# 
# # Chunking
# 
# 
# 
# 
# # Heatmap of dtw
# # Pairwise linear regression for DTW normalized distance as a function of translation step size, same language (linguistic distance?)
# # dtw ~ translation_steps + linguistic_distance + plot_steps
# # DTW between (x, y) ~ "" + "" + distance (chunk 1 to chunk 2 vs chunk 1 to chunk 3)
# # BERT similarity between the chapters/chunks
# 
# 
# # Or, either manually / per cluster from sliding window, use those as the "texts" to increase n()
# 
# # Foote novelty
# # For a similarity matrix, run a kernel over the matrix across the diagonal
# # Compare the similarity to previous sentences, and dissimilarity to neighboring sentences
# 
# 
# 
# ## Wikipedia list of English words of french origin
# 
# ## Make a list of cut points / major scene changes and minor scene changes -- fine grain and coarse grained
# 

