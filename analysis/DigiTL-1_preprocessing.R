library(tidyverse)
library(here)
setwd(here())
library(tokenizers)
library(janitor)

# Functions ----

granularize.sentences <- function(grain.size, sentences){
  combined.sentences <- character(0)
  for (i in seq(1, length(sentences), by = grain.size)) {
    end.index <- min(i + grain.size - 1, 
                     length(sentences))
    combined.sentences <- c(combined.sentences, 
                            paste(sentences[i:end.index], 
                                  collapse = " "))
  }
  return(combined.sentences)
}

sliding.window <- function(window.size, sentences){
  true.window = window.size - 1
  combined.sentences <- character(0)
  
  for (ii in 1:(length(sentences)-true.window)){
    end.index <- min(ii + true.window, 
                     length(sentences))
    combined.sentences <- c(combined.sentences, 
                            paste(sentences[ii:end.index], 
                                  collapse = " "))
  }
  return(combined.sentences)
}

# Karma ----

## Load in the files ----
text1 <- read_file("../data/karma_texts/final/English_1_1894.final.txt")
text2 <- read_file("../data/karma_texts/final/Russian_2_1894.final.txt")
text3 <- read_file("../data/karma_texts/final/French_3_1895.final.txt")
text4 <- read_file("../data/karma_texts/final/English_4_1896.final.txt")

## Remove carriage returns ----

text1.clean <- text1 |> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") 

text2.clean <- text2 |> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") 

text3.clean <- text3 |> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") 

text4.clean <- text4  |>  
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") 

## Split the sentences ----

text1.clean.split <- text1.clean |> 
  tokenize_sentences() |> 
  unlist() 
text1.clean.split <- tibble(story = 1, lang = "english", sentences = text1.clean.split) 

text2.clean.split <- text2.clean |> 
  tokenize_sentences() |> 
  unlist() 
text2.clean.split <- tibble(story = 2, lang = "russian", sentences = text2.clean.split)

text3.clean.split <- text3.clean |> 
  tokenize_sentences() |> 
  unlist() 
text3.clean.split <- tibble(story = 3, lang = "french", sentences = text3.clean.split)

text4.clean.split <- text4.clean |> 
  tokenize_sentences() |> 
  unlist()
text4.clean.split <- tibble(story = 4, lang = "english", sentences = text4.clean.split)


all.clean.split <- bind_rows(text1.clean.split, 
                             text2.clean.split, 
                             text3.clean.split,
                             text4.clean.split) |> 
  group_by(story) |> 
  mutate(sentence_num = 1:n()) |> 
  ungroup()


## Saving the data ----

write_csv(all.clean.split, "../data/text_split/karma.clean.split.csv")

## Chunk version ----

# #Eng1
# eng1.1 <- read_file("../data/Karma texts/final.split/English_1_1894.split-1.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# eng1.2 <- read_file("../data/Karma texts/final.split/English_1_1894.split-2.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# eng1.3 <- read_file("../data/Karma texts/final.split/English_1_1894.split-3.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# eng1.4 <- read_file("../data/Karma texts/final.split/English_1_1894.split-4.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# eng1.5 <- read_file("../data/Karma texts/final.split/English_1_1894.split-5.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# 
# eng1.1 <- tibble(story = 1,
#                  chunk = 1,
#                  lang = "eng",
#                  sentences = eng1.1) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# eng1.2 <- tibble(story = 1,
#                  chunk = 2,
#                  lang = "eng",
#                  sentences = eng1.2) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# eng1.3 <- tibble(story = 1,
#                  chunk = 3,
#                  lang = "eng",
#                  sentences = eng1.3) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# eng1.4 <- tibble(story = 1,
#                  chunk = 4,
#                  lang = "eng",
#                  sentences = eng1.4) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# eng1.5 <- tibble(story = 1,
#                  chunk = 5,
#                  lang = "eng",
#                  sentences = eng1.5) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# 
# # Rus2
# rus2.1 <- read_file("../data/Karma texts/final.split/Russian_2_1894.split-1.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# rus2.2 <- read_file("../data/Karma texts/final.split/Russian_2_1894.split-2.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# rus2.3 <- read_file("../data/Karma texts/final.split/Russian_2_1894.split-3.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# rus2.4 <- read_file("../data/Karma texts/final.split/Russian_2_1894.split-4.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# rus2.5 <- read_file("../data/Karma texts/final.split/Russian_2_1894.split-5.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# 
# rus2.1 <- tibble(story = 2,
#                  chunk = 1,
#                  lang = "rus",
#                  sentences = rus2.1) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# rus2.2 <- tibble(story = 2,
#                  chunk = 2,
#                  lang = "rus",
#                  sentences = rus2.2) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# rus2.3 <- tibble(story = 2,
#                  chunk = 3,
#                  lang = "rus",
#                  sentences = rus2.3) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# rus2.4 <- tibble(story = 2,
#                  chunk = 4,
#                  lang = "rus",
#                  sentences = rus2.4) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# rus2.5 <- tibble(story = 2,
#                  chunk = 5,
#                  lang = "rus",
#                  sentences = rus2.5) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# 
# 
# # Fre3
# fre3.1 <- read_file("../data/Karma texts/final.split/French_3_1895.split-1.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# fre3.2 <- read_file("../data/Karma texts/final.split/French_3_1895.split-2.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# fre3.3 <- read_file("../data/Karma texts/final.split/French_3_1895.split-3.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# fre3.4 <- read_file("../data/Karma texts/final.split/French_3_1895.split-4.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# fre3.5 <- read_file("../data/Karma texts/final.split/French_3_1895.split-5.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# 
# fre3.1 <- tibble(story = 3,
#                  chunk = 1,
#                  lang = "fre",
#                  sentences = fre3.1) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# fre3.2 <- tibble(story = 3,
#                  chunk = 2,
#                  lang = "fre",
#                  sentences = fre3.2) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# fre3.3 <- tibble(story = 3,
#                  chunk = 3,
#                  lang = "fre",
#                  sentences = fre3.3) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# fre3.4 <- tibble(story = 3,
#                  chunk = 4,
#                  lang = "fre",
#                  sentences = fre3.4) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# fre3.5 <- tibble(story = 3,
#                  chunk = 5,
#                  lang = "fre",
#                  sentences = fre3.5) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# 
# 
# #Eng4
# eng4.1 <- read_file("../data/Karma texts/final.split/English_4_1896.split-1.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# 
# eng4.2 <- read_file("../data/Karma texts/final.split/English_4_1896.split-2.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# 
# eng4.3 <- read_file("../data/Karma texts/final.split/English_4_1896.split-3.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# 
# eng4.4 <- read_file("../data/Karma texts/final.split/English_4_1896.split-4.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# 
# eng4.5 <- read_file("../data/Karma texts/final.split/English_4_1896.split-5.txt")|> 
#   str_replace_all("[\r]", replacement = " ") |> 
#   str_replace_all("[\n]", replacement = " ") |> 
#   str_replace_all("- ", replacement = "") |>
#   str_replace_all("  ", replacement = " ") |> 
#   tokenize_sentences() |> 
#   unlist()
# 
# eng4.1 <- tibble(story = 4,
#                  chunk = 1,
#                  lang = "eng",
#                  sentences = eng4.1) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# eng4.2 <- tibble(story = 4,
#                  chunk = 2,
#                  lang = "eng",
#                  sentences = eng4.2) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# eng4.3 <- tibble(story = 4,
#                  chunk = 3,
#                  lang = "eng",
#                  sentences = eng4.3) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# eng4.4 <- tibble(story = 4,
#                  chunk = 4,
#                  lang = "eng",
#                  sentences = eng4.4) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# eng4.5 <- tibble(story = 4,
#                  chunk = 5,
#                  lang = "eng",
#                  sentences = eng4.5) |> 
#   mutate(chunk_sent_num = 1:n(),
#          chunk_prop_num = chunk_sent_num/n())
# 
# 

### Combining dfs ----

# all.clean.chunk.split <- rbind(eng1.1, eng1.2, eng1.3, eng1.4, eng1.5,
#                                rus2.1, rus2.2, rus2.3, rus2.4, rus2.5,
#                                fre3.1, fre3.2, fre3.3, fre3.4, fre3.5,
#                                eng4.1, eng4.2, eng4.3, eng4.4, eng4.5) |> 
#   group_by(story) |> 
#   mutate(sent_num = 1:n(),
#          prop_num = sent_num/n()) |> 
#   ungroup()
# 
# write_csv(all.clean.chunk.split, "../data/text_split/all.clean.split_VChunk.csv")


## Granularity: groups of 3 sentences, 5 sentences, 10 sentences per row ----

granular.split <- data.frame()

for (grains in 1:20){
  text1.clean.split <- text1.clean |> 
    tokenize_sentences() |> 
    unlist()
  
  text1.grained <- data.frame(story = 1, lang = "english",
                         sentences = granularize.sentences(grains, 
                                                           text1.clean.split),
                         grain_size = grains)
  
  text2.clean.split <- text2.clean |> 
    tokenize_sentences() |> 
    unlist() 
  
  text2.grained <- data.frame(story = 2, lang = "russian",
                         sentences = granularize.sentences(grains, text2.clean.split),
                         grain_size = grains)
  
  
  text3.clean.split <- text3.clean |> 
    tokenize_sentences() |> 
    unlist() 
  
  text3.grained <- data.frame(story = 3, lang = "french",
                         sentences = granularize.sentences(grains, text3.clean.split),
                         grain_size = grains)
  
  text4.clean.split <- text4.clean |> 
    tokenize_sentences() |> 
    unlist()
  
  text4.grained <- data.frame(story = 4, lang = "english",
                         sentences = granularize.sentences(grains, text4.clean.split),
                         grain_size = grains)
  
  
  temp <- bind_rows(text1.grained,
                              text2.grained,
                              text3.grained,
                    text4.grained) |> 
    group_by(story, grain_size) |> 
    mutate(index = 1:n()) |> 
    ungroup()
  
  granular.split <- rbind(granular.split, temp)
}



write_csv(granular.split, "../data/text_split/karma.granular_split.csv")

## Sliding window ----

## Test:
# sentences = seq(1:32)
# sliding.window(10, sentences)

window.split <- data.frame()

for (window in 1:20){
  text1.clean.split <- text1.clean |> 
    tokenize_sentences() |> 
    unlist()
  
  text1.window <- data.frame(story = 1, lang = "english",
                             sentences = sliding.window(window,
                                                        text1.clean.split),
                             window = window)
  
  text2.clean.split <- text2.clean |> 
    tokenize_sentences() |> 
    unlist() 
  
  text2.window <- data.frame(story = 2, lang = "russian",
                             sentences = sliding.window(window, text2.clean.split),
                             window = window)
  
  
  text3.clean.split <- text3.clean |> 
    tokenize_sentences() |> 
    unlist() 
  
  text3.window <- data.frame(story = 3, lang = "french",
                             sentences = sliding.window(window, text3.clean.split),
                             window = window)
  
  text4.clean.split <- text4.clean |> 
    tokenize_sentences() |> 
    unlist()
  
  text4.window <- data.frame(story = 4, lang = "english",
                             sentences = sliding.window(window, text4.clean.split),
                             window = window)
  
  
  temp <- bind_rows(text1.window,
                    text2.window,
                    text3.window,
                    text4.window) |> 
    group_by(story, window) |> 
    mutate(index = 1:n()) |> 
    ungroup()
  
  window.split <- rbind(window.split, temp)
}

write_csv(window.split, "../data/text_split/karma.window_split.csv")

# Le Pere Martin ----

## Load in the files ----
text1 <- read_file("../data/lpm_texts/final/french_1_1882-final.txt")
text2 <- read_file("../data/lpm_texts/final/russian_2_1884-final.txt")
text3 <- read_file("../data/lpm_texts/final/russian_3_1885-final.txt")
text4 <- read_file("../data/lpm_texts/final/french_4_1887-final.txt")

## Remove carriage returns ----

text1.clean <- text1 |> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") 

text2.clean <- text2 |> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") 

text3.clean <- text3 |> 
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") 

text4.clean <- text4  |>  
  str_replace_all("[\r]", replacement = " ") |> 
  str_replace_all("[\n]", replacement = " ") |> 
  str_replace_all("- ", replacement = "") |>
  str_replace_all("  ", replacement = " ") 

## Split the sentences ----

text1.clean.split <- text1.clean |> 
  tokenize_sentences() |> 
  unlist() 
text1.clean.split <- tibble(story = 1, lang = "french", sentences = text1.clean.split) 

text2.clean.split <- text2.clean |> 
  tokenize_sentences() |> 
  unlist() 
text2.clean.split <- tibble(story = 2, lang = "russian", sentences = text2.clean.split)

text3.clean.split <- text3.clean |> 
  tokenize_sentences() |> 
  unlist() 
text3.clean.split <- tibble(story = 3, lang = "russian", sentences = text3.clean.split)

text4.clean.split <- text4.clean |> 
  tokenize_sentences() |> 
  unlist()
text4.clean.split <- tibble(story = 4, lang = "french", sentences = text4.clean.split)


all.clean.split <- bind_rows(text1.clean.split, 
                             text2.clean.split, 
                             text3.clean.split,
                             text4.clean.split) |> 
  group_by(story) |> 
  mutate(sentence_num = 1:n()) |> 
  ungroup()


## Saving the data ----

write_csv(all.clean.split, "../data/text_split/lpm.clean.split.csv")


## Granularity: groups of 3 sentences, 5 sentences, 10 sentences per row ----

granular.split <- data.frame()

for (grains in 1:20){
  text1.clean.split <- text1.clean |> 
    tokenize_sentences() |> 
    unlist()
  
  text1.grained <- data.frame(story = 1, lang = "french",
                             sentences = granularize.sentences(grains, 
                                                               text1.clean.split),
                             grain_size = grains)
  
  text2.clean.split <- text2.clean |> 
    tokenize_sentences() |> 
    unlist() 
  
  text2.grained <- data.frame(story = 2, lang = "rus",
                             sentences = granularize.sentences(grains, text2.clean.split),
                             grain_size = grains)
  
  
  text3.clean.split <- text3.clean |> 
    tokenize_sentences() |> 
    unlist() 
  
  text3.grained <- data.frame(story = 3, lang = "russian",
                             sentences = granularize.sentences(grains, text3.clean.split),
                             grain_size = grains)
  
  text4.clean.split <- text4.clean |> 
    tokenize_sentences() |> 
    unlist()
  
  text4.grained <- data.frame(story = 4, lang = "french",
                             sentences = granularize.sentences(grains, text4.clean.split),
                             grain_size = grains)
  
  
  temp <- bind_rows(text1.grained,
                    text2.grained,
                    text3.grained,
                    text4.grained) |> 
    group_by(story, grain_size) |> 
    mutate(index = 1:n()) |> 
    ungroup()
  
  granular.split <- rbind(granular.split, temp)
}



write_csv(granular.split, "../data/text_split/lpm.granular_split.csv")

## Sliding window ----


## Test:
# sentences = seq(1:32)
# sliding.window(10, sentences)

window.split <- data.frame()

for (window in 1:20){
  text1.clean.split <- text1.clean |> 
    tokenize_sentences() |> 
    unlist()
  
  text1.window <- data.frame(story = 1, lang = "french",
                            sentences = sliding.window(window,
                                                       text1.clean.split),
                            window = window)
  
  text2.clean.split <- text2.clean |> 
    tokenize_sentences() |> 
    unlist() 
  
  text2.window <- data.frame(story = 2, lang = "russian",
                            sentences = sliding.window(window, text2.clean.split),
                            window = window)
  
  
  text3.clean.split <- text3.clean |> 
    tokenize_sentences() |> 
    unlist() 
  
  text3.window <- data.frame(story = 3, lang = "russian",
                            sentences = sliding.window(window, text3.clean.split),
                            window = window)
  
  text4.clean.split <- text4.clean |> 
    tokenize_sentences() |> 
    unlist()
  
  text4.window <- data.frame(story = 4, lang = "french",
                            sentences = sliding.window(window, text4.clean.split),
                            window = window)
  
  
  temp <- bind_rows(text1.window,
                    text2.window,
                    text3.window,
                    text4.window) |> 
    group_by(story, window) |> 
    mutate(index = 1:n()) |> 
    ungroup()
  
  window.split <- rbind(window.split, temp)
}

write_csv(window.split, "../data/text_split/lpm.window_split.csv")

