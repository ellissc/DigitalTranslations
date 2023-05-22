library(tidyverse)
library(here)
setwd(here())
library(tokenizers)
library(janitor)
library(Rtsne)
library(dimRed)
library(wesanderson)
library(patchwork)
library(slider)
library(lsa)
library(dtw)


## Load in the files ----
eng1 <- read_file("../data/Karma texts/English_1_1894_corrected.txt")
rus2 <- read_file("../data/Karma texts/Russian_2_1894_corrected.txt")
fre3.1 <- read_file("../data/Karma texts/French_3_1895_part1_corrected.txt")
fre3.2 <- read_file("../data/Karma texts/French_3_1895_part2_corrected.txt")
fre3 <- paste(fre3.1, fre3.2)
rm(fre3.1, fre3.2)
eng4 <- read_file("../data/Karma texts/English_4_1896_corrected.txt")


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

## Sliding window ----
# A tibble: 4 × 2
# story mean.length
#   <dbl>       <dbl>
# 1     1        24.2
# 2     2        21.1
# 3     3        20.8
# 4     4        19.0

sliding.window <- function(window.size = 20, cleaned.story){
  words <- str_split(cleaned.story, pattern = " ") |> 
    unlist()
  
  sliding.sentences <- c()
  
  for (ii in 1:(length(words)-window.size)){
    window.subset <- words[ii:(ii+window.size)]
    sliding.sentences <- append(sliding.sentences, paste(window.subset, collapse = " "))
  }
  return(data.frame(sliding.sentences))
}


window.size = 20

eng1.sliding.sentences <- sliding.window(window.size, eng1.clean) |> 
  mutate(story = 1, lang = "eng") |> 
  mutate(sentence_num = 1:n(),
         prop_num = sentence_num/n())

rus2.sliding.sentences <- sliding.window(window.size, rus2.clean) |> 
  mutate(story = 2, lang = "rus") |> 
  mutate(sentence_num = 1:n(),
         prop_num = sentence_num/n())

fre3.sliding.sentences <- sliding.window(window.size, fre3.clean) |> 
  mutate(story = 3, lang = "fre") |> 
  mutate(sentence_num = 1:n(),
         prop_num = sentence_num/n())

eng4.sliding.sentences <- sliding.window(window.size, eng4.clean) |> 
  mutate(story = 4, lang = "eng") |> 
  mutate(sentence_num = 1:n(),
         prop_num = sentence_num/n())

all.sliding.split <- rbind(eng1.sliding.sentences,
                           rus2.sliding.sentences,
                           fre3.sliding.sentences,
                           eng4.sliding.sentences)


write_csv(all.sliding.split, "../data/text_split/all.clean.window-split.csv")

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

all.clean.split <- bind_rows(eng1.clean.split, 
                             rus2.clean.split, 
                             fre3.clean.split,
                             eng4.clean.split) |> 
  group_by(story) |> 
  mutate(sentence_num = 1:n()) |> 
  ungroup()

all.clean.split |> 
  rowwise() |> 
  mutate(split = str_split(sentences, " ")) |> 
  mutate(length = length(split)) |> 
  group_by(story) |> 
  summarize(mean.length = mean(length))




# write_csv(all.clean.split, "../data/text_split/all.clean.split_V2.csv")
