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


all.clean.split <- bind_rows(eng1.clean.split, 
                             rus2.clean.split, 
                             fre3.clean.split,
                             eng4.clean.split) |> 
  group_by(story) |> 
  mutate(sentence_num = 1:n()) |> 
  ungroup()


## Saving the data ----

write_csv(all.clean.split, "../data/text_split/all.clean.split_V2.csv")

## Chunk version ----

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


## Combining dfs ----

all.clean.chunk.split <- rbind(eng1.1, eng1.2, eng1.3, eng1.4, eng1.5,
                               rus2.1, rus2.2, rus2.3, rus2.4, rus2.5,
                               fre3.1, fre3.2, fre3.3, fre3.4, fre3.5,
                               eng4.1, eng4.2, eng4.3, eng4.4, eng4.5) |> 
  group_by(story) |> 
  mutate(sent_num = 1:n(),
         prop_num = sent_num/n()) |> 
  ungroup()

write_csv(all.clean.chunk.split, "../data/text_split/all.clean.split_VChunk.csv")


## Granularity: groups of 3 sentences, 5 sentences, 10 sentences per row ----

granularize.sentences <- function(grain.size, sentences){
  combined.sentences <- character(0)
  for (i in seq(1, length(sentences), by = grain.size)) {
    end.index <- min(i + grain.size - 1, 
                     length(sentences))
    combined.sentences <- c(combined.sentences, 
                            paste(sentences[i:end.index], 
                                  collapse = ""))
  }
  return(combined.sentences)
}

eng1.clean.split <- eng1.clean |> 
  tokenize_sentences() |> 
  unlist()

eng1.by5 <- data.frame(story = 1, lang = "eng",
                       sentences = granularize.sentences(5, eng1.clean.split),
                       grain_size = 5)

eng1.by10 <- data.frame(story = 1, lang = "eng",
                       sentences = granularize.sentences(10, eng1.clean.split),
                       grain_size = 10)

eng1.by20 <- data.frame(story = 1, lang = "eng",
                        sentences = granularize.sentences(20, eng1.clean.split),
                        grain_size = 20)

rus2.clean.split <- rus2.clean |> 
  tokenize_sentences() |> 
  unlist() 

rus2.by5 <- data.frame(story = 2, lang = "rus",
                       sentences = granularize.sentences(5, rus2.clean.split),
                       grain_size = 5)

rus2.by10 <- data.frame(story = 2, lang = "rus",
                        sentences = granularize.sentences(10, rus2.clean.split),
                        grain_size = 10)

rus2.by20 <- data.frame(story = 2, lang = "rus",
                        sentences = granularize.sentences(20, rus2.clean.split),
                        grain_size = 20)

fre3.clean.split <- fre3.clean |> 
  tokenize_sentences() |> 
  unlist() 

fre3.by5 <- data.frame(story = 3, lang = "fre",
                       sentences = granularize.sentences(5, fre3.clean.split),
                       grain_size = 5)

fre3.by10 <- data.frame(story = 3, lang = "fre",
                        sentences = granularize.sentences(10, fre3.clean.split),
                        grain_size = 10)

fre3.by20 <- data.frame(story = 3, lang = "fre",
                        sentences = granularize.sentences(20, fre3.clean.split),
                        grain_size = 20)

eng4.clean.split <- eng4.clean |> 
  tokenize_sentences() |> 
  unlist()

eng4.by5 <- data.frame(story = 4, lang = "eng",
                       sentences = granularize.sentences(5, eng4.clean.split),
                       grain_size = 5)

eng4.by10 <- data.frame(story = 4, lang = "eng",
                        sentences = granularize.sentences(10, eng4.clean.split),
                        grain_size = 10)

eng4.by20 <- data.frame(story = 4, lang = "eng",
                        sentences = granularize.sentences(20, eng4.clean.split),
                        grain_size = 20)


granular.split <- bind_rows(eng1.by5, eng1.by10, eng1.by20,
                            rus2.by5, rus2.by10, rus2.by20,
                            fre3.by5, fre3.by10, fre3.by20,
                            eng4.by5, eng4.by10, eng4.by20) |> 
  group_by(story, grain_size) |> 
  mutate(index = 1:n()) |> 
  ungroup()

write_csv(granular.split, "../data/text_split/granular_split.csv")


