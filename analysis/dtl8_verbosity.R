library(tidyverse)
library(lmerTest)
library(brms)

stories <- read_csv("../data/text_split/all.clean.split.csv")
chunks <- read_csv("../data/text_split/all.clean.split_VChunk.csv")

story.verbosity <- stories %>% 
  group_by(story) %>% 
  summarize(story_sentence_count = n())

chunk.verbosity <- chunks %>% 
  group_by(story, chunk) %>% 
  summarize(chunk_sentence_count = n(), .groups = "drop") 

# Story-sections range from 21 to 46 sentences, with good amount of variability
summary(chunk.verbosity$chunk_sentence_count)
hist(chunk.verbosity$chunk_sentence_count)

# English versions tend to have more sentences and more words. 
# Despite this similarity, they still differ more in the DTW. Cool. 

dtw.df <- read_csv("../data/processed_data/karma_dtw_values.csv")
chunk.dtw <- read_csv("../data/processed_data/karma.chunk_dtw.csv")

dtw.df %>% filter(y.story == 1)


chunk.dtw2 <- chunk.dtw %>% 
  mutate(translation.steps = y.story - x.story,
         chunk.diff = y.chunk - x.chunk) %>%
  filter(translation.steps >= 0 & chunk.diff >= 0) %>%
  filter(x.story != y.story & x.chunk == y.chunk) %>% 
  left_join(chunk.verbosity %>% rename(x.story = story,
                                       x.chunk = chunk,
                                       x.chunk_sentence_count = chunk_sentence_count,
  )) %>% 
  left_join(chunk.verbosity %>% rename(y.story = story,
                                       y.chunk = chunk,
                                       y.chunk_sentence_count = chunk_sentence_count)) %>% 
  mutate(chunk_sent_count_diff = abs(x.chunk_sentence_count - y.chunk_sentence_count)) %>%
  mutate(same.lang = x.story == 1 & y.story==4) %>%
  mutate(dtw.s = dtw / max(dtw)) %>%
  select(x, y, translation.steps, dtw.s, chunk_sent_count_diff, same.lang, everything()) 


chunk.dtw2 %>% filter(translation.steps > 0, x.chunk==y.chunk, x.story ==1) %>% 
  ggplot(aes(x = translation.steps, y=dtw.s, color=factor(x.chunk), group = x.chunk)) + 
  geom_line() + theme_classic()


verbosity.m1 <- lmer(dtw.s ~ translation.steps + 
                       (1 | y.story) + (1|y.chunk),
                     chunk.dtw2)
summary(verbosity.m1)

verbosity.m2 <- lmer(dtw.s ~ translation.steps + chunk_sent_count_diff +  same.lang + 
                       (1 | y.story) + (1|y.chunk),
                     chunk.dtw2)
summary(verbosity.m2)


chunk.dtw2 <- chunk.dtw2 %>% 
  mutate(translation.steps.ord = ordered(translation.steps))

verbosity.m3 <- ordinal::clmm(translation.steps.ord ~  dtw.s + chunk_sent_count_diff + 
                                (1 | y.story) + (1|y.chunk),
                              chunk.dtw2)
summary(verbosity.m3)


bm7.1 <- brm(formula = translation.steps.ord ~ 1 + dtw.s +  chunk_sent_count_diff + 
               (1 | y.story) + (1|y.chunk),
             data = chunk.dtw2,
             family = cumulative(), cores = 4, iter = 4000, 
             control = list(adapt_delta = 0.9),
             file = "stat_models/model7-1_chunkedversion1.rds"
             )
summary(bm7.1)

bm7.2 <- brm(formula = dtw.s ~ translation.steps +  same.lang + 
               (1 | y.story) + (1|y.chunk),
             data = chunk.dtw2,
             family = gaussian, cores = 4, iter = 4000, 
             control = list(adapt_delta = 0.9999),
             file = "stat_models/model7-2_chunkedversion1.rds"
             )
summary(bm7.2)
