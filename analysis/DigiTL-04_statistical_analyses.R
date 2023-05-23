library(tidyverse)
library(here)
setwd(here())
library(janitor)
library(patchwork)
library(reshape2)
library(brms)
library(ggdist)
library(lme4)
library(lmerTest)
options(mc.cores = parallel::detectCores())
theme_set(theme_bw() + theme(text =  element_text(size = 14)))



## Load in the files ----
chunk_recon_df <- read_csv("../data/processed_data/karma_chunk_reconstruction_df.csv",
                              show_col_types = F) 

chunk_dtw <- read_csv("../data/processed_data/karma.chunk_dtw.csv",
                      show_col_types = F)

reconstruction_df <- read_csv("../data/processed_data/karma_reconstruction_df.csv",
                              show_col_types = F) |> 
  select(-c(x.max, y.max, x,y,value))

chunk_recon_df <- chunk_recon_df |> 
  left_join(chunk_dtw, by = c("x.story", "x.chunk", "y.story","y.chunk"))

dtw_df <- read_csv("../data/processed_data/karma_dtw_values.csv",
                   show_col_types = F) |> 
  rename(x.story = story,
         y.story = variable,
         dtw.distance = value)

reconstruction_df <- reconstruction_df |> 
  left_join(dtw_df, by = c("x.story","y.story"))

## Variables ----
# x.story = true lang
# x.num.prop = true_sentence_order (normalized)
# y.story = pred lang
# y.num.prop = pred_order (normalized)
# max.sim = sentence_similarity (individual)
# num.prop.diff = prediction_diff
# translation_steps (per story)
# reconstruction_score (average prediction difference)
# reconstruction_score (average cosine similarity)
# dtw (per story)

reconstruction_df <- reconstruction_df |> 
  rename(true_lang = x.story,
         true_order.prop = x.num.prop,
         true_order = x.num,
         pred_lang = y.story,
         pred_order.prop = y.num.prop,
         pred_order = y.num,
         similarity = max.sim,
         pred_diff = num.prop.diff,
         pred_diff.abs = num.prop.diff.abs)

## To fix:
chunk_recon_df <- chunk_recon_df |> 
  rename(true_lang = x.story,
         true_order.prop = x.num.prop,
         true_order = x.num,
         pred_lang = y.story,
         pred_order.prop = y.num.prop,
         pred_order = y.num,
         similarity = max.sim,
         pred_diff = num.prop.diff,
         pred_diff.abs = num.prop.diff.abs)

rm(dtw_df, chunk_dtw)
gc()



# Data viz ----
ggplot(reconstruction_df, aes(x = pred_order.prop, y = true_order.prop, 
                              color = pred_lang, shape = pred_lang))+
  geom_point(size = 5)+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~true_lang)+
  scale_color_brewer(type = "qual", palette = 3)+
  labs(caption = "Faceted by true_lang")

ggplot(reconstruction_df, aes(x = pred_diff, y = true_order.prop, 
                              color = pred_lang, shape = pred_lang))+
  geom_point(alpha = 0.25, size = 2)+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~true_lang)+
  scale_color_brewer(type = "qual", palette = 3)+
  labs(caption = "Faceted by true_lang")

# Variables / setting contrasts:
# True order & pred order -- already normalized
# DTW distance -- already normalized
# Translation steps? Set as factor?
reconstruction_df$translation_steps <- as.numeric(reconstruction_df$translation_steps)


## Chunk model ----
# DTW(chunk i j) ~ 1 + translation_steps + chunk_difference
# Do they differ more if they are farther apart in the translation steps vs if they are not the same plot point?



## Model 1 ----

# Can we predict the true order as a combination of the predicted order and translation steps

## Lexical distance as 1-lexical similarity
lex.dis <- data.frame(true_lang = c("eng1","rus2","fre3","eng4"),
                      pred_lang = c("eng1","rus2","fre3","eng4")) |> 
  expand(true_lang, pred_lang) |> 
  mutate(distance = c(0,0,0.74,0.69,
                      0,0,0.74,0.69,
                      0.74,0.74,0,0.53,
                      0.74, 0.74,0.53,0))

reconstruction_df <- reconstruction_df |> 
  left_join(lex.dis, by = c("true_lang","pred_lang"))

reconstruction_df <- reconstruction_df |> 
  mutate(different_langs = true_lang != pred_lang)

reconstruction_df.different_langs <- reconstruction_df |> 
  filter(true_lang != pred_lang)



# Linear model version:
lm1 <- lm(formula = pred_diff.abs ~ 1 + distance + translation_steps,
          data = reconstruction_df.different_langs)
          # data = reconstruction_df)

summary(lm1)


mlm2 <- lmer(formula = pred_diff.abs ~ 1 + distance + translation_steps + 
               (1 + translation_steps || true_lang), # doesn't fit correlations between them
          data = reconstruction_df.different_langs, 
          control = lmerControl(optimizer = "bobyqa"))
# Try a different optimizer - bobyqa

summary(mlm2)

ggplot(reconstruction_df.different_langs |> filter(pred_diff.abs != 0),
       aes(x = pred_diff.abs))+
  geom_density()+
  scale_x_log10()

# Scaling the pred_diff.abs:
# Add a small value to bump from zero, then scale by log10
# Model 
  

# Prior predictive check needed?

# Priors:
basic_priors <- get_prior(pred_diff.abs ~ 1 + distance + translation_steps + 
                            (1 + distance + translation_steps || true_lang),
                          data = reconstruction_df.different_langs,
                          family = Beta())


# Model:
# bm1 <- brm(formula = pred_diff.abs ~ 1 + distance + translation_steps + 
#              (1 + distance + translation_steps || true_lang),
#            data = reconstruction_df.different_langs,
#            family = Beta(),
#            prior = basic_priors, cores = 8, iter = 400,
#            file = "stat_models/model1-pred_diff-Fixed.rds")

bm1 <- brm(bf(pred_diff.abs ~ 1 + distance + translation_steps + 
                (1 + distance + translation_steps || true_lang),
              phi ~ 1),
           data = reconstruction_df.different_langs,
           family = zero_inflated_beta(), # also fitting a zero parameter for those at zero
           # prior = basic_priors, 
           cores = 8, iter = 4000,
           file = "stat_models/model1-pred_diff-Fixed.rds")

summary(bm1)

plot(bm1) # Chains don't converge well

pp_check(bm1)

## Warnings:
#   1: There were 2127 divergent transitions after warmup. See
# https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# to find out why this is a problem and how to eliminate them. 
# 2: Examine the pairs() plot to diagnose sampling problems
# 
# 3: The largest R-hat is 1.12, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#r-hat 
# 4: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#bulk-ess 
# 5: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#tail-ess 


## Model 2 ----

# Can we predict the translation steps based on the dtw distance and prediction difference?
# Formula: translation_steps ~ dtw + prediction_diff + (1 + "..." | true_lang)

# Need to filter down DF to distinct rows for translation steps, dtw, and prediction diff?

# LM version:
lm2 <- lm(translation_steps ~ 1 + dtw.distance + pred_diff.abs,
          data = reconstruction_df)
# Swap the IV DV to check to see how it relates to others
summary(lm2)

# Priors:
basic_priors2 <- get_prior(translation_steps ~ 1 + dtw.distance + pred_diff + (1 + dtw.distance + pred_diff | true_lang),
                          data = reconstruction_df,
                          family = gaussian(link = "logit"))
# Model:
# Change to an ordinal model?
bm2 <- brm(formula = translation_steps ~ 1 + dtw.distance + pred_diff + (1 + dtw.distance + pred_diff | true_lang),
           data = reconstruction_df,
           family = gaussian(link = "logit"),
           prior = basic_priors2, cores = 8, iter = 8000,
           file = "stat_models/model2-translation_steps.rds")

summary(bm2)

plot(bm2) # Chains don't converge well

pp_check(bm2)

