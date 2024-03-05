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

## Starting from scratch, but leaving the previous code at the bottom.

# Loading in the files ----

dtw.df <- read_csv("../data/processed_data/karma_dtw_values.csv", show_col_type = F)

chunk.dtw <- read_csv("../data/processed_data/karma.chunk_dtw.csv", show_col_type = F)


## Model 1: can we predict translation steps from DTW distance? ----
# ordered(translation_steps) ~ 1 + dtw.distance +  (1 + dtw.distance | true_lang)

lmdf <- dtw.df |> 
  filter(translation.steps >= 0) |> 
  mutate(story = factor(story))

ggplot(lmdf, aes(x = dtw.distance, y = translation.steps))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)

lm1 <- lm(translation.steps ~ 1 + dtw.distance, data = lmdf)
summary(lm1)


# Complex version:
lm2 <- lmer(translation.steps ~ 1 + dtw.distance + (1 + dtw.distance | story), 
            data = lmdf) # failed to converge.
summary(lm2)


# Bayesian version:
bm2 <- brm(formula = ordered(translation.steps) ~ 1 + dtw.distance +  (1 + dtw.distance | story),
           data = lmdf |> filter(x.story != y.story),
           family = cumulative(), cores = 4, iter = 4000,
           file = "stat_models/model2-small-translation_steps_ordinal2.rds")
summary(bm2)

pp_check(bm2, ndraws = 50)

tidybayes::get_variables(bm2)[1:20]

posterior_draws <- brms::as_draws_matrix(bm2)[,c("b_Intercept[1]",
                                                     "b_Intercept[2]",
                                                     "b_dtw.distance")]
bayesplot::mcmc_areas(posterior_draws)+
  geom_vline(xintercept = 0, color = 'red')


# Predicted translation steps for the 10 observations
# Compare the actual to the predicted translation steps
# Try scaling DTW

# Redo with chunk df but limit chunk == chunk

bm1.5 <- brm(formula = ordered(translation.steps) ~ 1 + dtw.s +  (1 + dtw | y.story),
           data = lmdf2 |> filter(x.chunk == y.chunk & x.story != y.story) |> mutate(dtw.s = dtw / max(dtw)),
           family = cumulative(), cores = 4, iter = 4000,
           file = "stat_models/model1-5_chunkedversion3.rds")
summary(bm1.5)

posterior_draws <- brms::as_draws_matrix(bm1.5)[,c("b_Intercept[1]",
                                                 "b_Intercept[2]",
                                                 "b_dtw.s")]
bayesplot::mcmc_areas(posterior_draws)+
  geom_vline(xintercept = 0, color = 'red')

lmdf2 |> filter(x.chunk == y.chunk  & x.story != y.story) |> mutate(dtw.s = dtw / max(dtw)) |> 
  ggplot(aes(x = dtw.s, group = translation.steps, fill = translation.steps))+
  geom_density(alpha = 0.5)


## Model 2: Do they differ more if they are farther apart in the story? ----
# DTW(chunk i j) ~ 1 + translation_steps + chunk_difference

lmdf2 <- chunk.dtw |> 
  mutate(translation.steps = y.story - x.story,
         chunk.diff = y.chunk - x.chunk) |> 
  filter(translation.steps >= 0 & chunk.diff >= 0)

ggplot(lmdf2, aes(x = chunk.diff, y = dtw, color = factor(translation.steps)))+
  geom_point()+
  geom_smooth(method = 'lm', se = F)

lm3 <- lm(dtw ~ 1 + translation.steps + chunk.diff, data = lmdf2)
summary(lm3)

# Bayesian version:
bm3 <- brm(formula = dtw ~ 1 + translation.steps + chunk.diff,
           data = lmdf2,
           family = gaussian(), cores = 4, iter = 4000,
           file = "stat_models/model3-chunk-dtw.rds")
summary(bm3)

pp_check(bm3, ndraws = 50) # should be a bimodal distribution?

tidybayes::get_variables(bm3)[1:5]

posterior_draws2 <- brms::as_draws_matrix(bm3)[,c("b_Intercept",
                                                 "b_translation.steps",
                                                 "b_chunk.diff")]
bayesplot::mcmc_areas(posterior_draws2)+
  geom_vline(xintercept = 0, color = 'red')


## Model 3: Can we predict the scope based on distance? ----
# Sliding_window ~ 1 + dtw.distance ?





################################################################################


# 
# ## Load in the files ----
# chunk_recon_df <- read_csv("../data/processed_data/karma_chunk_reconstruction_df.csv",
#                               show_col_types = F) 
# 
# chunk_dtw <- read_csv("../data/processed_data/karma.chunk_dtw.csv",
#                       show_col_types = F)
# 
# reconstruction_df <- read_csv("../data/processed_data/karma_reconstruction_df.csv",
#                               show_col_types = F) |> 
#   select(-c(x.max, y.max, x,y,value))
# 
# chunk_recon_df <- chunk_recon_df |> 
#   left_join(chunk_dtw, by = c("x.story", "x.chunk", "y.story","y.chunk"))
# 
# dtw_df <- read_csv("../data/processed_data/karma_dtw_values.csv",
#                    show_col_types = F) |> 
#   rename(x.story = story,
#          y.story = variable,
#          dtw.distance = value)
# 
# reconstruction_df <- reconstruction_df |> 
#   left_join(dtw_df, by = c("x.story","y.story"))
# 
# ## Variables ----
# # x.story = true lang
# # x.num.prop = true_sentence_order (normalized)
# # y.story = pred lang
# # y.num.prop = pred_order (normalized)
# # max.sim = sentence_similarity (individual)
# # num.prop.diff = prediction_diff
# # translation_steps (per story)
# # reconstruction_score (average prediction difference)
# # reconstruction_score (average cosine similarity)
# # dtw (per story)
# 
# reconstruction_df <- reconstruction_df |> 
#   rename(true_lang = x.story,
#          true_order.prop = x.num.prop,
#          true_order = x.num,
#          pred_lang = y.story,
#          pred_order.prop = y.num.prop,
#          pred_order = y.num,
#          similarity = max.sim,
#          pred_diff = num.prop.diff,
#          pred_diff.abs = num.prop.diff.abs)
# 
# ## To fix:
# chunk_recon_df <- chunk_recon_df |> 
#   rename(true_lang = x.story,
#          true_order.prop = x.num.prop,
#          true_order = x.num,
#          pred_lang = y.story,
#          pred_order.prop = y.num.prop,
#          pred_order = y.num,
#          similarity = max.sim,
#          pred_diff = num.prop.diff,
#          pred_diff.abs = num.prop.diff.abs)
# 
# rm(dtw_df, chunk_dtw)
# gc()
# 
# 
# 
# # Data viz ----
# ggplot(reconstruction_df, aes(x = pred_order.prop, y = true_order.prop, 
#                               color = pred_lang, shape = pred_lang))+
#   geom_point(size = 5)+
#   geom_smooth(method = "lm", se = F)+
#   facet_wrap(~true_lang)+
#   scale_color_brewer(type = "qual", palette = 3)+
#   labs(caption = "Faceted by true_lang")
# 
# ggplot(reconstruction_df, aes(x = pred_diff, y = true_order.prop, 
#                               color = pred_lang, shape = pred_lang))+
#   geom_point(alpha = 0.25, size = 2)+
#   geom_smooth(method = "lm", se = F)+
#   facet_wrap(~true_lang)+
#   scale_color_brewer(type = "qual", palette = 3)+
#   labs(caption = "Faceted by true_lang")
# 
# # Variables / setting contrasts:
# # True order & pred order -- already normalized
# # DTW distance -- already normalized
# # Translation steps? Set as factor?
# reconstruction_df$translation_steps <- as.numeric(reconstruction_df$translation_steps)
# 
# 
# ## Chunk model: This is the one. ----
# # DTW(chunk i j) ~ 1 + translation_steps + chunk_difference
# # Do they differ more if they are farther apart in the translation steps vs if they are not the same plot point?
# 
# 
# 
# ## Model 1 ----
# 
# # Can we predict the true order as a combination of the predicted order and translation steps
# 
# ## Lexical distance as 1-lexical similarity
# lex.dis <- data.frame(true_lang = c("eng1","rus2","fre3","eng4"),
#                       pred_lang = c("eng1","rus2","fre3","eng4")) |> 
#   tidyr::expand(true_lang, pred_lang) |> 
#   mutate(distance = c(0,0,0.74,0.69,
#                       0,0,0.74,0.69,
#                       0.74,0.74,0,0.53,
#                       0.74, 0.74,0.53,0))
# 
# reconstruction_df <- reconstruction_df |> 
#   left_join(lex.dis, by = c("true_lang","pred_lang"))
# 
# reconstruction_df <- reconstruction_df |> 
#   mutate(different_langs = true_lang != pred_lang)
# 
# reconstruction_df.different_langs <- reconstruction_df |> 
#   filter(true_lang != pred_lang)
# 
# 
# 
# # Linear model version:
# lm1 <- lm(formula = pred_diff.abs ~ 1 + distance + translation_steps,
#           data = reconstruction_df.different_langs)
#           # data = reconstruction_df)
# 
# summary(lm1)
# 
# 
# mlm2 <- lmer(formula = pred_diff.abs ~ 1 + distance + translation_steps + 
#                (1 + translation_steps || true_lang), # doesn't fit correlations between them
#           data = reconstruction_df.different_langs, 
#           control = lmerControl(optimizer = "bobyqa"))
# # Try a different optimizer - bobyqa
# 
# summary(mlm2)
# 
# ggplot(reconstruction_df.different_langs |> filter(pred_diff.abs != 0),
#        aes(x = pred_diff.abs))+
#   geom_density()+
#   scale_x_log10()
# 
# # Scaling the pred_diff.abs:
# # Add a small value to bump from zero, then scale by log10
# # Model 
#   
# 
# # Prior predictive check needed?
# 
# # Priors:
# basic_priors <- get_prior(pred_diff.abs ~ 1 + distance + translation_steps + 
#                             (1 + distance + translation_steps || true_lang),
#                           data = reconstruction_df.different_langs,
#                           family = Beta())
# 
# 
# # Model:
# # bm1 <- brm(formula = pred_diff.abs ~ 1 + distance + translation_steps + 
# #              (1 + distance + translation_steps || true_lang),
# #            data = reconstruction_df.different_langs,
# #            family = Beta(),
# #            prior = basic_priors, cores = 8, iter = 400,
# #            file = "stat_models/model1-pred_diff-Fixed.rds")
# 
# bm1 <- brm(bf(pred_diff.abs ~ 1 + distance + translation_steps + 
#                 (1 + distance + translation_steps || true_lang),
#               phi ~ 1),
#            data = reconstruction_df.different_langs,
#            family = zero_inflated_beta(), # also fitting a zero parameter for those at zero
#            # prior = basic_priors, 
#            cores = 8, iter = 4000,
#            file = "stat_models/model1-pred_diff-Fixed.rds")
# 
# summary(bm1)
# 
# plot(bm1) # Chains don't converge well
# 
# pp_check(bm1)
# 
# 
# ## NEW MODEL!
# 
# # (chunk level dtw ~ 1 + distance + translation_steps)
# # At the chunk level, 
# 
# 
# 
# 
# ## Warnings:
# #   1: There were 2127 divergent transitions after warmup. See
# # https://mc-stan.org/misc/warnings.html#divergent-transitions-after-warmup
# # to find out why this is a problem and how to eliminate them. 
# # 2: Examine the pairs() plot to diagnose sampling problems
# # 
# # 3: The largest R-hat is 1.12, indicating chains have not mixed.
# # Running the chains for more iterations may help. See
# # https://mc-stan.org/misc/warnings.html#r-hat 
# # 4: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# # Running the chains for more iterations may help. See
# # https://mc-stan.org/misc/warnings.html#bulk-ess 
# # 5: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# # Running the chains for more iterations may help. See
# # https://mc-stan.org/misc/warnings.html#tail-ess 
# 
# 
# ## Model 2 ----
# 
# # Can we predict the translation steps based on the dtw distance and prediction difference?
# # Formula: translation_steps ~ dtw + prediction_diff + (1 + "..." | true_lang)
# 
# # Need to filter down DF to distinct rows for translation steps, dtw, and prediction diff?
# 
# # LM version:
# lm2 <- lm(translation_steps ~ 1 + dtw.distance + pred_diff.abs,
#           data = reconstruction_df)
# # Swap the IV DV to check to see how it relates to others
# summary(lm2)
# 
# # Priors:
# basic_priors2 <- get_prior(translation_steps ~ 1 + dtw.distance + pred_diff + (1 + dtw.distance + pred_diff | true_lang),
#                           data = reconstruction_df,
#                           family = gaussian(link = "logit"))
# # Model:
# # Change to an ordinal model?
# bm2 <- brm(formula = ordered(translation_steps) ~ 1 + dtw.distance +  (1 + dtw.distance | true_lang),
#            data = reconstruction_df.different_langs,
#            family = cumulative(), cores = 4, iter = 4000,
#            file = "stat_models/model2-translation_steps_ordinal.rds")
# 
# bm2.2 <- brm(formula = translation_steps ~ 1 + dtw.distance +  (1 + dtw.distance | true_lang),
#            data = reconstruction_df.different_langs,
#            family = gaussian(), cores = 4, iter = 4000,
#            file = "stat_models/model2-translation_steps_gaussian.rds")
# 
# # As stories become more dissimilar, does that indicate that they are separated by more steps?
# # What are the symptoms of cultural separation / translation?
# 
# summary(bm2.2)
# 
# plot(bm2.2) # Chains don't converge well
# 
# pp_check(bm2.2)
# 
