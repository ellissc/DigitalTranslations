library(here)
setwd(here())
library(tidyverse)
library(TSP)
library(ggrepel)
library(patchwork)
library(dtw)

theme_set(theme_bw(base_size = 14))

# General functions ----
scale.distance = function(x) { (x - min(x)) / (max(x) - min(x)) }


# Generates a "language" as a 2D space of 30 points
create_language <- function(num_points = 250, 
                            range.low = -50, 
                            range.high = 50){
  story = data.frame(x = sample(range.low:range.high, num_points, replace = T),
                     y = sample(range.low:range.high, num_points, replace = T)) |> 
    mutate(t = 1:n()) |> 
    mutate(label = if_else(t == 1, "Start",
                           if_else(t == max(t), "End", NA)))
  
  return(story)
}

# Euclidean distance function
euclid <- function(x1, y1, x2, y2, scaled = T, reverse = T) {
  if (scaled) { 
    distance = scale.distance(sqrt((x2 - x1)^2 + (y2 - y1)^2))
    }
  else {
    distance = sqrt((x2 - x1)^2 + (y2 - y1)^2)
  }
  if (reverse){
    distance = 1 - distance
  }
  
  return (distance)
}

# Recreates the story of original.x using language.other
#
# Goes through the points in original.x and calculates the distances to all the
# points in the language.other, selects the closest point, and repeats.
#
match_story <- function(original.x, language.other, 
                        deterministic = T, fidelity = 1){
  reconstruction <- data.frame()
  
  for (ii in 1:nrow(original.x)){
    x.original <- original.x[ii,]$x
    y.original <- original.x[ii,]$y
    
    if (deterministic){
      nearest <- language.other |> 
        mutate(distance = euclid(x,y,x.original, y.original, F, F)) |> 
        filter(distance == min(distance)) 
      # print(nearest)
      
      nearest <- nearest |> 
        slice_sample(n = 1)
      # print(nearest)
      
    } else {
      nearest <- language.other |> 
        mutate(distance = euclid(x,y,x.original, y.original, T, T))
      # print(nearest)
      
      nearest <- nearest |> 
        slice_sample(n = 1, weight_by = distance^fidelity)
        # slice_sample(n = 1, weight_by = exp(distance*fidelity)) 
      # e^(distance * x), as x goes to zero, everything has equal weight as 
      # it increases, it becomes more extreme
      # print(nearest)
    }
    
    reconstruction <- rbind(reconstruction, nearest)
  }
  
  reconstruction <- reconstruction |> 
    mutate(t.new = 1:n(),
           label = if_else(t.new == 1, "Start",
                           if_else(t.new == max(t.new), "End", NA)))
  
  return(reconstruction)
}

# Test plot ----
# iterative.nd <- iterate_translation(original, 4, F, 1)
# data.frame(x = seq(0,1,by = 0.1)) |> 
#   mutate(exp0 = exp(x*0),
#          exp25 = exp(x*0.25),
#          exp50 = exp(x*0.5),
#          exp75 = exp(x*0.75),
#          exp100 = exp(x*1),
#          exp125 = exp(x*1.25),
#          exp150 = exp(x*1.5),
#          exp175 = exp(x*1.75)) |> 
#   mutate(x.reverse = 1 - x) |> 
#   pivot_longer(cols = starts_with("exp"),
#                values_to = "weighting",
#                names_to = "exp.type") |> 
#   mutate(fidelity = as.numeric(gsub("exp","",exp.type))/100) |> 
#   ggplot(aes(x = x.reverse, y = weighting, color = fidelity, group = fidelity))+
#   geom_line()
# 
# data.frame(x = seq(0,1,by = 0.01)) |> 
#   mutate(x.reverse = 1 - x) |> 
#   mutate(fidelity1 = x.reverse^1,
#          fidelity10 = x.reverse^10,
#          fidelity100 = x.reverse^100) |> 
#   pivot_longer(cols = starts_with("fidelity"),
#                values_to = "weighting",
#                names_to = "fidelity") |> 
#   mutate(fidelity = as.numeric(gsub("fidelity","",fidelity))) |> 
#   ggplot(aes(x = x, y = weighting, color = fidelity, group = fidelity))+
#   geom_line()

# Iterative translation ----

## Helper functions ----

### Chain translations ----
# Creates a translation chain of n_iterations
iterate_translation <- function(original.text, n_iterations = 4, 
                                range.low = -50, range.high = 50,
                                deterministic = T, fidelity = 1, same_language = T){
  original.x <- original.text
  
  # If it's the same language, create the language outside of the for-loop
  if (same_language) {lang.x <- create_language(range.low = range.low, 
                                                range.high = range.high)} 
  
  holder.df <- data.frame()
  
  for (ii in 1:n_iterations){
    # If they are "different languages", create a new language each iteration
    if (!same_language) {lang.x <- create_language(range.low = range.low, 
                                                   range.high = range.high)} 
    recon.x <- match_story(original.x, lang.x, 
                           deterministic, fidelity) |> 
      mutate(story = ii)
    
    holder.df <- rbind(holder.df, recon.x)
    
    original.x <- recon.x
  }
  
  final <- rbind(original.text |> 
                   mutate(t.new = t, distance = -1),
                 holder.df)
  
  return(final)
}

### Multi-translations ----
# Creates a set of translations that can reference the original.
# Same as the above function, but the original.x is not updated.
multi_translations <- function(original.text, n_iterations = 4, 
                               range.low = -50, range.high = 50,
                               deterministic = T, fidelity = 1, same_language = T){
  original.x <- original.text
  
  if (same_language) {lang.x <- create_language(range.low = range.low, 
                                                range.high = range.high)}
  
  holder.df <- data.frame()
  
  for (ii in 1:n_iterations){
    if (!same_language) {lang.x <- create_language(range.low = range.low, 
                                                   range.high = range.high)}
    recon.x <- match_story(original.x, lang.x, 
                           deterministic, fidelity) |> 
      mutate(story = ii)
    
    holder.df <- rbind(holder.df, recon.x)
    
    # original.x <- recon.x
  }
  
  final <- rbind(original.text |> 
                   mutate(t.new = t, distance = -1),
                 holder.df)
  
  return(final)
}

## DTW function ----
dtw.custom <- function(language = "same", translation = "iterative", 
                       style = "deterministic", num_translations = 30,
                       fidelity.val, translation.df){
  dtw.holder <- data.frame()
  original_n <- translation.df |> 
    filter(story == 1)
  
  for (iter in 2:num_translations){
    subset_n <- translation.df |> 
      filter(story == iter)
    
    dtw.res <- dtw(subset_n |> 
                     select(x,y),
                   original_n |> 
                     select(x,y))
    
    dtw.holder <- dtw.holder |> 
      rbind(data.frame(language, translation, iter, style, fidelity = fidelity.val,
                       dtw.normalizedDistance = dtw.res$normalizedDistance,
                       dtw.distance = dtw.res$distance))
  }
  return(dtw.holder)
}




# Original language & story ----
# TODO: Try a more clear story arc?
# TODO: Calculate DTW between original and translations

### More coherent plot
new_point <- function(x0, y0){
  x1 <- x0 + rnorm(n = 1, mean = 1, sd = 1)
  y1 <- y0 + rnorm(n = 1, mean = 1, sd = 2)

  return (c(x1, y1))
}

original <- data.frame(x = 0, y = 0)

length = 50

for (i in 1:length){
  old_points <- original[i,]
  new_points <- new_point(old_points$x, old_points$y)
  original <- rbind(original, new_points)
}

original <- original[2:(length + 1),] |> 
  mutate(t = 1:n(),
         story = 0,
         label = if_else(t == 1, "Start",
                         if_else(t == max(t), "End", NA)))

range.min <- min(c(original$x, original$y))
range.max <- max(c(original$x, original$y))

# ggplot(original, aes(x = x, y = y, color = t))+
#   geom_path(linewidth = 1)+
#   geom_point(aes(fill = t), color = "black", shape = 21)+
#   scale_color_distiller(name = "Sentence n")+
#   scale_fill_distiller(name = "Sentence n")+
#   # geom_label(aes(label = label), color = "black")+
#   ggtitle("Original story")

# original <- create_language() |> 
#   mutate(story = 0)







full.dtw <- data.frame()

# Same language, deterministic ----
## Iterative ----
n_translations <- 30

iterative <- iterate_translation(original, n_translations, range.min, range.max, deterministic = T, fidelity = 1, same_language = T)

# i.plot <- iterative |> 
#   filter(story != 0) |> 
#   mutate(story = factor(paste("Story", story),
#                         levels = paste("Story", min(story):n_translations))) |>  
#   ggplot(aes(x = x, y = y, color = t.new))+
#   geom_path(linewidth = 1)+
#   geom_point(aes(fill = t.new), color = "black", shape = 21)+
#   scale_color_distiller(name = "Sentence n")+
#   scale_fill_distiller(name = "Sentence n")+
#   # geom_label(aes(label = label), color = "black")+
#   facet_wrap(~factor(story),ncol = ceiling((n_translations+1)/2))+
#   ggtitle("Iterative translations")

# i.plot

## Scattershot ----

multi <- multi_translations(original, n_translations, range.min, range.max, deterministic = T, fidelity = 1, same_language = T)

# m.plot <- multi |> 
#   filter(story != 0) |> 
#   mutate(story = factor(paste("Story", story),
#                         levels = paste("Story", min(story):n_translations))) |> 
#   ggplot(aes(x = x, y = y, color = t.new))+
#   geom_path(linewidth = 1)+
#   geom_point(aes(fill = t.new), color = "black", shape = 21)+
#   scale_fill_distiller(palette = 2)+
#   scale_color_distiller(palette = 2)+
#   # geom_label(aes(label = label), color = "black")+
#   facet_wrap(~factor(story), ncol = ceiling((n_translations+1)/2))+
#   ggtitle("Scattershot translations")

# m.plot

full.dtw <- full.dtw |> 
  rbind(dtw.custom("same","iterative","deterministic",
                   n_translations, NA, iterative),
        dtw.custom("same","scattershot","deterministic",
                   n_translations, NA, multi))

# i.plot / m.plot+
#   plot_annotation(title = "Same language, deterministic", tag_levels = "A")
# 
# ggsave(filename = "model.deterministic-same.png",
#        path = "figures/model_figures",
#        units = "in",
#        dpi = 300,
#        width = 26,
#        height = 12)


# Different language, deterministic ----
## Iterative ----
iterative <- iterate_translation(original, n_translations, range.min, range.max, deterministic = T, fidelity = 1, same_language = F)

## Scattershot ----

multi <- multi_translations(original, n_translations, range.min, range.max, deterministic = T, fidelity = 1, same_language = F)

full.dtw <- full.dtw |> 
  rbind(dtw.custom("different","iterative","deterministic",
                   n_translations, NA, iterative),
        dtw.custom("different","scattershot","deterministic",
                   n_translations, NA, multi))


# Varying fidelity ----

for (fidelity.val in seq(10, 90, by = 10)){
  ## Same language, non-deterministic ----
  ### Iterative ----
  iterative <- iterate_translation(original, n_translations, 
                                   range.min, range.max, 
                                   deterministic = F, fidelity = fidelity.val,
                                   same_language = T)
  
  ### Scattershot ----
  
  multi <- multi_translations(original, n_translations, 
                              range.min, range.max, deterministic = F, 
                              fidelity = fidelity.val, same_language = T)
  
  full.dtw <- full.dtw |> 
    rbind(dtw.custom("same","iterative","non-deterministic", n_translations, 
                     fidelity.val, iterative),
          dtw.custom("same","scattershot","non-deterministic", n_translations, 
                     fidelity.val, multi))
  
  ## Different language, non-deterministic ----
  ### Iterative ----
  iterative <- iterate_translation(original, n_translations,
                                   range.min, range.max, deterministic = F, 
                                   fidelity = fidelity.val, same_language = F)
  
  ### Scattershot ----
  multi <- multi_translations(original, n_translations, 
                              range.min, range.max, deterministic = F, 
                              fidelity = fidelity.val, same_language = F)
  
  
  full.dtw <- full.dtw |> 
    rbind(dtw.custom("different","iterative","non-deterministic",
                     n_translations, fidelity.val, iterative),
          dtw.custom("different","scattershot","non-deterministic", 
                     n_translations, fidelity.val, multi))
}




# DTW plot ----

full.dtw |> 
  filter(style == "deterministic") |> 
  mutate(language = paste(language, "language")) |> 
  ggplot(aes(x = iter, y = dtw.normalizedDistance, 
             color = translation, shape = translation))+
  geom_point(alpha = 0.75, size = 2)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("blue","red"), name = "Translation\ntype")+
  scale_shape_manual(values = c(15, 17), name = "Translation\ntype")+
  xlab("Translation order")+
  ylab("Normalized DTW distance")+
  facet_wrap(~language)+
  ggtitle("Deterministic translation")

ggsave(filename = paste0("model.dtw-deterministic.png"),
       path = "figures/model_figures",
       units = "in",
       dpi = 300,
       width = 7,
       height = 5)

full.dtw |> 
  filter(style == "non-deterministic") |> 
  mutate(fidelity = factor(fidelity)) |> 
  mutate(language = paste(language, "language")) |> 
  ggplot(aes(x = iter, y = dtw.normalizedDistance, 
             color = fidelity))+
  geom_point(alpha = 0.75, size = 2)+
  geom_smooth(method = "lm")+
  scale_color_brewer(name = "Fidelity")+
  facet_grid(translation~language)+
  xlab("Translation order")+
  ylab("Normalized DTW distance")+
  ggtitle("Non-deterministic translation")+
  theme_dark()

ggsave(filename = paste0("model.dtw-non-deterministic.png"),
       path = "figures/model_figures",
       units = "in",
       dpi = 300,
       width = 7,
       height = 6)

full.dtw |> 
  filter(style == "non-deterministic") |> 
  mutate(fidelity = factor(fidelity)) |> 
  mutate(language = paste(language, "language")) |> 
  ggplot(aes(x = iter, y = dtw.distance, 
             color = fidelity))+
  geom_point(alpha = 0.75, size = 2)+
  geom_smooth(method = "lm")+
  scale_color_brewer(name = "Fidelity")+
  facet_grid(translation~language)+
  xlab("Translation order")+
  ylab("DTW distance (not normalized)")+
  ggtitle("Non-deterministic translation")+
  theme_dark()
