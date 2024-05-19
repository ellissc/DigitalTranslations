library(here)
setwd(here())
library(tidyverse)
library(TSP)
library(ggrepel)
library(patchwork)

theme_set(theme_bw(base_size = 14))

# General functions ----

# Generates a "language" as a 2D space of 30 points
create_language <- function(num_points = 250, range.low = -50, range.high = 50){
  story = data.frame(x = sample(range.low:range.high, num_points, replace = T),
                     y = sample(range.low:range.high, num_points, replace = T)) |> 
    mutate(t = 1:n()) |> 
    mutate(label = if_else(t == 1, "Start",
                           if_else(t == max(t), "End", NA)))
  
  return(story)
}

# Euclidean distance function
euclid <- function(x1, y1, x2, y2) {
  sqrt((x2 - x1)^2 + (y2 - y1)^2)
}

# Recreates the story of original.x using language.other
#
# Goes through the points in original.x and calculates the distances to all the
# points in the language.other, selects the closest point, and repeats.
#
# TODO: Add a probabilistic version, for the nearest neighbor
match_story <- function(original.x, language.other, deterministic = T, determinism = 1){
  reconstruction <- data.frame()
  
  for (ii in 1:nrow(original.x)){
    x.original <- original.x[ii,]$x
    y.original <- original.x[ii,]$y
    
    if (deterministic){
      nearest <- language.other |> 
        mutate(distance = euclid(x,y,x.original, y.original)) |> 
        filter(distance == min(distance)) 
      # print(nearest)
      
      nearest <- nearest |> 
        slice_sample(n = 1)
      # print(nearest)
      
    } else {
      nearest <- language.other |> 
        mutate(distance = euclid(x,y,x.original, y.original))
      # print(nearest)
      
      nearest <- nearest |> 
        slice_sample(n = 1, weight_by = exp(distance*determinism)) 
        # e^(distance * x), as x goes to zero, everything has equal weight
        # as it increases, it becomes more extreme
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


# df <- data.frame(index = 1:10) |> 
#   mutate(weight.even = 1/10,
#          weight.uneven = index / sum(index))
# 
# x <- c()
# 
# y <- c()
# 
# for (i in 1:5000){
#   x.df <- df |> 
#     slice_sample(n = 1, weight_by = weight.even)
#   
#   y.df <- df |> 
#     slice_sample(n = 1, weight_by = weight.uneven)
#   
#   x <- append(x, x.df$index)
#   y <- append(y, y.df$index)
# }
# 
# hist(x)
# hist(y)


# Iterative translation ----

## Helper functions ----

# Creates a translation chain of n_iterations
iterate_translation <- function(original.text, n_iterations = 4, deterministic = T, determinism = 1){
  original.x <- original.text
  
  holder.df <- data.frame()
  
  for (ii in 1:n_iterations){
    lang.x <- create_language()
    recon.x <- match_story(original.x, lang.x, deterministic, determinism) |> 
      mutate(story = ii)
    
    holder.df <- rbind(holder.df, recon.x)
    
    original.x <- recon.x
  }
  
  final <- rbind(original.text |> 
                   mutate(t.new = t, distance = -1),
                 holder.df)
  
  return(final)
}

## Running the iterative translations ----

### More coherent plot
new_point <- function(x0, y0){
  x1 <- x0 + rnorm(n = 1, mean = 0, sd = 5)
  y1 <- y0 + rnorm(n = 1, mean = 0, sd = 5)
  
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

# original <- create_language() |> 
#   mutate(story = 0)

iterative <- iterate_translation(original, 4)

i.plot <- iterative |> 
  mutate(story = paste("Story", story)) |> 
  ggplot(aes(x = x, y = y, color = t.new))+
  geom_path(linewidth = 1)+
  geom_point(aes(fill = t.new), color = "black", shape = 21)+
  scale_color_distiller()+
  scale_fill_distiller()+
  geom_label(aes(label = label), color = "black")+
  facet_wrap(~factor(story), ncol = 5)+
  ggtitle("Stories translated iteratively")


## Longer chain ----


# iterative <- iterate_translation(original, 9)
# 
# iterative |> 
#   mutate(story = paste("Story", story)) |> 
#   mutate(story = factor(story, levels = paste("Story", 0:9))) |> 
#   ggplot(aes(x = x, y = y, color = t.new))+
#   geom_path(linewidth = 1)+
#   scale_color_distiller(name = "Timestep")+
#   geom_label(aes(label = label), color = "black")+
#   facet_wrap(~factor(story))+
#   ggtitle("Stories translated iteratively")




# Translation with reference to the original ----

## Helper functions ----

# Creates a set of translations that can reference the original.
# Same as the above function, but the original.x is not updated.
multi_translations <- function(original.text, n_iterations = 4, deterministic = T, determinism = 1){
  original.x <- original.text
  
  holder.df <- data.frame()
  
  for (ii in 1:n_iterations){
    lang.x <- create_language()
    recon.x <- match_story(original.x, lang.x, deterministic, determinism) |> 
      mutate(story = ii)
    
    holder.df <- rbind(holder.df, recon.x)
    
    # original.x <- recon.x
  }
  
  final <- rbind(original.text |> 
                   mutate(t.new = t, distance = -1),
                 holder.df)
  
  return(final)
}

## Running the simulation ----
# original2 <- create_language() |> 
#   mutate(story = 0)

multi <- multi_translations(original, 4)

m.plot <- multi |> 
  mutate(story = paste("Story", story)) |> 
  ggplot(aes(x = x, y = y, color = t.new))+
  geom_path(linewidth = 1)+
  geom_point(aes(fill = t.new), color = "black", shape = 21)+
  scale_fill_distiller(palette = 2)+
  scale_color_distiller(palette = 2)+
  geom_label(aes(label = label), color = "black")+
  facet_wrap(~factor(story), ncol = 5)+
  ggtitle("Stories translated with reference to original")



# Comparison ----

## Visual
deterministic <- m.plot / i.plot

## DTW 


# Non-deterministic -----
# Non-working at the moment.

iterative.nd <- iterate_translation(original, 4, F, 2)

i.plot.nd <- iterative.nd |> 
  mutate(story = paste("Story", story)) |> 
  ggplot(aes(x = x, y = y, color = t.new))+
  geom_path(linewidth = 1)+
  geom_point(aes(fill = t.new), color = "black", shape = 21)+
  scale_color_distiller()+
  scale_fill_distiller()+
  geom_label(aes(label = label), color = "black")+
  facet_wrap(~factor(story), ncol = 5)+
  ggtitle("Stories translated iteratively")


multi.nd <- multi_translations(original, 4, F, 2)

m.plot.nd <- multi.nd |> 
  mutate(story = paste("Story", story)) |> 
  ggplot(aes(x = x, y = y, color = t.new))+
  geom_path(linewidth = 1)+
  geom_point(aes(fill = t.new), color = "black", shape = 21)+
  scale_fill_distiller(palette = 2)+
  scale_color_distiller(palette = 2)+
  geom_label(aes(label = label), color = "black")+
  facet_wrap(~factor(story), ncol = 5)+
  ggtitle("Stories translated with reference to original")

nondeterministic <- m.plot.nd / i.plot.nd
nondeterministic
