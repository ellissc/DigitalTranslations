library(here)
setwd(here())
library(tidyverse)
library(TSP)
library(ggrepel)
library(patchwork)

theme_set(theme_bw(base_size = 14))

# General functions ----

# Generates a "language" as a 2D space of 30 points
create_language <- function(num_points = 30){
  story = data.frame(x = sample(1:50, num_points, replace = T),
                     y = sample(1:50, num_points, replace = T)) |> 
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
match_story <- function(original.x, language.other){
  reconstruction <- data.frame()
  
  for (ii in 1:nrow(original.x)){
    x.original <- original.x[ii,]$x
    y.original <- original.x[ii,]$y
    
    nearest <- language.other |> 
      mutate(distance = euclid(x,y,x.original, y.original)) |> 
      filter(distance == min(distance)) |> 
      slice_sample(n = 1)
    
    reconstruction <- rbind(reconstruction, nearest)
  }
  
  reconstruction <- reconstruction |> 
    mutate(t.new = 1:n(),
           label = if_else(t.new == 1, "Start",
                           if_else(t.new == max(t.new), "End", NA)))
  
  return(reconstruction)
}





# Iterative translation ----

## Helper functions ----

# Creates a translation chain of n_iterations
iterate_translation <- function(original.text, n_iterations = 4){
  original.x <- original.text
  
  holder.df <- data.frame()
  
  for (ii in 1:n_iterations){
    lang.x <- create_language(num_points = 50)
    recon.x <- match_story(original.x, lang.x) |> 
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
original <- create_language() |> 
  mutate(story = 0)

iterative <- iterate_translation(original, 9)

i.plot <- iterative |> 
  mutate(story = paste("Story", story)) |> 
  ggplot(aes(x = x, y = y, color = t.new))+
  geom_path(linewidth = 1)+
  scale_color_distiller()+
  geom_label(aes(label = label), color = "black")+
  facet_wrap(~factor(story), ncol = 5)+
  ggtitle("Stories translated iteratively")


## Longer chain ----

original <- create_language() |> 
  mutate(story = 0)

iterative <- iterate_translation(original, 49)

iterative |> 
  mutate(story = paste("Story", story)) |> 
  mutate(story = factor(story, levels = paste("Story", 0:49))) |> 
  ggplot(aes(x = x, y = y, color = t.new))+
  geom_path(linewidth = 1)+
  scale_color_distiller(name = "Timestep")+
  geom_label(aes(label = label), color = "black")+
  facet_wrap(~factor(story))+
  ggtitle("Stories translated iteratively")




# Translation with reference to the original ----

## Helper functions ----

# Creates a set of translations that can reference the original.
# Same as the above function, but the original.x is not updated.
multi_translations <- function(original.text, n_iterations = 4){
  original.x <- original.text
  
  holder.df <- data.frame()
  
  for (ii in 1:n_iterations){
    lang.x <- create_language(num_points = 50)
    recon.x <- match_story(original.x, lang.x) |> 
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
original2 <- create_language() |> 
  mutate(story = 0)

multi <- multi_translations(original2, 9)

m.plot <- multi |> 
  mutate(story = paste("Story", story)) |> 
  ggplot(aes(x = x, y = y, color = t.new))+
  geom_path(linewidth = 1)+
  scale_color_distiller(palette = 2)+
  geom_label(aes(label = label), color = "black")+
  facet_wrap(~factor(story), ncol = 5)+
  ggtitle("Stories translated with reference to original")



# Comparison ----

## Visual
m.plot / i.plot

## DTW 


