# Day 5
library(tidyverse)
coords <- readLines("day05/data.txt") 

# Part 1
coords_df <- data.frame(coords) %>% 
  separate(coords, c("start_x", "start_y", "end_x", "end_y")) %>% 
  filter(start_x == end_x | start_y == end_y) %>% 
  rowwise() %>% 
  mutate(x = list(seq(start_x,end_x)),
         y = list(seq(start_y, end_y))) %>% 
  unnest(cols = everything()) %>% 
  count(x, y)

# Part2

coords_df2 <- data.frame(coords) %>% 
  separate(coords, c("start_x", "start_y", "end_x", "end_y")) %>% 
  rowwise() %>% 
  mutate(x = list(seq(start_x,end_x)),
         y = list(seq(start_y, end_y))) %>% 
  unnest(cols = everything()) %>% 
  count(x, y) %>% 
  filter(n>1)

