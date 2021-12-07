directions <- readLines("day02/data.txt")

library(dplyr)
library(tidyr)

# Part 1
dir_df <- data.frame(directions) %>% 
  separate(directions, into = c("where", "values")) %>% 
  group_by(where) %>% 
  summarize(total_values = sum(as.numeric(values))) 

(dir_df$total_values[dir_df$where == "down"] - dir_df$total_values[dir_df$where == "up"])* dir_df$total_values[dir_df$where == "forward"]
  
# Part 2

dir_df <- data.frame(directions) %>% 
  separate(directions, into = c("where", "values")) %>% 
  mutate(values = case_when(where == "up" ~ -(as.numeric(values)),
                            TRUE ~ as.numeric(values)))

aim <- 0
horiz <- 0
depth <- 0

for (i in 1:1000){
  if("forward" %in% dir_df[i, 1]) {
    horiz <- horiz + dir_df[i, 2]
    depth <- depth + dir_df[i, 2] * aim
  }
  else{
    aim <- aim + dir_df[i, 2]
  }
  print(paste0("New depth is ", depth, ", new aim is ", aim))
}

horiz * depth
