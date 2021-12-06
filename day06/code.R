fish <- readLines("day06/fish.txt")
fish_v <- as.vector(as.numeric(unlist(strsplit(fish, ","))))

# Fish farm function
lantern <- function(x){
  fishfarm <- x -1
  if(0 %in% x){
    number_new_fish <- table(x)[1]
    # reset 0 fish
    reset <- replace(x, x == 0, 7)
    # adding new fish
    new_fish <- rep(9, times = number_new_fish)
    fishfarm <- c(reset, new_fish)
    fishfarm <- fishfarm - 1
  }
  fishfarm
}
# Part 1
day <- 1
fish_list <- list()
fish_list[[1]] <- fish_v
fish_tank <- c()

while(day < 81){
  print(paste("Day", day))
  fish_list[[2]] <- lantern(fish_list[[1]])
  fish_list[1] <- NULL
  day <- day + 1
  gc()
}

# Part 2
library(dplyr)
fish_table <- as.data.frame(fish_v) %>% 
  count(fish_v) %>% 
  add_row(fish_v = c(0, 6:8), n = 0) %>% 
  arrange(fish_v)

day <-1  

while (day < 257){
  print(paste("Day", day))
  day <- day + 1 
  new_fish <- fish_table[1, 2]
  fish_table$n <- lead(fish_table$n, default = new_fish)
  fish_table$n <- ifelse(fish_table$fish_v == 6, new_fish + fish_table[7, 2], 
                         fish_table$n)
}  

sum(fish_table$n)
