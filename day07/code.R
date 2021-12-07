crabs <- readLines("day07/data.txt")

library(dplyr)
crabs_df <- as.data.frame(strsplit(crabs, ","), col.names = "pos") %>% 
  count(pos) %>% 
  arrange(desc(n)) %>% 
  mutate(pos = as.numeric(pos))

crabs_df$fuel <- rep(0)

# Part 1
for(i in 1:nrow(crabs_df)) {
  crabs_df$fuel[i] <- sum(abs((crabs_df$pos[-i] - crabs_df$pos[i]) * crabs_df$n[-i]))
}

# Part 2

crabs_df$fuel2 <- rep(0)
dif <- NA
crabsum <- rep(0, 651)

for(i in 1:nrow(crabs_df)) {
  for (j in 1:nrow(crabs_df)){
    dif <- abs(crabs_df$pos[j]- crabs_df$pos[i])
    if (dif == 0){
      crabsum[j] <- 0
    }
    else{
      crabsum[j] <- last(cumsum(1:dif)) * crabs_df$n[j]
    }
  }
  crabs_df$fuel2[i] <- sum(crabsum)
}


