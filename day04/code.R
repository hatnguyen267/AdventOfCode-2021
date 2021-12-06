bingo <- read.table("day04/bingo.txt", sep = ",")
bingoboard <- read.table("day04/bingoboard.txt") 

# First part with some really unwieldy loops
bingo <- as.numeric(bingo[1, ])
bingoboard$ID <- rep(1:100, each = 5)
bingoboard$rows <- rep(1:5)

library(dplyr)
library(tidyr)

bingoboard <- bingoboard %>% 
  pivot_longer(cols = starts_with("V"), 
               names_to = "cols", 
               values_to = "num")

bingoboard$scores <- rep(0)
bingoboard$row_sum_scores <- rep(0)
bingoboard$col_sum_scores <- rep(0)

bingpot <- bingoboard
winset <- c()
index <- NA

for (i in 1:100){
  if(5 %in% unique(bingpot$row_sum_scores) || 5 %in% unique(bingpot$col_sum_scores)){
    break
  }
  else{
    for(r in 1:2500){
      bingpot$scores[r] <- ifelse(bingpot$num[r] %in% bingo[i], 1, bingpot$scores[r])
    }
    bingpot <- bingpot %>% 
      group_by(ID, cols) %>% 
      mutate(col_sum_scores = sum(scores)) %>% 
      ungroup() %>% 
      group_by(ID, rows) %>% 
      mutate(row_sum_scores = sum(scores))
    print(paste("Calling Bingo", bingo[i], "of index", i, "Max scores are (col)", 
                max(bingpot$col_sum_scores), "and (row)", max(bingpot$row_sum_scores)))
    index <- i
  }
}

finalscores <- bingpot %>% 
  group_by(ID) %>% 
  filter(any(col_sum_scores %in% 5),
         scores %in% 0) %>% 
  pull(num) %>% 
  sum() %>% 
  "*"(bingo[index])

# Part2 
losepot <- bingoboard
losepot$win <- rep(0)
losepot$win_turn <- rep(0)
index <- i

for (i in 1:100){
  if(all(losepot$win == 1)){
    break
  }
  else{
    for(r in 1:2500){
      losepot$scores[r] <- ifelse(losepot$num[r] %in% bingo[i], 1, losepot$scores[r])
    }
    losepot <- losepot %>% 
      group_by(ID, cols) %>% 
      mutate(col_sum_scores = sum(scores)) %>% 
      ungroup() %>% 
      group_by(ID, rows) %>% 
      mutate(row_sum_scores = sum(scores)) %>% 
      ungroup() %>% 
      group_by(ID) %>% 
      mutate(win = 
               case_when(5 %in% unique(col_sum_scores)|| 5 %in% unique(row_sum_scores) ~ 1,
                         TRUE ~ 0),
             win_turn = ifelse(win_turn == 0, 
                               ifelse(5 %in% unique(col_sum_scores)|| 5 %in% unique(row_sum_scores), 
                                      as.numeric(i), 0), win_turn))
    print(paste("Calling Bingo", bingo[i], "of index", i, "Max scores are (col)", 
                max(losepot$col_sum_scores), "and (row)", max(losepot$row_sum_scores)))
    index <- i
  }
}

losepot %>% 
  filter(win_turn == max(losepot$win_turn),
         scores == 0) %>% 
  pull(num) %>% 
  sum() %>% 
  "*"(bingo[max(losepot$win_turn)])
  
