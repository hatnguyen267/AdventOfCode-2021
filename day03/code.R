## Part 1
string <- readr::read_csv("day03/advent3.csv", col_types = "c")
names(string) <- c("number")

split <- strsplit(string$number, "")

df <- as.data.frame(matrix(unlist(split), nrow=length(split), byrow=TRUE),
                    stringsAsFactors = FALSE)
df2 <- as.data.frame(sapply(df, function(x) as.numeric(as.character(x))))

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

ReverseMode <- function(x) {
  ux <- unique(x)
  ux[which.min(tabulate(match(x, ux)))]
}

gamma <- apply(df2, 2, Mode)
epsilon <- apply(df2, 2, ReverseMode)

gamma_hold <- c()
epsilon_hold <- c()
for(i in 1:12){
  gamma_hold[i] <- gamma[i]*(2^(12-i))
}
gamma_dec <- sum(gamma_hold)
for(i in 1:12){
  epsilon_hold[i] <- epsilon[i]*(2^(12-i))
}
epsilon_dec <- sum(epsilon_hold)
result <- epsilon_dec*gamma_dec

## Part 2
# Oxygen
oxygen <- df2

oxy_mode <- function(x){
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  if(length(which(tab == max(tab))) > 1){
    mode <- 1
  }
  else {
    mode <- ux[which.max(tab)]
  }
  mode
}

row <- nrow(df2)
  
for(i in 1:12){
  if(row > 1){
    oxygen <- oxygen[oxygen[[i]] == oxy_mode(oxygen[[i]]), ]
    row <- nrow(oxygen) 
  }
  if(row == 1){
    print("Filtering done")
    oxygen <- as.numeric(oxygen[1, ])
  }
}
  
# CO2

co2_mode <- function(x){
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  if(length(which(tab == min(tab))) > 1){
    mode <- 0
  }
  else {
    mode <- ux[which.min(tab)]
  }
  mode
} 

co2 <- df2
row <- nrow(df2)

for(i in 1:12){
  if(row > 1){
    co2 <- co2[co2[[i]] == co2_mode(co2[[i]]), ]
    row <- nrow(co2) 
  }
  if(row == 1){
    print("Filtering done")
    co2 <- as.numeric(co2)
  }
}

co2_hold <- c()
oxy_hold <- c()

for(i in 1:12){
  oxy_hold[i] <- oxygen[i]*(2^(12-i))
  co2_hold[i] <- co2[i]*(2^(12-i))
}

oxy_dec <- sum(oxy_hold)
co2_dec <- sum(co2_hold)
life_support <- oxy_dec*co2_dec
