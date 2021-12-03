string <- readr::read_csv("advent3.csv", col_types = "c")
names(string) <- c("number")

split <- strsplit(string$number, "")
split <- as.data.frame(unlist(split))
df <- data.frame(matrix(unlist(split), nrow=length(split), byrow=TRUE),
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
