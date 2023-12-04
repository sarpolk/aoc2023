## advent of code 2023
## day 4

# test <- as.list(readLines("/Users/polks/Desktop/misc/aoc2023/day4/test.txt"))
# data <- test

data <- as.list(readLines("/Users/polks/Desktop/misc/aoc2023/day4/data.txt"))

# part 1
points <- lapply(data, function(x){
  winners <- as.numeric(unlist(str_split(unlist(str_split(x, "[:|]"))[2], " ")))
  mine <- as.numeric(unlist(str_split(unlist(str_split(x, "[:|]"))[3], " ")))
  
  match <- sum(winners[which(!is.na(winners))] %in% mine[which(!is.na(mine))])
  if(match == 0){
    p <- 0
  } else if(match >= 1){
    p <- 2 ^ (match - 1)
  }
  return(p)
})

sum(unlist(points))

# part 2
cards <- as.list(rep(1, length(data)))
for (i in 1:length(data)){
  x <- data[[i]]
  nCards <- cards[[i]]
  card <- as.numeric(str_remove(unlist(str_split(x, "[:|]"))[1], "Card "))
  winners <- as.numeric(unlist(str_split(unlist(str_split(x, "[:|]"))[2], " ")))
  mine <- as.numeric(unlist(str_split(unlist(str_split(x, "[:|]"))[3], " ")))
  
  match <- sum(winners[which(!is.na(winners))] %in% mine[which(!is.na(mine))])
  c <- 1
  while(c <= match){
    cards[[c + i]] <- cards[[c + i]] + 1 * nCards
    c <- c + 1
  }
}
sum(unlist(cards))
