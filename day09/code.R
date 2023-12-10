## advent of code 2023
## day 9

# test <- as.list(readLines("/Users/polks/Desktop/misc/aoc2023/day09/test.txt"))
# data <- test

data <- readLines("/Users/polks/Desktop/misc/aoc2023/day09/data.txt")

# part 1
aListers <- lapply(data, function(x){
  y <- as.numeric(unlist(str_split(x, " ")))
  a <- y[length(y)]
  while(abs(sum(y - lag(y), na.rm = T)) > 0){
    y <- y - lag(y)
    a <- a + y[length(y)]
  }
  return(a)
})
sum(unlist(aListers))

# part 2
aListers <- lapply(data, function(x){
  y <- rev(as.numeric(unlist(str_split(x, " "))))
  a <- y[length(y)]
  while(abs(sum(y - lag(y), na.rm = T)) > 0){
    y <- y - lag(y)
    a <- a + y[length(y)]
  }
  return(a)
})
sum(unlist(aListers))
