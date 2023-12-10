## advent of code 2023
## day 6

test <- readLines("/Users/polks/Desktop/misc/aoc2023/day06/test.txt")
data <- test

data <- readLines("/Users/polks/Desktop/misc/aoc2023/day06/data.txt")

# part 1
dataUnlisted <- unlist(str_split(data, " "))
dataDF <- data.frame(t(matrix(dataUnlisted[which(dataUnlisted != "")], ncol = 2)[-1, ])) %>% mutate_all(as.numeric)
dataList <- as.list(dataDF)

winners <- lapply(dataList, function(x){
  curList <- vector(mode = "list", length = x[1] + 1)
  i <- 0
  while (i <= x[1]){
    distance <- i * (x[1] - i)
    curList[[i + 1]] <- distance > x[2]
    i <- i + 1
  }
  sum(unlist(curList))
})

prod(unlist(winners))

# part 2
dataUnlisted <- as.numeric(unlist(str_split(str_remove(paste(unlist(str_split(data, " ")), collapse = ""), "Time:"), "Distance:")))

x <- dataUnlisted
i <- 0
while (i <= x[1]){
  distance <- i * (x[1] - i)
  curList[[i + 1]] <- distance > x[2]
  i <- i + 1
} # slow but whatever
sum(unlist(curList))
