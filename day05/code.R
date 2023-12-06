## advent of code 2023
## day 5

# test <- readLines("/Users/polks/Desktop/misc/aoc2023/day05/test.txt")
# data <- test

data <- as.list(readLines("/Users/polks/Desktop/misc/aoc2023/day05/data.txt"))

blanks <- c(which(data == ""), length(data) + 1)
nBlanks <- length(blanks)

seeds <- data[1]
seeds <- as.list(as.numeric(unlist(str_split(str_remove(seeds, "seeds: "), " "))))

dataList <- vector(mode = "list", length = nBlanks - 1)
i <- 1
while(i < nBlanks){
  dataList[[i]] <- data[(blanks[i] + 1):(blanks[i + 1] - 1)]
  i <- i+1
}

allLoc <- data.frame(X1 = c(0:99))
translation <- lapply(dataList, function(x){
  ranges <- as.list(x[2:length(x)])
  rangesFull <- lapply(ranges, function(y){
    range <- as.numeric(unlist(strsplit(y, split = " ")))
    rangeDiff <- range[1] - range[2]
    source <- c(range[2], range[2] + range[3] - 1)
    data.frame(sourceMin = source[1],
               sourceMax = source[2],
               rangeDiff)
  })
})

finalLoc <- lapply(seeds, translation = translation, function(seed, translation){
  for (i in 1:7){
    range <- do.call(rbind, translation[[i]]) %>% 
      filter(seed >= sourceMin & seed <= sourceMax)
    if (nrow(range) > 0){
      seed <- seed + range$rangeDiff
    }
  }
  return(seed)
})

min(unlist(finalLoc))

# part 2

