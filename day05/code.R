## advent of code 2023
## day 5

test <- readLines("/Users/polks/Desktop/misc/aoc2023/day05/test.txt")
data <- test

# data <- as.list(readLines("/Users/polks/Desktop/misc/aoc2023/day05/data.txt"))

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

seeds <- data[1]
seeds <- as.numeric(unlist(str_split(str_remove(seeds, "seeds: "), " ")))
seeds <- as.list(as.data.frame(matrix(seeds, ncol = length(seeds) / 2)))
# seeds

# seed <- seeds[[1]]
# translation
finalLoc <- lapply(seeds, translation = translation, function(seed, translation){
  for (i in 1:7){
    ifelse(length(seed) > 2, 
           seed <- as.list(data.frame(matrix(seed, nrow = 2))),
           seed <- seed)
    ifelse(!is.list(seed), seed <- list(seed), seed <- seed)
    newLocList <- lapply(seed, function(s){
      seed1 <- s[1]
      seed2 <- s[1] + s[2] - 1
      newLoc <- lapply(translation[[i]], function(x){
        newSeeds <- NULL
        
        seed2_lower <- seed2 < x$sourceMin
        seed1_higher <- seed1 > x$sourceMax
        
        if (seed2_lower){
          # skip this range
          
        } else if (seed1_higher){
          # skip this range
          
        } else if (!seed1 <= x$sourceMin & !seed2 <= x$sourceMin & seed1 >= x$sourceMin & seed2 >= x$sourceMin){
          # yay perfect
          
          newSeeds <- c(seed1 + x$rangeDiff, seed2 - seed1 + 1)
        } else {
          # panic
          lower <- x$sourceMin - seed1 > 0 # seed range goes lower than source
          higher <- seed2 - x$sourceMax > 0 # seed range goes higher than source
          if (lower & !higher){
            seedRange1 <- c(seed1, x$sourceMin - 1)
            seedRange2 <- c(x$sourceMin, seed2) + x$rangeDiff
            
            newSeeds <- lapply(list(seedRange1, seedRange2), function(a){c(a[1], a[2] - a[1] + 1)})
          } else if (!lower & higher){
            seedRange1 <- c(seed1, x$sourceMax) + x$rangeDiff
            seedRange2 <- c(x$sourceMax + 1, seed2)
            
            newSeeds <- lapply(list(seedRange1, seedRange2), function(a){c(a[1], a[2] - a[1] + 1)})
          } else if (lower & higher){
            seedRange1 <- c(seed1, x$sourceMin - 1)
            seedRange2 <- c(x$sourceMin, x$sourceMax) + x$rangeDiff
            seedRange3 <- c(x$sourceMax + 1, seed2)
            
            newSeeds <- lapply(list(seedRange1, seedRange2, seedRange3), function(a){c(a[1], a[2] - a[1] + 1)})
          }
        }
        
        
        return(newSeeds)
        
        })
      newLoc[sapply(newLoc, is.null)] <- NULL
      ifelse(length(newLoc) == 0, newLoc <- s, newLoc <- newLoc)
      return(newLoc)
    })
    newLocList
    
    newLoc <- unlist(newLocList)
    ifelse(is.null(newLoc), seed <- seed, seed <- newLoc)
  }
  return(seed)
})

# min(unlist(finalLoc)[seq(1, length(unlist(finalLoc)), 2)]) # returning 0, this is incorrect
