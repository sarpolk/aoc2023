## advent of code 2023
## day 2

# test <- as.list(readLines("/Users/polks/Desktop/misc/aoc2023/day2/test.txt"))
# data <- test

data <- as.list(readLines("/Users/polks/Desktop/misc/aoc2023/day2/data.txt"))

# part 1
# The Elf would first like to know which games would have been possible if the
# bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?

colors <- data.frame(nMax = c(12, 13, 14),
                     color = c("red", "green", "blue"))

toSum <- lapply(data, function(x){
  curDat <- unlist(str_split(str_replace(str_remove(x, "Game "), ":", ";"), "; "))
  i <- as.numeric(curDat[1])
  curSets <- as.list(curDat[2:length(curDat)])
  
  curSet <- lapply(curSets, function(y){
    curSet <- data.frame(V1 = y) %>% 
      mutate(V1 = strsplit(as.character(V1), ", ")) %>% 
      unnest(V1) %>% 
      separate(V1, c("n", "color"), sep = " ")
    
    check <- merge(curSet, colors) %>% 
      rowwise %>% 
      mutate(allowed = ifelse(as.numeric(n) <= nMax, T, F))
    
    !sum(check$allowed) < nrow(check)
  })
  ifelse(!sum(unlist(curSet)) < length(unlist(curSet)), i, 0)
})

sum(unlist(toSum))

# part 2
toSum <- lapply(data, function(x){
  curDat <- unlist(str_split(str_replace(str_remove(x, "Game "), ":", ";"), "; "))
  i <- as.numeric(curDat[1])
  curSets <- data.frame(V1 = curDat[2:length(curDat)])
  curNs <- curSets %>% 
    mutate(V1 = strsplit(as.character(V1), ", ")) %>% 
    unnest(V1) %>% 
    separate(V1, c("n", "color"), sep = " ") %>% 
    mutate(n = as.numeric(n)) %>% 
    group_by(color) %>% 
    filter(n == max(n, na.rm = T)) %>% 
    distinct()
  
  prod(curNs$n)
})

sum(unlist(toSum))
