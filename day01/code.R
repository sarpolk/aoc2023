## advent of code 2023
## day 1

# test <- as.list(readLines("/Users/polks/Desktop/misc/aoc2023/day01/test.txt"))

data <- as.list(readLines("/Users/polks/Desktop/misc/aoc2023/day01/data.txt"))

# part 1
listNum <- lapply(data, function(x){
  num <- gsub("[^0-9.-]", "", x)
  as.numeric(paste0(str_sub(num, 1, 1), str_sub(num, -1, -1)))
  })

Reduce("+", listNum)

# part 2
# test2 <- as.list(readLines("/Users/polks/Desktop/misc/aoc2023/day1/test2.txt"))

nums <- list("one", "1", "two", "2", "three", "3", "four", "4", "five", "5",
             "six", "6", "seven", "7", "eight", "8", "nine", "9", "ten", "10")

listNum <- lapply(data, function(x){
  print(x)
  numList <- lapply(nums, x = x, function(x, num){unlist(gregexpr(num, x))})
  numArray <- unlist(numList)
  min <- min(numArray[which(numArray != -1)])
  max <- max(numArray[which(numArray != -1)])

  first <- unlist(nums[which(unlist(lapply(numList, function(x){min %in% x})))])
  last <- unlist(nums[which(unlist(lapply(numList, function(x){max %in% x})))])
  
  if (nchar(first) > 1){first <- nums[which(nums == first) + 1]}
  if (nchar(last) > 1){last <- nums[which(nums == last) + 1]}
  as.numeric(paste0(first, last))
})

Reduce("+", listNum)
