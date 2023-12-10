## advent of code 2023
## day 8

# test <- readLines("/Users/polks/Desktop/misc/aoc2023/day08/test3.txt")
# data <- test

data <- readLines("/Users/polks/Desktop/misc/aoc2023/day08/data.txt")

sequence <- data[1]
instruct <- data.frame(x = data[3:length(data)]) %>% 
  mutate(start = str_sub(x, 1, 3),
         L = str_sub(x, 8, 10),
         R = str_sub(x, 13, 15)) %>% 
  select(-x)

# part 1
position <- "AAA"
i <- 1
steps <- 0
while(position != "ZZZ"){
  curInstruct <- str_sub(sequence, i, i)
  position <- (instruct %>% filter(start == position))[, curInstruct]
  ifelse(i == nchar(sequence), i <- 1, i <- i + 1)
  steps <- steps + 1
}
steps

# part 2 (cri)
positions <- as.list((instruct %>% 
                        mutate(startGroup = str_sub(start, 3, 3)) %>% 
                        filter(startGroup == "A"))$start)

# lcm hint from reddit
listZ <- lapply(positions, function(p){
  position <- p
  i <- 1
  steps <- 0
  while(str_sub(position, 3, 3) != "Z"){
    curInstruct <- str_sub(sequence, i, i)
    position <- (instruct %>% filter(start == position))[, curInstruct]
    i <- ifelse(i == nchar(sequence), 1, i + 1)
    steps <- steps + 1
  }
  return(steps)
})

# function stolen from stack overflow: https://stackoverflow.com/questions/49974601/r-prime-factorization
prime_factors <- function(x, i = 2, factors = NULL) {
  if(x < i) factors
  else if(! x %% i) prime_factors(x / i, i, c(factors, i))
  else  prime_factors(x, i + 1, factors)
}

options(scipen=999)
prod(unique(unlist(lapply(listZ, prime_factors))))
