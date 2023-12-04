## advent of code 2023
## day 3

# test <- matrix(readLines("/Users/polks/Desktop/misc/aoc2023/day3/test.txt"))
# data <- test

data <- matrix(readLines("/Users/polks/Desktop/misc/aoc2023/day3/data.txt"))

# part 1
lengthRow <- nchar(data[1])
data <- unlist(strsplit(data, ""))

findPositions <- function(x, find){
  nope <- str_sub(find, 1, 1)
  if(nope == "!"){
    find <- str_sub(find, 2, -1)
  } else {
    nope <- NULL
  }
  toFind <- paste0(nope, "grepl(find, x)")

  which(eval(parse(text = toFind)))
}

numPos <- findPositions(data, find = "[0-9]")
numbers <- as.data.frame(numPos) %>% 
  mutate(group = replace_na(numPos - lag(numPos), 9))

group <- 0
for(i in 1:nrow(numbers)){
  cur <- numbers[i,]
  if(cur$group != 1){
    group <- group + 1
  }
  numbers[i, "group"] <- group
}

symbols <- findPositions(data, find = "![0-9]")
free <- findPositions(data, find = "[.]")
symbols <- symbols[which(!symbols %in% free)]

adj <- c(-1, 1, 
         -lengthRow - 1, -lengthRow, -lengthRow + 1,
         lengthRow - 1, lengthRow, lengthRow + 1)

symbolsAdj <- merge(symbols, adj) %>% 
  mutate(adjPos = x + y)

numbers.list <- asplit(numbers, 1)
isAdj <- unlist(lapply(numbers.list, adj = adj, function(x, adj){
  x[["numPos"]] %in% symbolsAdj$adjPos
}))
numAdj <- cbind(numbers, isAdj) %>% 
  group_by(group) %>% 
  mutate(isAdj = ifelse(sum(isAdj > 0), 1, 0))

data.df <- data.frame(num = data) %>% 
  mutate(numPos = row.names(.))

numAdj <- merge(numAdj, data.df)
  
numAdj %>%
  filter(isAdj == 1) %>%
  group_by(group) %>% 
  mutate(n = n(),
         name = 1:n()) %>%
  select(group, n, num, name) %>%
  pivot_wider(names_from = name, names_prefix = "digit", values_from = num) %>%
  ungroup() %>%
  unite("num", c(digit1, digit2, digit3), sep = "") %>%
  mutate(num = as.numeric(str_remove_all(num, "NA"))) %>%
  summarise(sum(num))
# 528799


# part 2 (i cri)
numbers
gears <- data.frame(gearPos = findPositions(data, find = "[*]")) %>% 
  mutate(gearGroup = row.names(.))

gearsAdj <- merge(gears, adj) %>% 
  mutate(adjPos = gearPos + y)

whichAdj <- lapply(numbers.list, adj = adj, function(x, adj){
  x[["isAdj"]] <- x[["numPos"]] %in% gearsAdj$adjPos
  if(x[["numPos"]] %in% gearsAdj$adjPos){
    x[["gearGroup"]] <- gearsAdj[which(x[["numPos"]] == gearsAdj$adjPos), "gearGroup"]
  } else {
    x[["gearGroup"]] <- NA
  }
  x
})

numGear <- as.data.frame(do.call(rbind, whichAdj)) %>% 
  group_by(group) %>% 
  mutate(gearGroup = max(gearGroup, na.rm = T)) %>% 
  filter(!is.na(gearGroup)) %>%
  group_by(group, gearGroup) %>% 
  mutate(n = n()) %>% 
  group_by(gearGroup) %>% 
  mutate(oneGear = n() - n) %>% 
  filter(oneGear != 0) %>% 
  group_by(group, gearGroup) %>% 
  mutate(name = 1:n()) %>% 
  ungroup

merge(numGear, data.df) %>% 
  select(gearGroup, group, n, num, name) %>%
  group_by(group) %>% 
  pivot_wider(names_from = name, names_prefix = "digit", values_from = num) %>%
  ungroup() %>%
  unite("num", c(digit1, digit2, digit3), sep = "") %>%
  mutate(num = as.numeric(str_remove_all(num, "NA"))) %>%
  group_by(gearGroup) %>% 
  mutate(prod = prod(num)) %>% 
  distinct(gearGroup, prod) %>% 
  ungroup() %>% 
  summarise(sum(prod))

# 84907174
