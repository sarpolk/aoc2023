## advent of code 2023
## day 7

test <- as.list(readLines("/Users/polks/Desktop/misc/aoc2023/day07/test.txt"))
data <- test

data <- readLines("/Users/polks/Desktop/misc/aoc2023/day07/data.txt")

# part 1

cards <- as.list(c(2:9, "T", "J", "Q", "K", "A"))
cardsList <- lapply(data, cards = cards, function(x, cards){
  curData <- unlist(str_split(x, " "))
  hand <- curData[1]
  bid <- as.numeric(curData[2])
  
  cardCount <- unlist(lapply(cards, hand = hand, function(card, hand){
    str_count(hand, card)
  }))
  
  if (sum(grepl(5, cardCount)) == 1){
    return(c("five", hand, bid))
  } else if (sum(grepl(4, cardCount)) == 1){
    return(c("four", hand, bid))
  } else if (sum(grepl(3, cardCount)) == 1 & sum(grepl(2, cardCount)) == 1){
    return(c("full house", hand, bid))
  } else if (sum(grepl(3, cardCount)) == 1){
    return(c("three", hand, bid))
  } else if (sum(grepl(2, cardCount)) == 2){
    return(c("two pairs", hand, bid))
  } else if (sum(grepl(2, cardCount)) == 1){
    return(c("one pair", hand, bid))
  } else {
    return(c("you suck", hand, bid))
  }
})

data.frame(t(matrix(unlist(cardsList), nrow = 3))) %>% 
  rename(kind = X1, hand = X2, bid = X3) %>% 
  separate(hand, c("trash", "c1", "c2", "c3", "c4", "c5"), "") %>% 
  mutate(kind = factor(kind, levels = c("five", "four", "full house", "three", "two pairs", "one pair", "you suck")),
         c1 = factor(c1, levels = unlist(cards)),
         c2 = factor(c2, levels = unlist(cards)),
         c3 = factor(c3, levels = unlist(cards)),
         c4 = factor(c4, levels = unlist(cards)),
         c5 = factor(c5, levels = unlist(cards)),) %>% 
  arrange(desc(kind), c1, c2, c3, c4, c5) %>% 
  mutate(rank = row.names(.),
         score = as.numeric(bid)*as.numeric(rank)) %>% 
  summarise(sum(score))

# part 2
cards <- as.list(c("J", 2:9, "T", "Q", "K", "A"))
cardsList <- lapply(data, cards = cards, function(x, cards){
  curData <- unlist(str_split(x, " "))
  hand <- curData[1]
  bid <- as.numeric(curData[2])
  
  cardCount <- unlist(lapply(cards, hand = hand, function(card, hand){
    str_count(hand, card)
  }))
  
  jokers <- cardCount[1]
  cardCount <- cardCount[-1]
  maxCard <- max(which(cardCount == max(cardCount)))
  cardCount[maxCard] <- cardCount[maxCard] + jokers
  
  if (sum(grepl(5, cardCount)) == 1){
    return(c("five", hand, bid))
  } else if (sum(grepl(4, cardCount)) == 1){
    return(c("four", hand, bid))
  } else if (sum(grepl(3, cardCount)) == 1 & sum(grepl(2, cardCount)) == 1){
    return(c("full house", hand, bid))
  } else if (sum(grepl(3, cardCount)) == 1){
    return(c("three", hand, bid))
  } else if (sum(grepl(2, cardCount)) == 2){
    return(c("two pairs", hand, bid))
  } else if (sum(grepl(2, cardCount)) == 1){
    return(c("one pair", hand, bid))
  } else {
    return(c("you suck", hand, bid))
  }
})

data.frame(t(matrix(unlist(cardsList), nrow = 3))) %>% 
  rename(kind = X1, hand = X2, bid = X3) %>% 
  separate(hand, c("trash", "c1", "c2", "c3", "c4", "c5"), "") %>% 
  mutate(kind = factor(kind, levels = c("five", "four", "full house", "three", "two pairs", "one pair", "you suck")),
         c1 = factor(c1, levels = unlist(cards)),
         c2 = factor(c2, levels = unlist(cards)),
         c3 = factor(c3, levels = unlist(cards)),
         c4 = factor(c4, levels = unlist(cards)),
         c5 = factor(c5, levels = unlist(cards))) %>% 
  arrange(desc(kind), c1, c2, c3, c4, c5) %>% 
  mutate(rank = row.names(.),
         score = as.numeric(bid)*as.numeric(rank)) %>% 
  summarise(sum(score))


