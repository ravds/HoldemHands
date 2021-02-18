# My friend, Daksh Sharma, has since gone on to develop a C++ program inspired by HoldemHands, to which I have shifted my focus because of its 500-1000x speed improvement. 
# Despite optimizing my R code, simulations run at a speed of ~ 100/second, whereas Daksh's C++ version is able to currently run win/loss simulations at ~ 50 - 100k/second. 

# Bug note: Not passing a flop to f function will create situations where top2 will return 2 NA, which will lead to the detection of a two pair. 

# Use an environment to implement the bijection of cards to integers. Imagining ace as rank 1, 2 as rank 2, ... king as rank 13, map ascending cards (by rank) of the club suit to 0 to 12,
# diamond suit 13 to 25, hearts 26 to 38, and spades 39 to 51. In this way, rank and suit info are readily available:
# Rank is ((mapped integer) mod 13) + 1, and suit is the floor of the dividend of the mapped integer and 13. 
CardNumberPairs = new.env(hash = TRUE)
list2env(setNames(as.list(0:51),list("ac", "2c", "3c", "4c","5c","6c","7c","8c","9c","tc","jc","qc","kc","ad","2d","3d","4d","5d","6d","7d","8d","9d","td","jd","qd","kd","ah","2h","3h","4h","5h","6h","7h","8h","9h","th","jh","qh","kh","as","2s","3s","4s","5s","6s","7s","8s","9s","ts","js","qs","ks")),envir = CardNumberPairs)

# Interacting with the environment through a function just encapsulates the bijection
cardToNumber = function (card) {
  number = CardNumberPairs[[card]];
  return(number);
}

# Vectorized input and output. (Mapped Integer) mod 13 + 1
toRanks = function(hand) {
  return(hand%%13 + rep(1,length(hand)));
}
# Vectorized input and output. 
# 0 indicates clubs, 1 diamonds, 2 hearts, and 3 spades. 
# However, consistency is all that matters in a game like hold 'em where there is no hierarchy among suits. 
toSuits = function(hand) {
  return(floor(hand/13));
}

# top2 takes a vector or list and returns the counts of the two most frequently repeated elements. 
# Examples: 
# top2(c("daksh","daksh","robbie","joe","robbie")) 
# output: c(2,2)
# top2(c(5,5,5,1,1,5,1,2)) 
# output c(4,3)
# All the "rank-repeated" hands are either defined by the frequencies of their single most common rank (quads, set, and pair) or two most common ranks (full house and two pair).
# Additionally a flush can be detected by passing suits to top2 and checking for a number >= 5. 
# Table is a built-in function that returns a 2xn table for an n-dimensional vector or list where the first row is the sorted sequence (descending by frequency) of vector elements. 
# For example, (b,b,a,a,a,a,c,c) returns a table with its first row a,b,c, and its second row as the corresponding frequency of each element in that column, so 4,2,2
# In other words, a table of, [[a,b,c],[x,y,z]] means that a occurs x times, b occurs y times, and c occurs z times and x >= y >=z. I then turn this into a matrix. 
# Since as.matrix returns a vector (1xn) of only the frequencies (e.g. x,y, and z) when a table object is passed to it, I don't have to specify the row. 
top2 = function(hand) {
  return((as.matrix(table(hand))[1:2]));
}

# It's tempting to try to evaluate for a straight and a flush, but those two hands only imply a straight flush for a five card hand. 
# A 7 card (e.g. hole cards and flop, turn, and river) may contain both hands without actually containing a SF
hasStraightFlush = function(hand) {
  hand = sort(hand);
  handSize = length(hand);
  test = ((hand[handSize] - hand[handSize-4] == 4) && (toSuits(hand[handSize]) == toSuits(hand[handSize-4])))||((hand[handSize-1] - hand[handSize-5] == 4) && (toSuits(hand[handSize-1]) == toSuits(hand[handSize-5])))||((hand[handSize-2] - hand[handSize-6] == 4) && (toSuits(hand[handSize-2]) == toSuits(hand[handSize-6])));
# FALSE || NA || NA returns NA, but this just means that the latter two conditions were irrelevant (these are >5 card hand checks that wouldn't apply to a 5 card hand, for example), and so for our purposes 
# we ought to change that NA to FALSE
  if (is.na(test)) {test = FALSE;}
  return(test);
}

hasQuads = function(hand) {
  hand = toRanks(hand);
  return(4 %in% top2(hand));
}

hasFullHouse = function(hand) {
  hand = toRanks(hand);
  return(any((3:4 %in% top2(hand)))&&!(1 %in% top2(hand)));
}

hasFlush = function(hand) {
  hand = toSuits(hand);
  return(any((5:13 %in% top2(hand))));
}

hasStraight = function(hand) {
  hand = toRanks(hand);
  return(all((1:5 %in% hand))||all((2:6 %in% hand))||all((3:7 %in% hand))||all((4:8 %in% hand))||all((5:9 %in% hand))||all((6:10 %in% hand))||all((7:11 %in% hand))||all((8:12 %in% hand))||all((9:13 %in% hand))||all((c(1,10,11,12,13) %in% hand)));
}

hasSet = function(hand) {
  hand = toRanks(hand);
  return(any(3:4 %in% top2(hand)));
}

hasTwoPair = function(hand) {
  hand = toRanks(hand);
  return(any((2:4 %in% top2(hand)))&&!(1 %in% top2(hand)));
}

hasPair = function(hand) {
  hand = toRanks(hand);
  return(any((2:4 %in% top2(hand))))
}

# I could set the flop, turn, and river to NULL in the simulation function, f, using the built-in list function, but this would be less OOP and harder to read
setBoard = function(flop = NULL, turn = NULL, river = NULL) {
  boardvalues = list(flop, turn, river);
  return(boardvalues);
}
# Hold 'em almost never has more than 9 players, so we don't need more players than that, but we may as well always instantiate all 9 as it is cheap and will give us flexibility later on
setHoleCards = function(p1 = NULL, p2 = NULL, p3 = NULL, p4 = NULL, p5 = NULL, p6 = NULL, p7 = NULL, p8 = NULL, p9 = NULL) {
  holecardvalues = list(p1,p2,p3,p4,p5,p6,p7,p8,p9);
  return(holecardvalues);
}

shareBoardWithHoleCards = function(holecards, board) {
  hands = list(append(unlist(board),unlist(holecards[[1]])),append(unlist(board),unlist(holecards[[2]])),append(unlist(board),unlist(holecards[[3]])),append(unlist(board),unlist(holecards[[4]])),append(unlist(board),unlist(holecards[[5]])),append(unlist(board),unlist(holecards[[6]])),append(unlist(board),unlist(holecards[[7]])),append(unlist(board),unlist(holecards[[8]])),append(unlist(board),unlist(holecards[[9]])));
  return(hands)
}

# findBestHandMade returns a 2xn matrix where n is the number of players (including oneself). The first row contain 1's for every hand one has, and second row has only one 1 
# for the best hand made by all the opponents. 
findBestHandMade = function(numPlayers, hands = list(c(),c(),c(),c(),c(),c(),c(),c(),c())) {
  handsMade = matrix(0, nrow = 9, ncol = 9);
  print(hands);
  print(hasStraightFlush(hands[[1]]));
  handsMade[1,] = c(hasStraightFlush(hands[[1]]),hasQuads(hands[[1]]),hasFullHouse(hands[[1]]),hasFlush(hands[[1]]),hasStraight(hands[[1]]),hasSet(hands[[1]]),hasTwoPair(hands[[1]]),hasPair(hands[[1]]),1);
  
  bestHandMade = matrix(nrow = 2, ncol = 9);
  bestHandMade[1,] = handsMade[1,];
  bestHandMade[2,] = rep(0,9);
  
  for (i in 2:9) {handsMade[i,1] = hasStraightFlush(hands[[i]]);};
  if(any(handsMade[2:numPlayers,1])) {bestHandMade[2,1] = 1}
  else {for (i in 2:9) {handsMade[i,2] = hasQuads(hands[[i]]);};
    if(any(handsMade[2:numPlayers,2]==1)) {bestHandMade[2,2] = 1}
    else {for (i in 2:9) {handsMade[i,3] = hasFullHouse(hands[[i]]);}; 
      if(any(handsMade[2:numPlayers,3]==1)) {bestHandMade[2,3] = 1}
      else {for (i in 2:9) {handsMade[i,4] = hasFlush(hands[[i]]);}; 
        if(any(handsMade[2:numPlayers,4]==1)) {bestHandMade[2,4] = 1}
        else {for (i in 2:9) {handsMade[i,5] = hasStraight(hands[[i]]);}; 
          if(any(handsMade[2:numPlayers,5]==1)) {bestHandMade[2,5] = 1}
          else {for (i in 2:9) {handsMade[i,6] = hasSet(hands[[i]]);};
            if(any(handsMade[2:numPlayers,6]==1)) {bestHandMade[2,6] = 1}
            else {for (i in 2:9) {handsMade[i,7] = hasTwoPair(hands[[i]]);};
              if(any(handsMade[2:numPlayers,7]==1)) {bestHandMade[2,7] = 1}
              else {for (i in 2:9) {handsMade[i,8] = hasPair(hands[[i]]);};
                if(any(handsMade[2:numPlayers,8]==1)) {bestHandMade[2,8] = 1}
                else {bestHandMade[2,9] = 1};}}}}}}}
  
  return(bestHandMade);
}

# f is our main simulation method. We input our hole cards and at least the flop (the turn and river are each optional), we simulate numPlayers-1 hands, we use findBestHandMade to 
# figure out the best hand currently made by an opponent for each simulation, and divide this tally by the number of simulations, which should give us an unbiased estimator of the 
# real likelihood of each hand being the best made amongst numPlayers - 1 opponents. The first row contains 1's for every hand we have made, the second is an estimate of the pmf, and the third 
# is that pdf summed to get the cdf 

f = function(numPlayers, input, n = 500) {
  # remove the spaces from the card values argument, "ac 9c as 9s 8c" bedoming "ac9cas9s8c"
  input = gsub(" ", "",input, fixed = TRUE);
  
  # partitions "ac9cas9s8c" into "ac", "9c", "as, "9s", "8c", and converts missing data (turn or river here) into "1", which becomes NULL when passed to the environment 
  mHC1 = max(substr(input,1,2),"1");
  mHC2 = max(substr(input,3,4),"1");
  flop1 = max(substr(input,5,6),"1");
  flop2 = max(substr(input,7,8),"1");
  flop3 = max(substr(input,9,10),"1");
  turn = max(substr(input,11,12),"1");
  river = max(substr(input,13,14),"1");
  
  # convert alphanumeric card descriptors into integers or NULL references
  mHC1 = cardToNumber(mHC1);
  mHC2 = cardToNumber(mHC2);
  flop1 = cardToNumber(flop1);
  flop2 = cardToNumber(flop2);
  flop3 = cardToNumber(flop3);
  turn = cardToNumber(turn);
  river = cardToNumber(river);

  # creates a vector to contain one's hole cards ("ac 9c" becomes c(0,8))
  myHoleCards = c(mHC1, mHC2);
  # creates a vector for flop ("as 9s" 8c" become c(39, 47, 7))
  flop = c(flop1,flop2,flop3);
  # instantiates a deck (0 - 51 first 13 are ace of clubs to king of clubs, then ad, 2d ... kd, ah, 2h ... kh, as, 2s ... ks)
  deck = 0:51;
  # creates a list of our flop vector, turn integer, and river integer (again turn and river may be NULL)
  board = setBoard(flop, turn, river);
  # creates a list of all the hole card vectors at the table, setting unspecificed values to NULL 
  holeCards = setHoleCards(myHoleCards);
  # turns hole card vectors into lists and joins each hole card list with board list 
  hands = shareBoardWithHoleCards(holeCards, board);
  # removes the ah, 9h, as 9s, 8c from the deck because those have already been dealt 
  deck = setdiff(deck, unlist(hands));
  # initialize a 2 x 9 matrix where row 1 is us, row 2 is the best hand among all the opponents, in order to store our count
  bestHandsMade = matrix(0, nrow = 2, ncol = 9);
  # I put an option to evaluate with no personal hole cards, doesn't ever actually come into use because function always assumes first two cards are hole cards 
  if(is.null(myHoleCards)) {
    for (i in 1:n) {
      deck = sample(deck);
      holeCards = setHoleCards(deck[1:2], deck[3:4],deck[5:6],deck[7:8],deck[9:10], deck[11:12], deck[13:14],deck[15:16],deck[17:18]);
      hands = shareBoardWithHoleCards(holeCards, board);
      bestHandsMade = bestHandsMade + findBestHandMade(numPlayers, hands);
    }
  }
  # n is the number of simulations or deals, so now we deal cards to 8 other people and we use findBestHandMade to only look at the (numPlayers -1 number) of opponents 
  else {
    for(i in 1:n) {
      # uses the built in sample function to shuffle the deck 
      deck = sample(deck);
      holeCards = setHoleCards(myHoleCards, deck[1:2], deck[3:4],deck[5:6],deck[7:8],deck[9:10], deck[11:12], deck[13:14],deck[15:16]);
      hands = shareBoardWithHoleCards(holeCards, board);
      # findBestHandMade returns a matrix with a 1 for every hand you have (should probably just be best but i'll change that) and a 1 for the best hand 
      # amongst (all) the opponent(s)
      bestHandsMade = bestHandsMade + findBestHandMade(numPlayers, hands);
    }
  }
  # divide bestHandsMade matrix by number of simulations to get probabilities from counts
  bestHandsMade = bestHandsMade/n;
  # lets add a cumulative mass row which tell us the probability that, for example, a full house OR HIGHER is held 
  CMF = c(sum(bestHandsMade[2,1:1]), sum(bestHandsMade[2,1:2]), sum(bestHandsMade[2,1:3]), sum(bestHandsMade[2,1:4]), sum(bestHandsMade[2,1:5]), sum(bestHandsMade[2,1:6]), sum(bestHandsMade[2,1:7]), sum(bestHandsMade[2,1:8]), sum(bestHandsMade[2,1:9]))
  bestHandsMade = rbind(bestHandsMade, CMF);
  # data frames are just prettier ways of displaying the matrix 
  dataBestHands = data.frame(bestHandsMade);
  # add the column names to our data frame
  colnames(dataBestHands) = c("Straight Flush","Quads","Full House","Flush","Straight","Set","Two Pair","Pair","High Card");
  # add row names to our data frame
  rownames(dataBestHands) = c("What I have", "Best Hands PMF", "Best Hands CMF");
  # display the data frame
  View(dataBestHands);
  }

# for situations where we want to know the best hand that will be made BY THE RIVER at the flop or turn, we use a nearly identical simulation function g, whose only difference 
# is that it deals the turn if it is NULL and the flop if it is NULL 
g = function(numPlayers, input, n = 500) {
  input = gsub(" ", "",input, fixed = TRUE);
  
  mHC1 = max(substr(input,1,2),"1");
  mHC2 = max(substr(input,3,4),"1");
  flop1 = max(substr(input,5,6),"1");
  flop2 = max(substr(input,7,8),"1");
  flop3 = max(substr(input,9,10),"1");
  turn = max(substr(input,11,12),"1");
  river = max(substr(input,13,14),"1");
  
  mHC1 = cardToNumber(mHC1);
  mHC2 = cardToNumber(mHC2);
  flop1 = cardToNumber(flop1);
  flop2 = cardToNumber(flop2);
  flop3 = cardToNumber(flop3);
  turn = cardToNumber(turn);
  river = cardToNumber(river);
  
  myHoleCards = c(mHC1, mHC2);
  flop = c(flop1,flop2,flop3);
  # g simulates board too, so first it needs to see what yes to be dealt
  if(is.null(flop)) {simulateFlop = TRUE;} else simulateFlop = FALSE;
  if(is.null(turn)) {simulateTurn = TRUE;} else simulateTurn = FALSE;
  if(is.null(river)) {simulateRiver = TRUE;} else simulateRiver = FALSE;
  deck = 0:51;
  board = setBoard(flop, turn, river);
  holeCards = setHoleCards(myHoleCards);
  hands = shareBoardWithHoleCards(holeCards, board);
  deck = setdiff(deck, unlist(hands));
  bestHandsMade = matrix(0, nrow = 2, ncol = 9);
  if(is.null(myHoleCards)) {
    for (i in 1:n) {
      deck = sample(deck);
      if(simulateFlop) {flop = deck[19:21];}
      if(simulateTurn) {turn = deck[22:22];}
      if(simulateRiver) {river = deck[23:23];}
      board = setBoard(flop, turn, river);
      holeCards = setHoleCards(deck[1:2], deck[3:4],deck[5:6],deck[7:8],deck[9:10], deck[11:12], deck[13:14],deck[15:16],deck[17:18]);
      hands = shareBoardWithHoleCards(holeCards, board);
      bestHandsMade = bestHandsMade + findBestHandMade(numPlayers, hands);
    }
  }
  else {
    for(i in 1:n) {
      deck = sample(deck);
      if(simulateFlop) {flop = deck[17:19];}
      if(simulateTurn) {turn = deck[20:20];}
      if(simulateRiver) {river = deck[21:21];}
      board = setBoard(flop, turn, river);
      holeCards = setHoleCards(myHoleCards, deck[1:2], deck[3:4],deck[5:6],deck[7:8],deck[9:10], deck[11:12], deck[13:14],deck[15:16]);
      hands = shareBoardWithHoleCards(holeCards, board);
      bestHandsMade = bestHandsMade + findBestHandMade(numPlayers, hands);
    }
  }
  bestHandsMade = bestHandsMade/n;
  CMF = c(sum(bestHandsMade[2,1:1]), sum(bestHandsMade[2,1:2]), sum(bestHandsMade[2,1:3]), sum(bestHandsMade[2,1:4]), sum(bestHandsMade[2,1:5]), sum(bestHandsMade[2,1:6]), sum(bestHandsMade[2,1:7]), sum(bestHandsMade[2,1:8]), sum(bestHandsMade[2,1:9]))
  bestHandsMade = rbind(bestHandsMade, CMF);
  dataBestHands = data.frame(bestHandsMade);
  colnames(dataBestHands) = c("Straight Flush","Quads","Full House","Flush","Straight","Set","Two Pair","Pair","High Card");
  rownames(dataBestHands) = c("What I have", "Best Hands PMF", "Best Hands CMF");
  View(dataBestHands);
}

