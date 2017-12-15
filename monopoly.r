gameboard <- data.frame(space = 1:40, title = c("Go" , "Mediterranean Avenue" , "Community Chest" , "Baltic Avenue" , "Income Tax" , "Reading Railroad" , "Oriental Avenue" , "Chance" , "Vermont Avenue" , "Connecticut Avenue" , "Jail" , "St. Charles Place" , "Electric Company" , "States Avenue" , "Virginia Avenue" , "Pennsylvania Railroad" , "St. James Place" , "Community Chest" , "Tennessee Avenue" , "New York Avenue" , "Free Parking" , "Kentucky Avenue" , "Chance" , "Indiana Avenue" , "Illinois Avenue" , "B & O Railroad" , "Atlantic Avenue" , "Ventnor Avenue" , "Water Works" , "Marvin Gardens" , "Go to jail" , "Pacific Avenue" , "North Carolina Avenue" , "Community Chest" , "Pennsylvania Avenue" , "Short Line Railroad" , "Chance" , "Park Place" , "Luxury Tax" , "Boardwalk"))

chancedeck <- data.frame(index = 1:15, card = c("Advance to Go" , "Advance to Illinois Ave." , "Advance to St. Charles Place" , "Advance token to nearest Utility" , "Advance token to the nearest Railroad" , "Take a ride on the Reading Railroad" , "Take a walk on the Boardwalk" , "Go to Jail" , "Go Back 3 Spaces" , "Bank pays you dividend of $50" , "Get out of Jail Free" , "Make general repairs on all your property" , "Pay poor tax of $15" , "You have been elected Chairman of the Board" , "Your building loan matures"))

communitydeck <- data.frame(index = 1:16, card = c("Advance to Go" , "Go to Jail" , "Bank error in your favor ??? Collect $200" , "Doctor's fees Pay $50" , "From sale of stock you get $45" , "Get Out of Jail Free" , "Grand Opera Night Opening" , "Xmas Fund matures" , "Income tax refund" , "Life insurance matures ??? Collect $100" , "Pay hospital fees of $100" , "Pay school tax of $150" , "Receive for services $25" , "You are assessed for street repairs" , "You have won second prize in a beauty contest" , "You inherit $100"))

## for chance deck
...
    if(chancedrawn == 1) 
        code changes player position to space 1  # advance to go
    if(chancedrawn == 2) 
        code changes player position to space 25 # advance to Illinois avenue
    if(chancedrawn == 3)
        code changes player position to space 12 # advance to St. Charles Place
    if(chancedrawn == 4)
        code changes player position to space X  # advance to nearest Utility
    if(chancedrawn == 5)
        code chagges player position to space X  # advance to nearest Railroad
    if(chancedrawn == 6)
        code changes player position to space 6  # advance to Reading Railroad
    if(chancedrawn == 7)
        code changes player position to space 40 # advance to Boardwalk
    if(chancedrawn == 8)
        code changes player position to space 11 # advance to Jail
    if(chancedrawn == 9)
        code changes player position to space -3
    if(chancedrawn == 11)
        no move, but with a chance to get out of Jail for free
    else
        no move
...

## for the community deck
...
    if(communitydrawn == 1)
        code changes player position to space 1  # advance to Go
    if(communitydrawn == 2)
        code changes player position to space 11 # advance to Jail
    if(communitydrawn == 6)
        no move, but get a card for free
    else
        no move
...

dice <- function(){
    faces <- sample(1:6, 2, replace=TRUE)
    if(faces[1] == faces[2]) doubles = TRUE
    else doubles = FALSE
    movement = sum(faces)
    return(list(faces=faces, doubles=doubles, movement=movement))
}

## check status of player to see whether it is in jail of not
check_in_jail <- function(a, b) {
    if (a == 31) {
        ## step on 31, send to jail immediately
        a <- 11
        if (b == 0) {
            ## if player has no jail free card
            c <- TRUE  ## then in jail
        } else {
            c <- FALSE  ## else use a jail free card
            b <- b - 1
        }
    } else {
        c <- FALSE  ## safely landed on except 31
    }
    ## return 3 variables, 1)current location, 2)number of jail free cards, 3)in
    ## jail or not
    return(list(space = a, jail_free = b, jail = c))
}

## draw card from community deck
community_deck <- function(a, b) {
    card <- sample(16, 1)
    if (card == 1) {
        ## advance to go
        a <- 1
    } else if (card == 2) {
        ## advance to 'go to jail', then to the jail
        a <- 31
    } else if (card == 6) {
        ## get a jail free card
        b <- b + 1
    }
    ## return 2 variables, 1)current location, 2)number of jail free cards
    return(list(space = a, jail_free = b))
}





## draw card from chance deck
chance_deck <- function(a, b) {
    card <- sample(15, 1)
    if (card == 1) {
        ## advance to go
        a <- 1
    } else if (card == 2) {
        ## advance to Illinois avenue
        a <- 25
    } else if (card == 3) {
        ## advance to St. Charles Place
        a <- 12
    } else if (card == 4) {
        ## move to nearest Utility
        if (a == 23) {
            a <- 29  ## move forward to Water Works
        } else {
            a <- 13  ## move forward to Electric Company
        }
    } else if (card == 5) {
        ## move to nearest Railroad
        if (a == 8) {
            a <- 16  ## move forward to Pennsylvania Railroad
        } else if (a == 23) {
            a <- 26  ## move forward to B & O Railroad
        } else {
            a <- 6  ## move forward to Reading Railroad
        }
    } else if (card == 6) {
        a <- 6  ## take a ride on the Reading Railroad
    } else if (card == 7) {
        a <- 40  ## take a walk on the Boardwalk
    } else if (card == 8) {
        a <- 31  ## go to 'Go to Jail', then send to jail by chenk_in_jail function
    } else if (card == 9) {
        a <- a - 3  ## backward 3 space
    } else if (card == 11) {
        b <- b + 1  ## get a jail free card without movement
    }
    ## return 3 variables, 1)current location, 2)
    return(list(space = a, jail_free = b))
}

gameboard$count <- 0  ## add column to count space occurence

for (j in 1:2000) {
    space <- 0  ## current location
    jail_free <- 0  ## number of free jail card
    jail <- FALSE  ## in jail or not
    double_try <- 0  ## number of rolls to escape from jail
    
    for (i in 1:100) {
        roll_1 <- dice()  ## 1st roll in a row
        if (!jail) {
            space <- space + roll_1[[3]]
            while (space > 40) space <- space - 40
            
            ## draw community chest card
            if (space == 3 | space == 18 | space == 34) {
                card_drawn <- community_deck(space, jail_free)
                space <- card_drawn[[1]]
                jail_free <- card_drawn[[2]]
            }
            
            ## draw chance card
            if (space == 8 | space == 23 | space == 37) {
                card_drawn <- chance_deck(space, jail_free)
                space <- card_drawn[[1]]
                jail_free <- card_drawn[[2]]
            }
            
            space1 <- space
            jail_detect <- check_in_jail(space, jail_free)
            space <- jail_detect[[1]]
            jail_free <- jail_detect[[2]]
            jail <- jail_detect[[3]]
            
            ## if landed on 31 then move to the jail, player ends his turn immediately
            if (space1 == 31) {
                gameboard$count[space] <- gameboard$count[space] + 1
                next
            }
            gameboard$count[space] <- gameboard$count[space] + 1
            
            ## if 1st roll got double and doesn't go to jail, roll dices again
            if (roll_1[[2]]) {
                roll_2 <- dice()  ## 2nd roll in a row
                space <- space + roll_2[[3]]
                while (space > 40) space <- space - 40
                
                if (space == 3 | space == 18 | space == 34) {
                  card_drawn <- community_deck(space, jail_free)
                  space <- card_drawn[[1]]
                  jail_free <- card_drawn[[2]]
                }
                
                if (space == 8 | space == 23 | space == 37) {
                  card_drawn <- chance_deck(space, jail_free)
                  space <- card_drawn[[1]]
                  jail_free <- card_drawn[[2]]
                }
                
                space1 <- space
                jail_detect <- check_in_jail(space, jail_free)
                space <- jail_detect[[1]]
                jail_free <- jail_detect[[2]]
                jail <- jail_detect[[3]]
                
                ## if landed on 31 then move to the jail, player ends his turn immediately
                if (space1 == 31) {
                  gameboard$count[space] <- gameboard$count[space] + 1
                  next
                }
                gameboard$count[space] <- gameboard$count[space] + 1
                
                ## if 2nd roll also got double and doesn't go to jail, roll dices again
                if (roll_2[[2]]) {
                  roll_3 <- dice()  ## 3rd roll in a row
                  if (roll_3[[2]]) {
                    ## if 3rd roll got double, send player to jail and end this turn
                    space <- 31
                    jail_detect <- check_in_jail(space, jail_free)
                    space <- jail_detect[[1]]
                    jail_free <- jail_detect[[2]]
                    jail <- jail_detect[[3]]
                    gameboard$count[space] <- gameboard$count[space] + 1
                    next
                  } else {
                    space <- space + roll_3[[3]]
                    while (space > 40) space <- space - 40
                    if (space == 3 | space == 18 | space == 34) {
                      card_drawn <- community_deck(space, jail_free)
                      space <- card_drawn[[1]]
                      jail_free <- card_drawn[[2]]
                    }
                    if (space == 8 | space == 23 | space == 37) {
                      card_drawn <- chance_deck(space, jail_free)
                      space <- card_drawn[[1]]
                      jail_free <- card_drawn[[2]]
                    }
                    jail_detect <- check_in_jail(space, jail_free)
                    space <- jail_detect[[1]]
                    jail_free <- jail_detect[[2]]
                    jail <- jail_detect[[3]]
                    gameboard$count[space] <- gameboard$count[space] + 1
                    next
                  }
                }
            }
        } else {
            ## if player in jail
            roll_1 <- dice()
            ## can only escape by rolling double or rolling 3 times
            if (roll_1[[2]] | double_try == 3) {
                space <- 11 + roll_1[[3]]
                jail <- FALSE
                double_try <- 0
                gameboard$count[space] <- gameboard$count[space] + 1
            } else {
                ## keep staying in jail without rolling double or less then 3 times
                double_try <- double_try + 1
                gameboard$count[space] <- gameboard$count[space] + 1
            }
        }
    }
}

gameboard$freq <- prop.table(gameboard$count)

library(dplyr)
arrange(gameboard, desc(freq))
##    space                 title count        freq
## 1     11                  Jail 28797 0.123460866
## 2     25       Illinois Avenue  6724 0.028827686
## 3      1                    Go  6566 0.028150295
## 4     21          Free Parking  6547 0.028068837
## 5     19      Tennessee Avenue  6488 0.027815887
## 6     20       New York Avenue  6429 0.027562937
## 7      6      Reading Railroad  6374 0.027327137
## 8     17       St. James Place  6299 0.027005591
## 9     26        B & O Railroad  6281 0.026928420
## 10    12     St. Charles Place  6084 0.026083825
## 11    29           Water Works  6062 0.025989505
## 12    13      Electric Company  6009 0.025762279
## 13    22       Kentucky Avenue  5957 0.025539340
## 14    16 Pennsylvania Railroad  5930 0.025423583
## 15    24        Indiana Avenue  5885 0.025230656
## 16    27       Atlantic Avenue  5874 0.025183496
## 17    28        Ventnor Avenue  5811 0.024913397
## 18    32        Pacific Avenue  5777 0.024767629
## 19    30        Marvin Gardens  5759 0.024690458
## 20    15       Virginia Avenue  5748 0.024643298
## 21    40             Boardwalk  5679 0.024347476
## 22    33 North Carolina Avenue  5532 0.023717245
## 23    18       Community Chest  5438 0.023314241
## 24    35   Pennsylvania Avenue  5341 0.022898374
## 25     5            Income Tax  5223 0.022392475
## 26    36   Short Line Railroad  5217 0.022366751
## 27     9        Vermont Avenue  5198 0.022285293
## 28    10    Connecticut Avenue  5134 0.022010907
## 29     7       Oriental Avenue  5120 0.021950885
## 30    34       Community Chest  5080 0.021779394
## 31    14         States Avenue  4983 0.021363527
## 32     4         Baltic Avenue  4738 0.020313143
## 33    38            Park Place  4629 0.019845829
## 34    39            Luxury Tax  4613 0.019777233
## 35     2  Mediterranean Avenue  4569 0.019588592
## 36     3       Community Chest  4173 0.017890829
## 37    23                Chance  3136 0.013444917
## 38     8                Chance  2062 0.008840376
## 39    37                Chance  1982 0.008497393
## 40    31            Go to jail     0 0.000000000
