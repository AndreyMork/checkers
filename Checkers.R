library(plotrix)

board <- list(
    c(10, 1, 10, 1, 10, 1, 10, 1),
    c(1, 10, 1, 10, 1, 10, 1, 10),
    c(10, 1, 10, 1, 10, 1, 10, 1),
    c(0, 10, 0, 10, 0, 10, 0, 10),
    c(10, 0, 10, 0, 10, 0, 10, 0),
    c(-1, 10, -1, 10, -1, 10, -1, 10),
    c(10, -1, 10, -1, 10, -1, 10, -1),
    c(-1, 10, -1, 10, -1, 10, -1, 10)
    
    # c(10, 0, 10, 1, 10, 0, 10, 1),
    # c(1, 10, 1, 10, 1, 10, 1, 10),
    # c(10, 1, 10, 0, 10, 1, 10, 0),
    # c(0, 10, 1, 10, 0, 10, 0, 10),
    # c(10, -1, 10, 0, 10, 0, 10, 0),
    # c(-1, 10, -1, 10, -1, 10, -1, 10),
    # c(10, -1, 10, -1, 10, -1, 10, -1),
    # c(-1, 10, -1, 10, -1, 10, -1, 10)
    
    # c(10, 1, 10, 1, 10, 1, 10, 1),
    # c(1, 10, 1, 10, 1, 10, 1, 10),
    # c(10, 1, 10, 1, 10, 1, 10, 1),
    # c(0, 10, 0, 10, 0, 10, 1, 10),
    # c(10, 0, 10, -1, 10, -1, 10, 0),
    # c(0, 10, 0, 10, 0, 10, -1, 10),
    # c(10, -1, 10, -1, 10, -1, 10, -1),
    # c(-1, 10, 0, 10, -1, 10, 0, 10)
    
    # c(10, 1, 10, 0, 10, 0, 10, 0),
    # c(0, 10, 1, 10, 0, 10, 0, 10),
    # c(10, 0, 10, 0, 10, 0, 10, 0),
    # c(0, 10, 0, 10, 0, 10, 0, 10),
    # c(10, 0, 10, 0, 10, 0, 10, 0),
    # c(3, 10, 0, 10, 0, 10, 0, 10),
    # c(10, 0, 10, 0, 10, 0, 10, -3),
    # c(0, 10, 0, 10, 0, 10, 0, 10)
    
)
checkersNum <- function(s) {
    n <- 0
    for (y in 1:8) {
        for (x in 1:8) {
            if (board[[y]][x] == s || board[[y]][x] == 3 * s)
                n <- n + 1
        }
    }
    return (n)
}
white <- checkersNum(-1)
black <- checkersNum(1)

board2 <<- list(
    c(10, 1, 10, 1, 10, 1, 10, 1),
    c(1, 10, 1, 10, 1, 10, 1, 10),
    c(10, 1, 10, 1, 10, 1, 10, 1),
    c(0, 10, 0, 10, 0, 10, 0, 10),
    c(10, 0, 10, 0, 10, 0, 10, 0),
    c(-1, 10, -1, 10, -1, 10, -1, 10),
    c(10, -1, 10, -1, 10, -1, 10, -1),
    c(-1, 10, -1, 10, -1, 10, -1, 10)
)
white2 <- 12
black2 <- 12
checkable2 <- list()
checked2 <- NA
side2 <- -1
oneMore2 <- F
queenIsChecked2 <- F

makeBackup <- function() {
    board2 <<- board
    white2 <<- white
    black2 <<- black
    checkable2 <<- checkable
    checked2 <<- checked
    side2 <<- side
    oneMore2 <<- oneMore
    queenIsChecked2 <<- queenIsChecked
}
backup <- function() {
    board <<- board2
    white <<- white2
    black <<- black2
    checkable <<- checkable2
    checked <<- checked2
    side <<- side2
    oneMore <<- oneMore2
    queenIsChecked <<- queenIsChecked2
}
checkBoard <- function() {
    f <- F
    checkable <<- list()
    for (i in 1:8) {
        for (j in 1:8){
            if (board[[i]][j] == side) {
                if (check(c(i, j))) {
                    f <- T
                    checkable[[length(checkable) + 1]]<<- c(i, j)
                }
            }
            else if (board[[i]][j] == side * 3) {
                if (queenCheck(c(i, j))) {
                    f <- T
                    checkable[[length(checkable) + 1]]<<- c(i, j)
                }
            }
        }
    }
    return (f)
}
isCheckable <- function(pos) {
    if (length(checkable) == 0)
        return (T)
    else {
        x <- pos[2]
        y <- pos[1]
        for (i in 1:length(checkable)) {
            if (y == checkable[[i]][1] && x == checkable[[i]][2])
                return (T)
        }
    }
    return (F)
}
getCoordinates <- function(){
    N = locator(n = 1)
    for(i in 1 : 64)
        if((N[1] > -40 + ((i - 1) %% 8) * 10) && (N[2] < 40 - ((i - 1) %/% 8) * 10) &&
           (N[1] < -30 + ((i - 1) %% 8) * 10) && (N[2] > 30 - ((i - 1) %/% 8) * 10))
            return(c((1 + (i - 1) %/% 8), 1 + ((i - 1) %% 8)))
    if((N[1] < -40) && (N[1] > -50) && (N[2] < 50) && (N[2] > 40))
        return (0)
    if((N[1] > 40) && (N[1] < 50) && (N[2] < 50) && (N[2] > 40))
        return (9)
    if((N[1] > -38) && (N[1] < -34) && (N[2] < -40) && (N[2] > -50))
        return (11)
    if((N[1] >  34) && (N[1] <  38) && (N[2] < -40) && (N[2] > -50))
    {}
}
drawBoard <- function(){
    plot(0, 0, xlim = c(-60, 60), ylim = c(-60, 60), type = "n",
         bty = "n", xaxt = "n", yaxt = "n", col.lab = "#FFFFFF")
    for(i in 1 : 8)
        for(j in 1 : 8)
        {
            Js = (j - 1) * 10
            Is = (i - 1) * 10
            Cell = board[[i]][j]
            if(Cell == 10)
            {
                rect(-40 + Js, 40 - Is, 
                     -30 + Js, 30 - Is, col = "#d4a550", border = "#d4a550") 
            }
            else if(Cell == 0)
            {
                rect(-40 + Js, 40 - Is, 
                     -30 + Js, 30 - Is, col = "#5F3405", border = "#5B3501") 
            }
            else if(Cell == 1)
            {
                rect(-40 + Js, 40 - Is, 
                     -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501") 
                draw.circle(-35 + Js, 35 - Is, 4, col = "#090909", lwd = 2, border = "#252525")
                draw.circle(-35 + Js, 35 - Is, 3, col = "#090909", lwd = 2, border = "#252525")
                draw.circle(-35 + Js, 35 - Is, 2, col = "#101010", lwd = 2, border = "#252525")
            }
            else if(Cell == -1)
            {
                rect(-40 + Js, 40 - Is, 
                     -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501") 
                draw.circle(-35 + Js, 35 - Is, 4, col = "#A4A4A4", lwd = 2, border = "#D5D5D5")
                draw.circle(-35 + Js, 35 - Is, 3, col = "#A4A4A4", lwd = 2, border = "#D5D5D5")
                draw.circle(-35 + Js, 35 - Is, 2, col = "#B9B9B9", lwd = 2, border = "#D5D5D5")
            }
            else if(Cell == 2)
            {
                rect(-40 + Js, 40 - Is, 
                     -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501") 
                draw.circle(-35 + Js, 35 - Is, 4, col = "#090909", lwd = 2, border = "#252525")
                draw.circle(-35 + Js, 35 - Is, 3, col = "#090909", lwd = 2, border = "#252525")
                draw.circle(-35 + Js, 35 - Is, 2, col = "#101010", lwd = 2, border = "#252525")
                draw.circle(-35 + Js, 35 - Is, 4, lwd = 2, border = "#008500")
            }
            else if(Cell == -2)
            {
                rect(-40 + Js, 40 - Is, 
                     -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501") 
                draw.circle(-35 + Js, 35 - Is, 4, col = "#A4A4A4", lwd = 2, border = "#D5D5D5")
                draw.circle(-35 + Js, 35 - Is, 3, col = "#A4A4A4", lwd = 2, border = "#D5D5D5")
                draw.circle(-35 + Js, 35 - Is, 2, col = "#B9B9B9", lwd = 2, border = "#D5D5D5")
                draw.circle(-35 + Js, 35 - Is, 4, lwd = 2, border = "#008500")
            }
            else if(Cell == 3)
            {
                rect(-40 + Js, 40 - Is, 
                     -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501") 
                draw.circle(-35 + Js, 35 - Is, 4, col = "#101010", lwd = 2, border = "#303030")
            }
            else if(Cell == -3)
            {
                rect(-40 + Js, 40 - Is, 
                     -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501") 
                draw.circle(-35 + Js, 35 - Is, 4, col = "#A9A9A9", lwd = 2, border = "#D9D9D9")
            }
            else if(Cell == 4)
            {
                rect(-40 + Js, 40 - Is, 
                     -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501") 
                draw.circle(-35 + Js, 35 - Is, 4, col = "#101010", lwd = 2, border = "#303030")
                draw.circle(-35 + Js, 35 - Is, 4, lwd = 2, border = "#008500")
            }
            else if(Cell == -4)
            {
                rect(-40 + Js, 40 - Is, 
                     -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501") 
                draw.circle(-35 + Js, 35 - Is, 4, col = "#A9A9A9", lwd = 2, border = "#D9D9D9")
                draw.circle(-35 + Js, 35 - Is, 4, lwd = 2, border = "#008500")
            }
            else if((i == 9 - side || i == -1 * side) && !queenIsChecked && !is.na(checked))
            {
                rect(-40 + Js, 40 - Is, 
                     -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501") 
                draw.circle     (0 - 35 + Js,   0 + 35 - Is, 4,   col = "#d4a550", border = "#48036F", lwd = 2)
                draw.circle (-2.25 - 35 + Js, 1.3 + 35 - Is, 0.5, col = "#5B3501", border = "#5B3501")
                draw.circle (-0.85 - 35 + Js, 2.5 + 35 - Is, 0.5, col = "#5B3501", border = "#5B3501")
                draw.circle  (0.85 - 35 + Js, 2.5 + 35 - Is, 0.5, col = "#5B3501", border = "#5B3501")
                draw.circle  (2.25 - 35 + Js, 1.3 + 35 - Is, 0.5, col = "#5B3501", border = "#5B3501")
                polygon(c(-2.25 - 35 + Js,    0 - 35 + Js, -1.5 - 35 + Js), 
                        c(  1.3 + 35 - Is, -1.9 + 35 - Is, -1.9 + 35 - Is), 
                        col = "#5B3501", border = "#5B3501")
                polygon(c(-0.85 - 35 + Js,  0.7 - 35 + Js, -1.2 - 35 + Js), 
                        c(2.5  + 35 - Is, -1.9 + 35 - Is, -1.9 + 35 - Is), 
                        col = "#5B3501", border = "#5B3501")
                polygon(c(0.85 - 35 + Js,  1.2 - 35 + Js,  -0.7 - 35 + Js), 
                        c( 2.5 + 35 - Is, -1.9 + 35 - Is,  -1.9 + 35 - Is), 
                        col = "#5B3501", border = "#5B3501")
                polygon(c(2.25 - 35 + Js,   1.5 - 35 + Js,    0 - 35 + Js), 
                        c( 1.3  + 35 - Is, -1.9 + 35 - Is, -1.9 + 35 - Is), 
                        col = "#5B3501", border = "#5B3501")
                rect(-1.7 - 35 + Js, -2.4 + 35 - Is, 
                     1.7 - 35 + Js, -2.7 + 35 - Is, col = "#5B3501", border = "#5B3501")
            }
            else if(Cell == 5)
            {
                rect(-40 + Js, 40 - Is, 
                     -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501")
                draw.circle     (0 - 35 + Js,  0 + 35 - Is, 4,   col = "#d4a550", border = "#008500", lwd = 2)
                draw.circle     (0 - 35 + Js, -1 + 35 - Is, 1.5, col = "#5B3501", border = "#5B3501")
                draw.circle (-2.25 - 35 + Js,  1 + 35 - Is, 0.5, col = "#5B3501", border = "#5B3501")
                draw.circle (-0.85 - 35 + Js,  2 + 35 - Is, 0.5, col = "#5B3501", border = "#5B3501")
                draw.circle  (0.85 - 35 + Js,  2 + 35 - Is, 0.5, col = "#5B3501", border = "#5B3501")
                draw.circle  (2.25 - 35 + Js,  1 + 35 - Is, 0.5, col = "#5B3501", border = "#5B3501")
            }
            else if(Cell == -5)
            {
                rect(-40 + Js, 40 - Is, 
                     -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501") 
                draw.circle (0 - 35 + Js,   0 + 35 - Is, 4,   col = "#d4a550", border = "#A60000", lwd = 2)
                draw.circle (0 - 35 + Js, 0.8 + 35 - Is, 2.2, col = "#5B3501", border = "#5B3501")
                draw.circle(-1 - 35 + Js,   1 + 35 - Is, 0.8, col = "#d4a550", border = "#5B3501")
                draw.circle (1 - 35 + Js,   1 + 35 - Is, 0.8, col = "#d4a550", border = "#5B3501")
                draw.circle (0 - 35 + Js, -1.2 + 35 - Is, 1.5, col = "#5B3501", border = "#5B3501")
                segments(-0.5 - 35 + Js, -1.7 + 35 - Is,  0.5 - 35 + Js, -0.7 + 35 - Is, col = "#d4a550")
                segments (0.5 - 35 + Js, -1.7 + 35 - Is, -0.5 - 35 + Js, -0.7 + 35 - Is, col = "#d4a550")
            }
        }
    Row = c("A", "B", "C", "D", "E", "F", "G", "H")
    rect(-50,  50, -40, -50, col = "#d4a550", lwd = 3, border = "#5B3501")
    rect( 40,  50,  50, -50, col = "#d4a550", lwd = 3, border = "#5B3501")
    rect(-50,  50,  50,  40, col = "#d4a550", lwd = 3, border = "#5B3501")
    rect(-50, -40,  50, -50, col = "#d4a550", lwd = 3, border = "#5B3501")
    rect(-32,  -42, 32, -48, col = "#d4a550", lwd = 3, border = "#5B3501")
    polygon(c(-37.5, -34.5, -34.5), c(-45, -42, -48), col = "#5B3501", border = "#d4a550")
    polygon(c( 37.5,  34.5,  34.5), c(-45, -42, -48), col = "#5B3501", border = "#d4a550")
    polygon(c(-36, -33, -33), c(-45, -42, -48), col = "#5B3501", border = "#d4a550")
    polygon(c( 36,  33,  33), c(-45, -42, -48), col = "#5B3501", border = "#d4a550")  
    segments(-40, 50, -40, -50, lwd = 3, col = "#5B3501")
    segments( 40, 50,  40, -50, lwd = 3, col = "#5B3501")
    for(i in 1 : 8)
    {
        text(-35 + 10 * (i - 1),  45, Row[i], font = 2)
        text(-45, 35 - 10 * (i - 1), i, font = 2)
        text( 45, 35 - 10 * (i - 1), i, font = 2)
    }
    text( 45, 45, "X", cex = 2, col = "#A60000")
    text(-45, 45, "C", cex = 2, col = "#008500")
    points(-43.5, 46, pch = 17, cex = 1.5, col = "#008500")
    text(-45, -45, white, cex = 1.5, col = "#888888")        
    text( 45, -45, black, cex = 1.5, col = "#888888")
    if (nchar(mes) != 0)
        text(0, -45, mes)
    else 
        text(0, -45, "Checkers")
    if (side == -1)
        text(-45, -45, white, cex = 1.5, col = "#FFFFFF")
    else
        text( 45, -45, black, cex = 1.5, col = "#000000")
}

move <- function(start, finish) {
    if (finish[1] == 9 - side || finish[1] == -1 * side) { # && board[[finish[1]]][finish[2]] == side * 3)
        des <- (finish - start) / 2
        eatFlag <- F
        if (board[[finish[1]]][finish[2]] == -5) {
            eatFlag <- T
            uncheck(start)
            board[[start[1] + des[1]]][start[2] + des[2]] <- 0
            if (side == 1)
                white <<- white - 1
            else
                black <<- black - 1
            moveAnimation(start, finish, T)
        }
        else {
            uncheck(start)
            moveAnimation(start, finish, F)
        }
        
        board[[start[1]]][start[2]] <- 0
        board[[finish[1]]][finish[2]] <- 3 * side
        board <<- board
        oneMore <<- F
        checked <<- NA
        if (!oneMore && queenCheck(finish) && eatFlag) {
            side <<- side * -1
            oneMore <<- T
            checked <<- finish
            queenIsChecked <<- T
        }
    }
    else if (board[[finish[1]]][finish[2]] == 5) {
        uncheck(start)
        moveAnimation(start, finish, F)
        board[[finish[1]]][finish[2]] <- board[[start[1]]][start[2]]
        board[[start[1]]][start[2]] <- 0
    }
    else if (board[[finish[1]]][finish[2]] == -5) {
        uncheck(start)
        moveAnimation(start, finish, T)
        des <- (finish - start) / 2
        board[[finish[1]]][finish[2]] <- board[[start[1]]][start[2]]
        board[[start[1] + des[1]]][start[2] + des[2]] <- 0
        board[[start[1]]][start[2]] <- 0
        if (board[[finish[1]]][finish[2]] == 1)
            white <<- white - 1
        else
            black <<- black - 1
        if (check(finish)) {
            side <<- side * -1
            oneMore <<- T
            checked <<- finish
        }
        else{
            oneMore <<- F
            checked <<- NA
        }
    }
    side <<- side * -1
    board <<- board
}
check <- function(pos, changeBoard = F){
    f <- F
    aN <- length(checkable) == 0
    s <- side
    board[[pos[1]]][pos[2]] <- 2 * s
    if (changeBoard)
        checked <<- pos
    x <- (pos + c(1, 1) * s)[2]
    y <- (pos + c(1, 1) * s)[1]
    
    if (x > 0 && x < 9 && y > 0 && y < 9 && board[[y]][x] == 0 && aN && !oneMore)
        board[[y]][x] <- 5
    else if (x > 1 && x < 8 && y > 1 && y < 8 &&
             (board[[y]][x] == -1 * s || board[[y]][x] == -3 * s)
             && board[[y + s]][x + s] == 0) {
        board[[y + s]][x + s] <- -5
        f <- T
    }
    
    
    x <- (pos + c(1, -1) * s)[2]
    y <- (pos + c(1, -1) * s)[1]
    
    if (x > 0 && x < 9 && y > 0 && y < 9 && board[[y]][x] == 0 && aN && !oneMore)
        board[[y]][x] <- 5
    else if (x > 1 && x < 8 && y > 1 && y < 8 &&
             (board[[y]][x] == -1 * s || board[[y]][x] == -3 * s)
             && board[[y + s]][x - s] == 0) {
        board[[y + s]][x - s] <- -5
        f <- T
    }
    
    x <- (pos + c(-1, -1) * s)[2]
    y <- (pos + c(-1, -1) * s)[1]
    
    if (x > 1 && x < 8 && y > 1 && y < 8 &&
        (board[[y]][x] == -1 * s || board[[y]][x] == -3 * s)
        && board[[y - s]][x - s] == 0) {
        board[[y - s]][x - s] <- -5
        f <- T
    }
    
    x <- (pos + c(-1, 1) * s)[2]
    y <- (pos + c(-1, 1) * s)[1]
    
    if (x > 1 && x < 8 && y > 1 && y < 8 &&
        (board[[y]][x] == -1 * s || board[[y]][x] == -3 * s)
        && board[[y - s]][x + s] == 0) {
        board[[y - s]][x + s] <- -5
        f <- T
    }
    
    if (changeBoard)
        board <<- board
    return(f)
}
uncheck <- function(pos){
    s <- side
    board[[pos[1]]][pos[2]] <- board[[pos[1]]][pos[2]] - s
    checked <<- NA
    x <- (pos + c(1, 1) * s)[2]
    y <- (pos + c(1, 1) * s)[1]
    if (x > 0 && x < 9 && y > 0 && y < 9 && board[[y]][x] == 5)
        board[[y]][x] <- 0
    else if (x > 1 && x < 8 && y > 1 && y < 8 &&
             (board[[y]][x] == -1 * s || board[[y]][x] == -3 * s)
             && board[[y + s]][x + s] == -5)
        board[[y + s]][x + s] <- 0
    
    x <- (pos + c(1, -1) * s)[2]
    y <- (pos + c(1, -1) * s)[1]
    
    if (x > 0 && x < 9 && y > 0 && y < 9 && board[[y]][x] == 5)
        board[[y]][x] <- 0
    else if (x > 1 && x < 8 && y > 1 && y < 8 &&
             (board[[y]][x] == -1 * s || board[[y]][x] == -3 * s)
             && board[[y + s]][x - s] == -5)
        board[[y + s]][x - s] <- 0
    
    
    x <- (pos + c(-1, -1) * s)[2]
    y <- (pos + c(-1, -1) * s)[1]
    
    if (x > 1 && x < 8 && y > 1 && y < 8 &&
        (board[[y]][x] == -1 * s || board[[y]][x] == -3 * s)
        && board[[y - s]][x - s] == -5)
        board[[y - s]][x - s] <- 0
    
    x <- (pos + c(-1, 1) * s)[2]
    y <- (pos + c(-1, 1) * s)[1]
    
    if (x > 1 && x < 8 && y > 1 && y < 8 &&
        (board[[y]][x] == -1 * s || board[[y]][x] == -3 * s)
        && board[[y - s]][x + s] == -5)
        board[[y - s]][x + s] <- 0
    
    
    board <<- board
}
queenCheck <- function(pos, changeBoard = F) {
    board <- queenUncheck(pos, F)
    board[[pos[1]]][pos[2]] <- 4 * side
    directions <- data.frame(c(1, 1), c(1, -1), c(-1, 1), c(-1, -1))
    f <- F
    for (i in 1:4) {
        x <- pos[2] + directions[1, i]
        y <- pos[1] + directions[2, i]
        while (x > 0 && x < 9 && y > 0 && y < 9 && board[[y]][x] == 0) {
            x <- x + directions[1, i]
            y <- y + directions[2, i]
        }
        
        
        if (x > 0 && x < 9 && y > 0 && y < 9 &&
            board[[y]][x] != side && board[[y]][x] != side * 3) {
            x <- x + directions[1, i]
            y <- y + directions[2, i]
            while (x > 0 && x < 9 && y > 0 && y < 9 && board[[y]][x] == 0) {
                f <- T
                board[[y]][x] <- -5
                x <- x + directions[1, i]
                y <- y + directions[2, i]
            }
        }
    }
    if (!f){
        for (i in 1:4) {
            x <- pos[2] + directions[1, i]
            y <- pos[1] + directions[2, i]
            while (x > 0 && x < 9 && y > 0 && y < 9 && board[[y]][x] == 0) {
                board[[y]][x] <- 5
                x <- x + directions[1, i]
                y <- y + directions[2, i]
            }
        }
    }
    if (changeBoard) {
        board <<- board
        checked <<- pos
        queenIsChecked <<- T
    }
    return(f)
}
queenUncheck <- function(pos, bugFlag = T) {
    board[[pos[1]]][pos[2]] <- 3 * side
    directions <- data.frame(c(1, 1), c(1, -1), c(-1, 1), c(-1, -1))
    
    for (i in 1:4) {
        x <- pos[2] + directions[1, i]
        y <- pos[1] + directions[2, i]
        while (x > 0 && x < 9 && y > 0 && y < 9) {
            if (abs(board[[y]][x]) == 5)
                board[[y]][x] <- 0
            x <- x + directions[1, i]
            y <- y + directions[2, i]
        }
    }
    if (bugFlag) {
        checked <<- NA
        queenIsChecked <<- F
        board <<- board
    }
    else
        return (board)
}
queenMove <- function(start, finish) {
    if (board[[finish[1]]][finish[2]] == -5) {
        queenUncheck(start)
        moveAnimation(start, finish, T)
        direction <- sign(finish - start)
        x <- start[2] + direction[2]
        y <- start[1] + direction[1]
        
        while (board[[y]][x] == 0) {
            x <- x + direction[2]
            y <- y + direction[1]
        }
        board[[y]][x] <- 0
        board[[finish[1]]][finish[2]] <- board[[start[1]]][start[2]]
        board[[start[1]]][start[2]] <- 0
        if (board[[finish[1]]][finish[2]] == 3)
            white <<- white - 1
        else
            black <<- black - 1
        board <<- board
        if (queenCheck(finish)) {
            side <<- side * -1
            oneMore <<- T
            queenIsChecked <<- T
            checked <<- finish
        }
        else{
            oneMore <<- F
            checked <<- NA
        }
    }
    else {
        queenUncheck(start)
        moveAnimation(start, finish, F)
        board[[finish[1]]][finish[2]] <- board[[start[1]]][start[2]]
        board[[start[1]]][start[2]] <- 0
    }
    side <<- side * -1
    board <<- board
    
}
newGame <- function() {
    board <<- list(
        c(10, 1, 10, 1, 10, 1, 10, 1),
        c(1, 10, 1, 10, 1, 10, 1, 10),
        c(10, 1, 10, 1, 10, 1, 10, 1),
        c(0, 10, 0, 10, 0, 10, 0, 10),
        c(10, 0, 10, 0, 10, 0, 10, 0),
        c(-1, 10, -1, 10, -1, 10, -1, 10),
        c(10, -1, 10, -1, 10, -1, 10, -1),
        c(-1, 10, -1, 10, -1, 10, -1, 10)
    )
    white <<- 12
    black <<- 12
    checkable <<- list()
    checked <- NA
    side <<- -1
    oneMore <<- F
    queenIsChecked <<- F
}
moveAnimation <- function(start, finish, enemy) {
    if (!animationFlag)
        return (0)
    y <- start[1]
    x <- start[2]
    type <- board[[start[1]]][start[2]]
    if (enemy) {
        if (abs(type) == 1) {
            enemyX <- (start + (finish - start) / 2)[2]
            enemyY <- (start + (finish - start) / 2)[1]
        }
        else {
            direction <- sign(finish - start)
            enemyY <- start[1] + direction[1]
            enemyX <- start[2] + direction[2]
            
            while (board[[enemyY]][enemyX] == 0) {
                enemyX <- enemyX + direction[2]
                enemyY <- enemyY + direction[1]
            }
        }
        
    }
    board[[y]][x] <<- 0
    Is <- (y - 1) * 10
    Js <- (x - 1) * 10
    x_dir <- sign(finish - start)[2]
    y_dir <- -1 * sign(finish - start)[1]
    for (i in 0:10 * abs(start - finish)) {
        drawBoard()
        if (enemy && i > 5 * abs(start - c(enemyY, enemyX)) * sqrt(2))
            board[[enemyY]][enemyX] <<- 0
        
        if(type == 1)
        {
            rect(-40 + Js, 40 - Is,
                 -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501")
            draw.circle(-35 + Js + i * x_dir, 35 - Is + i * y_dir, 4, col = "#090909", lwd = 2, border = "#252525")
            draw.circle(-35 + Js + i * x_dir, 35 - Is + i * y_dir, 3, col = "#090909", lwd = 2, border = "#252525")
            draw.circle(-35 + Js + i * x_dir, 35 - Is + i * y_dir, 2, col = "#101010", lwd = 2, border = "#252525")
        }
        else if(type == -1)
        {
            rect(-40 + Js, 40 - Is,
                 -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501")
            draw.circle(-35 + Js + i * x_dir, 35 - Is + i * y_dir, 4, col = "#A4A4A4", lwd = 2, border = "#D5D5D5")
            draw.circle(-35 + Js + i * x_dir, 35 - Is + i * y_dir, 3, col = "#A4A4A4", lwd = 2, border = "#D5D5D5")
            draw.circle(-35 + Js + i * x_dir, 35 - Is + i * y_dir, 2, col = "#B9B9B9", lwd = 2, border = "#D5D5D5")
        }
        else if(type == 3)
        {
            rect(-40 + Js, 40 - Is,
                 -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501")
            draw.circle(-35 + Js + i * x_dir, 35 - Is + i * y_dir, 4, col = "#101010", lwd = 2, border = "#303030")
        }
        else if(type == -3)
        {
            rect(-40 + Js, 40 - Is,
                 -30 + Js, 30 - Is, col = "#5B3501", border = "#5B3501")
            draw.circle(-35 + Js + i * x_dir, 35 - Is + i * y_dir, 4, col = "#A9A9A9", lwd = 2, border = "#D9D9D9")
        }
        Sys.sleep(0.05)
    }
    if (enemy)
        board[[enemyY]][enemyX] <<- side * -1
    board[[y]][x] <<- type
}
action <- function(pos){
    x <- pos[2]
    y <- pos[1]
    if (board[[y]][x] == side && is.na(checked) && isCheckable(pos))
        check(pos, T)
    else if (board[[y]][x] == side * 3 && is.na(checked) && isCheckable(pos))
        queenCheck(pos, T)
    else if (abs(board[[y]][x]) == 5) {
        makeBackup()
        if (queenIsChecked)
            queenMove(checked, pos)
        else
            move(checked, pos)
    }
    else if (!is.na(checked) && !oneMore) {
        if (queenIsChecked)
            queenUncheck(checked)
        else
            uncheck(checked)
    }
    else if (board[[pos[1]]][pos[2]] != 0 && board[[pos[1]]][pos[2]] != 10){
        if (oneMore)
            mes <<- "You must eat!"
        else if (sign(board[[pos[1]]][pos[2]]) != side)
            mes <<- "Wrong side!"
        else if (length(checkable) != 0)
            mes <<- "There are checker, which able to eat!"
    }
}


AI_Core <- function(board, depth, side) {
    if (depth < 2) # influence of depth value on difficulty: 2 = easy, 4 = medium, 6 = hard, 8+ = impossible (to compute)
    {
        depth <- 2
    }
    else if (depth%%2 != 0)
    {
        depth <- depth - 1
    }
    move <- AI_Tree(board, depth, side)[[2]] # dont forget about checking if there is a possibility to make one more move
    return(move)
}
AI_Tree <- function(board, depth, side) {
    if (depth%%2 == 0)
    {
        bestvalue <- -1000
        Board_Array <- AI_Generate(board, side)
    }
    else
    {
        bestvalue <- 1000
        Board_Array <- AI_Generate(board, side*(-1))
    }
    n <- length(Board_Array)
    
    Layer <- list()
    for (i in 1:n)
    {
        Layer[[i]] <- list(value = NULL, board = Board_Array[[i]])
        if (depth > 1)
        {
            t <- AI_Evaluate(Layer[[i]]$board, side)
            if ((t == 1000)||(t == -1000))
                Layer[[i]]$value <- t
            else
                Layer[[i]]$value <- AI_Tree(Layer[[i]]$board, depth - 1, side)[[1]]
        }
        else
        {
            Layer[[i]]$value <- AI_Evaluate(Layer[[i]]$board, side)
        }
        if (depth%%2 == 0)
        {
            if (Layer[[i]]$value >= bestvalue)
            {
                bestvalue <- Layer[[i]]$value
                bestboard <- Layer[[i]]$board
            }
        }
        else
        {
            if (Layer[[i]]$value <= bestvalue)
            {
                bestvalue <- Layer[[i]]$value
                bestboard <- Layer[[i]]$board
            }
        }
    }
    
    return(list(bestvalue, bestboard))
} # depth value should be divisible by 2
generateEatMoves <- function(pos, board, s) {
    possMoves <- list()
    if (board[[pos[1]]][pos[2]] == s) {
        x <- (pos + c(1, 1) * s)[2]
        y <- (pos + c(1, 1) * s)[1]
        
        if (x > 1 && x < 8 && y > 1 && y < 8 &&
            (board[[y]][x] == -1 * s || board[[y]][x] == -3 * s)
            && board[[y + s]][x + s] == 0) {
            
            tempMoves <- generateEatMoves(c(y + s, x + s), replace(pos, c(y + s, x + s), board, T), s)
            if (length(tempMoves) == 0)
                possMoves[[length(possMoves) + 1]] <- replace(pos, c(y + s, x + s), board, T)
            else {
                for (i in tempMoves)
                    possMoves[[length(possMoves) + 1]] <- i
            }
        }
        
        
        x <- (pos + c(1, -1) * s)[2]
        y <- (pos + c(1, -1) * s)[1]
        
        if (x > 1 && x < 8 && y > 1 && y < 8 &&
            (board[[y]][x] == -1 * s || board[[y]][x] == -3 * s)
            && board[[y + s]][x - s] == 0) {
            
            tempMoves <- generateEatMoves(c(y + s, x - s), replace(pos, c(y + s, x - s), board, T), s)
            if (length(tempMoves) == 0)
                possMoves[[length(possMoves) + 1]] <- replace(pos, c(y + s, x - s), board, T)
            else {
                for (i in tempMoves)
                    possMoves[[length(possMoves) + 1]] <- i
            }
        }
        
        x <- (pos + c(-1, -1) * s)[2]
        y <- (pos + c(-1, -1) * s)[1]
        
        if (x > 1 && x < 8 && y > 1 && y < 8 &&
            (board[[y]][x] == -1 * s || board[[y]][x] == -3 * s)
            && board[[y - s]][x - s] == 0) {
            
            tempMoves <- generateEatMoves(c(y - s, x - s), replace(pos, c(y - s, x - s), board, T), s)
            if (length(tempMoves) == 0)
                possMoves[[length(possMoves) + 1]] <- replace(pos, c(y - s, x - s), board, T)
            else {
                for (i in tempMoves)
                    possMoves[[length(possMoves) + 1]] <- i
            }
        }
        
        x <- (pos + c(-1, 1) * s)[2]
        y <- (pos + c(-1, 1) * s)[1]
        
        if (x > 1 && x < 8 && y > 1 && y < 8 &&
            (board[[y]][x] == -1 * s || board[[y]][x] == -3 * s)
            && board[[y - s]][x + s] == 0) {
            
            tempMoves <- generateEatMoves(c(y - s, x + s), replace(pos, c(y - s, x + s), board, T), s)
            if (length(tempMoves) == 0)
                possMoves[[length(possMoves) + 1]] <- replace(pos, c(y - s, x + s), board, T)
            else {
                for (i in tempMoves)
                    possMoves[[length(possMoves) + 1]] <- i
            }
        }
    }
    else if (board[[pos[1]]][pos[2]] == s * 3) {
        directions <- data.frame(c(1, 1), c(1, -1), c(-1, 1), c(-1, -1))
        for (i in 1:4) {
            x <- pos[2] + directions[1, i]
            y <- pos[1] + directions[2, i]
            while (x > 0 && x < 9 && y > 0 && y < 9 && board[[y]][x] == 0) {
                x <- x + directions[1, i]
                y <- y + directions[2, i]
            }
            
            if (x > 0 && x < 9 && y > 0 && y < 9 &&
                board[[y]][x] != s && board[[y]][x] != s * 3) {
                x <- x + directions[1, i]
                y <- y + directions[2, i]
                while (x > 0 && x < 9 && y > 0 && y < 9 && board[[y]][x] == 0) {
                    tempMoves <- generateEatMoves(c(y, x), replace(pos, c(y, x), board, T), s)
                    if (length(tempMoves) == 0)
                        possMoves[[length(possMoves) + 1]] <- replace(pos, c(y, x), board, T)
                    else {
                        for (b in tempMoves)
                            possMoves[[length(possMoves) + 1]] <- b
                    }
                    x <- x + directions[1, i]
                    y <- y + directions[2, i]
                }
            }
            
        }
    }
    
    return(possMoves)
}
generateMoves <- function(pos, board, s) {
    possMoves <- list()
    if (board[[pos[1]]][pos[2]] == s) {
        x <- (pos + c(1, 1) * s)[2]
        y <- (pos + c(1, 1) * s)[1]
        
        if (x > 0 && x < 9 && y > 0 && y < 9 && board[[y]][x] == 0)
            possMoves[[length(possMoves) + 1]] <- replace(pos, c(y, x), board, F)
        
        
        x <- (pos + c(1, -1) * s)[2]
        y <- (pos + c(1, -1) * s)[1]
        
        if (x > 0 && x < 9 && y > 0 && y < 9 && board[[y]][x] == 0)
            possMoves[[length(possMoves) + 1]] <- replace(pos, c(y, x), board, F)
    }
    else if (board[[pos[1]]][pos[2]] == 3 * s) {
        directions <- data.frame(c(1, 1), c(1, -1), c(-1, 1), c(-1, -1))
        for (i in 1:4) {
            x <- pos[2] + directions[1, i]
            y <- pos[1] + directions[2, i]
            while (x > 0 && x < 9 && y > 0 && y < 9 && board[[y]][x] == 0) {
                possMoves[[length(possMoves) + 1]] <- replace(pos, c(y, x), board, F)
                x <- x + directions[1, i]
                y <- y + directions[2, i]
            }
        }
    }
    
    return(possMoves)
}
AI_Generate <- function(board, side, checkStuck = F) {
    moves <- list()
    for (y in 1:8) {
        for (x in 1:8) {
            if (board[[y]][x] == side || board[[y]][x] == 3 * side) {
                tempMoves <- generateEatMoves(c(y, x), board, side)
                if (checkStuck && length(tempMoves) > 0)
                    return (F)
                for (m in tempMoves)
                    moves[[length(moves) + 1]] <- m
            }
        }
    }
    if (length(moves) == 0) {
        for (y in 1:8) {
            for (x in 1:8) {
                if (board[[y]][x] == side || board[[y]][x] == 3 * side) {
                    tempMoves <- generateMoves(c(y, x), board, side)
                    if (checkStuck && length(tempMoves) > 0)
                        return (F)
                    for (m in tempMoves)
                        moves[[length(moves) + 1]] <- m
                }
            }
        }
    }
    if (checkStuck)
        return (T)
    return (moves)
}
replace <- function(start, finish, board, eat) {
    if (eat) {
        des <- sign(finish - start)
        x <- start[2] + des[2]
        y <- start[1] + des[1]
        while (board[[y]][x] == 0) {
            x <- x + des[2]
            y <- y + des[1]
        }
        board[[y]][x] <- 0
        board[[finish[1]]][finish[2]] <- board[[start[1]]][start[2]]
        board[[start[1]]][start[2]] <- 0
    }
    else {
        board[[finish[1]]][finish[2]] <- board[[start[1]]][start[2]]
        board[[start[1]]][start[2]] <- 0
    }
    if ((finish[1] == 1 && sign(board[[finish[1]]][finish[2]]) == -1)
        || (finish[1] == 8 && sign(board[[finish[1]]][finish[2]]) == 1))
        board[[finish[1]]][finish[2]] <- sign(board[[finish[1]]][finish[2]]) * 3
    return (board)
}
AI_Evaluate <- function(board, side) {
    blacks <- 0
    whites <- 0
    blackQs <- 0
    whiteQs <- 0
    blackDist <- 0
    whiteDist <- 0
    
    for (i in 1:8)
    {
        for (j in 1:8)
        {
            if (board[[i]][j] == 1)
            {
                blacks <- blacks + 1
                if ((i > 3)&&(i < 8))
                {
                    blackDist <- blackDist + i*0.1
                }
            }
            if (board[[i]][j] == -1)
            {
                whites <- whites + 1
                if ((i < 6)&&(i > 1))
                {
                    whiteDist <- whiteDist + (9-i)*0.1
                }
            }
            if (board[[i]][j] == 3)
            {
                blackQs <- blackQs + 1
            }
            if (board[[i]][j] == -3)
            {
                whiteQs <- whiteQs + 1
            }
        }
    }
    if (((blacks == 0)&&(blackQs == 0)) || AI_Generate(board, 1, T))
    {
        return(-1000*side)
    }
    else if (((whites == 0)&&(whiteQs == 0)) || AI_Generate(board, -1, T))
    {
        return(1000*side)
    }
    value <- (blacks - whites + blackDist - whiteDist + blackQs*10 - whiteQs*10)*side
    return(value)
}

AI_flag <- T
AI_side <- 1
level <- 2 * 1
checkable <- list()
checked <- NA
side <- -1
oneMore <- F
queenIsChecked <- F
mes <- ''
animationFlag <- F
while(white != 0 && black != 0) {
    if (side == -1 * AI_side || !AI_flag) {
        if (is.na(checked) && AI_Generate(board, side, T)) {
            if (AI_side == -1 * 1)
                mes <- "White won"
            else
                mes <- "Black won"
            break
        }
        if (oneMore) {
            if (queenIsChecked)
                queenCheck(checked, T)
            else
                check(checked, T)
        }
        checkBoard()
        
        drawBoard()
        mes <- ''
        coor <- getCoordinates()
        if (is.null(coor))
            next
        else if (coor == 11) {
            backup()
            next
        }
        else if (coor == 9)
            break
        else if (coor == 0){
            newGame()
            next
        }
        action(coor)
    }
    else {
        mes <- "AI is thinking..."
        drawBoard()
        mes <- ''
        if (AI_Generate(board, AI_side, T)) {
            if (AI_side == 1)
                mes <- "White won"
            else
                mes <- "Black won"
            break
        }
        board <- AI_Core(board, level, AI_side)
        if (AI_side == 1)
            white <- checkersNum(-1 * AI_side)
        else
            black <- checkersNum(-1 * AI_side)
        side <- side * -1
    }
}

if (black == 0)
    mes <- "White won"
if (white == 0)
    mes <- "Black won"

drawBoard()
print('end')