###--- Play with google sheet: https://goo.gl/Lpt7gf
##- rm(list=ls())

##---- libs ----
library(googlesheets)
# library(dplyr)

##---- read sheet ----
# all_sheets <- gs_ls()
#gs <- gs_title("Euro 2016 DIDE")
gs <- gs_title("Test ... Euro 2016 DIDE")

##---- scoring system ----
##- points
# scoring <- gs_read(gs, ws = "Scoring",  range = "B1:B4", col_names = FALSE)
scoring <- gs_read(gs, ws = "Copy of Scoring",  range = "B20:B25", col_names = FALSE)

P_SCORE <- as.numeric(scoring[1,]) # 3
P_RESULT <- as.numeric(scoring[2,]) # 1
R_BONUS <-  as.numeric(scoring[3,]) # 2
VR_BONUS <-  as.numeric(scoring[4,]) # 0.25
S_BONUS <-  as.numeric(scoring[5,]) # 2
VS_BONUS <-  as.numeric(scoring[6,]) # 0.1

##---- extract predictions ----
# tab <- gs_read(gs, ws = "Group stage",  range = cell_rows(1:37))
tab <- gs_read(gs, ws = "Knockout",  range = cell_rows(1:16))

##- named list of matrices
.a <- lapply(tab[,-(1:4)], function(x) strsplit(x, "-"))
preds <- lapply(.a, function(x){
  matrix(unlist( lapply(x, '[', 1:2))
         , ncol = 2, byrow = TRUE)
})



if(FALSE){
##---- simulate real score ----
##- Poisson
.n <- sum(!is.na( preds[[1]] )) # number of non NA scores
real <- matrix(as.character( rpois(.n, 1.3) ), ncol = 2)
##- add winner for draws
for (i in 1:nrow(real)){
  if (real[i, 1] == real[i, 2]) {
    j <- sample(1:2,1)
    real[i, j] <- paste(real[i, j], 'w', sep = '')
  }
}
##- sample of DIDE
# real2 <- matrix(NA, nrow = 36, ncol = 2)
#  for (i in 1:36){
#   real2[i,] <- preds[[ sample(1:length(preds), 1) ]][i, ]
#  }
}

##---- get real score ----
# real <- matrix(as.numeric(unlist(lapply(tab[, 4], function(x) strsplit(x, "-")), use.names = FALSE)), ncol = 2, byrow = TRUE)

##- deal with NA
# .r <- vapply(tab[, 4], function(x) strsplit(x, "-"), matrix(list(), 15, 1) )
##- force indexing in 2 values
# real <- matrix(as.numeric(unlist(lapply(.r, '[', 1:2))), ncol = 2, byrow = TRUE)

##---- scoring function ----
##- by game
# debug: pred = preds[[1]]; i = 2
ss <- function(pred, real){
  score <- rep(0, dim(pred)[1])
  for (i in 1:dim(real)[1] ){
    score[i] <- ifelse( any(is.na(real[i,])) , 0,
                 ifelse( identical(real[i,], pred[i,]) , P_SCORE, # score match
                   ifelse( sign(diff( as.numeric(sub('w', '.5', real[i,])) ) ) ==
                                     sign(diff( as.numeric(sub('w', '.5', pred[i,])) ) ) , P_RESULT, # result match
                           0) ))
  }
  return(score)
}

##---- results without bonus ----
rs0 <- sapply(preds, function(x) ss(x, real))

##---- bonuses ----
##- games with good results rarely predicted
rowsBonus_R <- rowSums(rs0!=0) / ncol(rs0) < 0.5# VR_BONUS
##- good results
mr <- rs0 > 0
bonusMatrix_R <- mr * rowsBonus_R * R_BONUS
# sum(bonusMatrix_R)

##- games with good scores rarely predicted
rowsBonus_S <- rowSums(rs0==3) / ncol(rs0) < 0.3# VR_BONUS
##- good scores
ms <- rs0 > 2
bonusMatrix_S <- ms * rowsBonus_S * S_BONUS
# sum(bonusMatrix_S)

##---- final results ---- 
rs1 <- rs0 + bonusMatrix_R + bonusMatrix_S
row.names(rs1) <- apply(tab[,2:3], 1, function(x) paste(x, collapse = " - "))

##---- grand total ----
final <- colSums(rs1)
final[order(final, decreasing = TRUE)]

##- display1
##- matrix of non null 
.m <- rs1[, colSums(rs1) != 0]
##- list of pipes + name
.b <- lapply(colnames(.m), function(x){
  c( rep("|", max(sum(.m[,x]) - 1, 0)), x )
})
##- force indexing to max length
display1 <- sapply(.b, '[', 1:max(final))
##- mask NA
display1[is.na(display1)] <- ""

##- best results centered
z <- colSums(.m)
w <- order(z)
n <- length(z)
x <- order(abs(1:n - median(1:n)) + 1, decreasing = T)
y <- w[order(x)]
display1 <- display1[,y]

##---- display2 ----
.c <- lapply(colnames(.m), function(x){
  c( rep("-", max(sum(.m[,x]) - 1, 0)), x )
})
display2 <- sapply(.c, '[', 1:max(final))
display2 <- t(sapply(.c, '[', 1:max(final)) )
# display2 <- t(vapply(.c, FUN =  '[', FUN.VALUE = vector("character", 10), 1:max(final)) )
display2[is.na(display2)] <- ""
display2 <- display2[y,]

##---- add results in gs ----
# gs_edit_cells(gs, ws = "Results", input = display2, anchor = "A1", col_names = FALSE )

##---- add row results
# rowrs1 <- data.frame(match = apply(tab[,2:3], 1, function(x) paste(x, collapse = " - ")), rs1)
# gs <- gs_ws_new(gs, ws = "RowResults", row_extent = nrow(rowrs1), col_extent = ncol(rowrs1) )
# gs_edit_cells(gs, ws = "RowResults", input = rowrs1, anchor = "A1", col_names = TRUE )
