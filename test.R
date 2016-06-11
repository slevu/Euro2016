###--- Play with google sheet: https://goo.gl/Lpt7gf
##- rm(list=ls())
##---- libs ----
library(googlesheets)
# library(dplyr)

##---- read sheet ----
# all_sheets <- gs_ls()
gs <- gs_title("Euro 2016 DIDE")

##---- extract predictions ----
tab <- gs_read(gs, ws = "Group stage",  range = cell_rows(1:37))
##- named list of matrices
preds <- lapply(tab[,-(1:3)], function(x) matrix(as.numeric(unlist(strsplit(x,"-"))), ncol = 2, byrow = TRUE))

if(FALSE){
##---- simulate real score ----
##- Poisson
real <- matrix(rpois(72, 1.4), ncol = 2)
##- sample of DIDE
real2 <- matrix(NA, nrow = 36, ncol = 2)
 for (i in 1:36){
  real2[i,] <- preds[[ sample(1:length(preds), 1) ]][i, ]
 }
}

##---- get real score ----
.r <- gs_read(gs, ws = "Results group stage",  range = "D1:D37")
real <- matrix(as.numeric(unlist(lapply(.r, function(x) strsplit(x, "-")), use.names = FALSE)), ncol = 2, byrow = TRUE)

##---- scoring system ----
##- points
scoring <- gs_read(gs, ws = "Scoring",  range = "B1:B4", col_names = FALSE)

 P_SCORE <- as.numeric(scoring[1,]) # 3
 P_RESULT <- as.numeric(scoring[2,]) # 1
 P_BONUS <-  as.numeric(scoring[3,]) # 2
 V_BONUS <-  as.numeric(scoring[4,]) # 0.25
 
##---- scoring function ----
##- by game
ss <- function(pred, real){
  score <- rep(0, dim(pred)[1])
  for (i in 1:dim(real)[1] ){
    score[i] <- ifelse( any(is.na(real[i,])) , 0,
                 ifelse( identical(real[i,], pred[i,]) , P_SCORE, # score match
                   ifelse( identical(sign(diff(real[i,])) , sign(diff(pred[i,]))), P_RESULT, # result match
                           0) ))
  }
  return(score)
}

##---- results without bonus ----
rs0 <- sapply(preds, function(x) ss(x, real))

##---- bonus ----
##- games with good results rarely predicted
rowsWithBonus <- rowSums(rs0!=0) / ncol(rs0) < V_BONUS
##- good results
m <- rs0 > 0
bonusMatrix <- m * rowsWithBonus * P_BONUS

##---- final results ---- 
rs1 <- rs0 + bonusMatrix
row.names(rs1) <- apply(tab[,2:3], 1, function(x) paste(x, collapse = " - "))

##---- grand total ----
final <- colSums(rs1)
final[order(final, decreasing = TRUE)]

##---- add results in gs ----
gs_edit_cells(gs, ws = "Results group stage",  input = rs1, anchor = "E1", col_names = TRUE )
