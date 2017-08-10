# install.packages('diagram')
library(diagram)

plot_statefig <- function(tmat, cols) {
  tmat <- try(t(tmat))
  if(is.character(tmat)) return(0)
  
  pos <- c(1, nrow(tmat) - 2 ,1)
  pos <- pos[pos > 0]
  
  curves <- matrix(0, nrow(tmat), ncol(tmat), dimnames = dimnames(tmat))
  curves[nrow(tmat), 1] <- (ncol(tmat) - 2) / (ncol(tmat) - .9)
  curves[-c(1,nrow(tmat)), -c(1,nrow(tmat))] <- (1) / ((nrow(tmat) - 2) ^ 2 + .5)
  if(nrow(tmat) == 3) curves[2,2] <- 0
  
  curves
  arr.col <- tmat
  for(i in 1:nrow(tmat)) {
    arr.col[i,] <- ifelse(arr.col[i,], cols[i], '0')
  }
  arr.col
  
  par(mfrow = c(1,1), mar = c(0, 0, 0, 0) + .1)
  
  plotmat(
    tmat, # transposed tmat matrix
    pos = pos, # vector of boxes per row
    curve = curves, # straight arrows
    box.type = 'rect',
    box.prop = 3/5,
    box.size = .13,
    shadow.col = 'slategrey',
    arr.col = arr.col,
    arr.lcol = arr.col
  )
}

# tmat <- rbind(
#   #TO:Entry    Death  # FROM:
#   c(F,  T),    # ENTRY
#   c(F,   F)     # Death
# )
# rownames(tmat) <- colnames(tmat) <- c('Entry', 'Death')
# 
# plot_statefig(tmat, c(0, 3,1))
# 
# 
# tmat <- rbind(
#   #TO:Entry CR   Death  # FROM:
#   c(F,    T,   T),    # ENTRY
#   c(F,    F,   T),    # CR
#   c(F,    F,   F)     # Death
# )
# rownames(tmat) <- colnames(tmat) <- c('Entry', 'CR', 'Death')
# 
# plot_statefig(tmat, c(0, 3,1))
# 
# tmat <- rbind(
#   #TO:Entry CR   TXP  # FROM:
#   c(F,    T,   T, T),    # ENTRY
#   c(F,    F,   T, T),    # CR
#   c(F,    T,   F, T),  # TXP
#   c(F, F, F ,F) # Death
# )
# rownames(tmat) <- colnames(tmat) <- c('Entry', 'CR', 'TXP',  'Death')
# 
# plot_statefig(tmat, c(0, 3, 4, 1))
# 
# tmat[2,3] <- F
# plot_statefig(tmat, c(0, 3, 4, 1))
# 
# tmat <- rbind(
#   #TO:Entry CR   TXP  # FROM:
#   c(F,    T,   T, F, T),    # ENTRY
#   c(F,    F,   T, T, T),    # CR
#   c(F,    T,   F, T, T),  # TXP
#   c(F,    T,   T, F, T),  # RL
#   c(F, F, F ,F, F) # Death
# )
# 
# rownames(tmat) <- colnames(tmat) <- c('Entry', 'CR', 'TXP', 'Relapse','Death')
# 
# plot_statefig(tmat, c(0, 3, 4, 2, 1))
# 
# 
# tmat <- rbind(
#   c(F, T, T, T),
#   c(F, F, T, T),
#   c(F, F, F, T),
#   c(F, F, F, F)
# )
# 
# cols <- c("#0000FF", "#00CD00", "#FF0000", "#000000")
# plot_statefig(tmat, cols)
