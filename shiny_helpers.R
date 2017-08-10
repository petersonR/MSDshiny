# Author: Ryan Peterson
# Date Created: 6/28/2017
# Date Modified: 7/13/2017
# Description: Shiny helper functions for Multistate Simulation Designer

library(shiny)

plotCurves <- function(params, max_time = 15, curve_type = c('CHaz')) {
  
  if(is.null(max_time)) max_time <- 15
  tt <- seq(0, max_time, length = 100)
  
  cHaz <- try(apply(params[c('lambdas', 'shapes')], 1, function(x) {
    -pweibull(tt, 
              shape = as.numeric(x[2]), 
              scale = (x[1]) ^ (-1/x[2]), F, T)
    }), T)
  
  if(is.character(cHaz)) return(0)
  
  yvals <- cHaz
  ylab <- 'Cumulative Hazard'
  if(curve_type == 'Haz') {
    yvals <- apply(yvals, 2, function(x) c(NA, diff(x)))
    ylab <- 'Hazard'
  } else if(curve_type == 'Surv') {
    yvals <- apply(yvals, 2, function(x) exp(-x))
    ylab <- 'Prop. Event-Free'
  }
  
  plotTrt <- length(params$trtHR) && any(params$trtHR != 1)
  
  if(plotTrt) {
    trtVals <- t(t(cHaz) * params$trtHR)
    if(curve_type == 'Haz') {
      trtVals <- apply(trtVals, 2, function(x) c(NA, diff(x)))
    } else if(curve_type == 'Surv') {
      trtVals <- apply(trtVals, 2, function(x) exp(-x))
    }
  } else trtVals <- NULL
  
  
  nPlots <- length(unique(params$from))
  heightdiff <- 2
  if(nPlots == 2) layout(matrix(c(1:3, 1:3), nrow = 3, ncol = 2), heights = c(heightdiff,heightdiff,1))
  if(nPlots == 3) layout(matrix(c(1:3, 4), nrow = 2, ncol = 2, byrow = T))
  if(nPlots == 4) layout(matrix(c(1:5, 5), nrow = 3, ncol = 2, byrow = T), heights = c(heightdiff,heightdiff, 1))
  if(nPlots == 1) layout(matrix(c(1:2), nrow = 2, ncol = 1), heights = c(heightdiff,1))
  
  cols <- params$cols
  
  par(mar = c(5,4,4,2) + .1)
  for(i in levels(params$from)[-length(levels(params$from))]) {
    
    fromIDX <- params$from == i
    
    yrange <- range(yvals[,fromIDX], 0, na.rm = T)
    if(plotTrt) yrange <- range(yrange, trtVals[,fromIDX], na.rm = T)
    
    plot(tt, as.matrix(yvals[,fromIDX])[,1], type = 'l', ylim = yrange ,
         xlab = 'Time', ylab = ylab, main = paste0('From ', i), col = cols[fromIDX][1])
    if(plotTrt) lines(tt, as.matrix(trtVals[,fromIDX])[,1], lty = 2, col = cols[fromIDX][1])
    
    for(j in unique(params$to[fromIDX])[-1]) {
      toIDX <- params$to == j
      lines(tt, yvals[,fromIDX & toIDX], col = cols[fromIDX & toIDX])
      if(plotTrt) lines(tt, trtVals[,fromIDX & toIDX], lty = 2, col = cols[fromIDX & toIDX])
    }
    
  }
  
  ## Use last plot as legend
  if(nPlots == 4 | nPlots < 3) {
    plot.new()
    par(mar = c(0,0,0,0))
    legend('top', legend = unique(params$to), col = unique(cols), lwd = 2, bty = 'n', ncol = length(unique(params$to)))
  } else {
    plot.new()
    par(mar = c(0,0,0,0))
    legend('center', legend = unique(params$to), col = unique(cols), lwd = 2, bty = 'n',y.intersp = 1.5)
  }
}

# # Unit test
# tmat <- rbind(
#   #TO:Entry CR   TXP  # FROM:
#   c(F,    T,   T, F, F),    # ENTRY
#   c(F,    F,   T, T, T),    # CR
#   c(F,    T,   F, T, T),  # TXP
#   c(F,    T,   T, F, T),  # RL
#   c(F, F, F ,F, F) # Death
# )
# rownames(tmat) <- colnames(tmat) <- c('Entry', 'CR', 'TXP', 'Relapse','Death')
# 
# params <- data.frame(expand.grid(to = rownames(tmat), from = rownames(tmat))[t(tmat),2:1])
# params$lambdas <- 1:nrow(params) / 10
# params$shapes <- 1
# params$trtHR <- 1
# params$cols <- as.numeric(params$to)

# plotCurves(params, curve_type = 'Surv')
# plotCurves(params, curve_type = 'CHaz')
# plotCurves(params, curve_type = 'Haz')
# 
# params$trtHR <- NULL
# plotCurves(params, curve_type = 'Surv')
# plotCurves(params, curve_type = 'CHaz')
# plotCurves(params, curve_type = 'Haz')
# 
# params$trtHR <- (1:nrow(params) / 1)
# plotCurves(params, curve_type = 'Surv')
# plotCurves(params, curve_type = 'CHaz')
# plotCurves(params, curve_type = 'Haz')
# 

mysim2 <- function(params, N, trtpct = 50, censorRate = 20, maxfollowup = Inf) {
  
  trtpct <- trtpct / 100
  censorOdds <- (censorRate / 100) / (1 - (censorRate / 100))
  
  cIDX <- params$to == params$to[nrow(params)]
  cparams <- params[cIDX,]
  cparams$to <- 'censor'
  cparams$lambdas <- cparams$lambdas * censorOdds
  params <- rbind(params, cparams)

  params
  nparams <- params[rep(1:(nrow(params)), N*2),]
  nparams$id <- rep(1:(N*2), each = nrow(params))
  trtID <- 1:(2 * N * (trtpct))
  nparams$x1 <- 0
  nparams$x1[nparams$id %in% trtID] <- 1
  nparams$lambdas <- pmax(nparams$lambdas, .Machine$double.eps)
  # Simulate event histories 
  e1params <- subset(nparams, from == params$from[1])
  
  etimes <- rweibull(nrow(e1params), shape = e1params$shapes, 
                     scale = (e1params$lambdas * exp(e1params$x1 * log(e1params$trtHR))) ^ (-1/e1params$shapes))
  e1info <- cbind(etimes, e1params)
  head(e1info)
  
  p <- e1info[order(e1info$id, e1info$from, e1info$etimes),]
  p$delta <- 1 * !duplicated(p[c('from', 'id')])
  p$entry <- 0
  p$exit <- p$entry + unlist(by(p$etimes, p$id, function(x) rep(min(x), length(x)), simplify = F))
  
  lostIDX <- p$delta == 1 & p$exit > maxfollowup
  sum(lostIDX)
  lostpIDX <- p$id %in% p$id[lostIDX]
  p$exit[lostpIDX] <- maxfollowup
  p$delta[lostpIDX] <- 1 * (p$to[lostpIDX] == 'censor')
  
  temp <- res <- p
  nleft <- sum(temp$delta == 1 & !(temp$to %in% c('censor', as.character(params$to[nrow(params)-1]))))
  
  n <- 2
  while(nleft > 0 & n < 50) {
    tempdf <- data.frame(id = temp$id, from = temp$to, entry = temp$exit)[temp$delta == 1 & temp$to != 'censor',]
    tempparams <- merge(tempdf, nparams, by = c('id', 'from'))
    tempparams <- tempparams[order(tempparams$id),]
    # cat(nrow(tempparams), 'People with event', n, '\n')
    
    etimes <- rweibull(nrow(tempparams), shape = tempparams$shapes, 
                       scale = (tempparams$lambdas * exp(tempparams$x1 * log(tempparams$trtHR))) ^ (-1/tempparams$shapes))
    temp <- cbind(etimes, tempparams)
    temp <- temp[order(temp$id, temp$from, temp$etimes),]
    
    head(temp)
    
    temp$exit <- temp$entry + unlist(by(temp$etimes, temp$id, function(x) rep(min(x), length(x)), simplify = F))
    head(temp)
    
    temp <- temp[order(temp$id, temp$from, temp$etimes),]
    temp$delta <- 1 * (!duplicated(temp[c('from', 'id')]))

    lostIDX <- temp$delta == 1 & temp$exit > maxfollowup
    lostpIDX <- temp$id %in% temp$id[lostIDX]
    temp$exit[lostpIDX] <- maxfollowup
    temp$delta[lostpIDX] <- 1 * (temp$to[lostpIDX] == 'censor')
    
    res <- rbind(res, temp)
    n <- n + 1
    nleft <- sum(temp$delta == 1 & !(temp$to %in% c('censor', as.character(params$to[nrow(params)-1]))))
  }
  
  ## Alternative - do uniform censoring
  # censored <- as.logical(rbinom(N*2, 1, censorRate/100))
  # cID <- (1:(N*2))[censored]
  # rescID <- (res$id  %in% cID)
  # cobs <- res[rescID,]
  # maxcTimes <- as.numeric(by(cobs$exit, cobs$id, max))
  # censTimes <- runif(sum(censored), 0, maxcTimes)
  # cobs1 <- data.frame(id = cID, censTimes = censTimes)
  # try1 <- merge(cobs, cobs1)
  # head(try1, 10)
  # 
  # try1 <- try1[try1$entry < try1$censTimes,]
  # try1$exit[try1$exit > try1$censTimes] <- try1$censTimes[try1$exit > try1$censTimes]
  # try1$delta[try1$exit == try1$censTimes & try1$delta == 1] <- 0
  # try2 <- try1[order(try1$id, try1$entry, decreasing = T),]
  # head(try2, 20)
  # try3 <- try2[!duplicated(try2$id),]
  # try3$to <- 'censor'
  # try3$delta <- 1
  # 
  # try4 <- rbind(try2, try3)
  # try4 <- try4[order(try4$id, try4$from, try4$to),]
  # head(try4, 20)
  
  res <- res[!(res$to == 'censor' & res$delta == 0),]
  
  res <- res[order(res$id, res$from, res$to),]
  cat('.')
  res
}

# params <- data.frame(expand.grid(to = rownames(tmat), from = rownames(tmat))[t(tmat),2:1])
# params$lambdas <- unlist(lapply(seq(nrow(params)), function(i) .1))
# params$shapes <- 1
# params$trtHR <- 1
# params$cols <- as.numeric(params$to)
# head(mysim2(params, 100, maxfollowup = 20))
# 
# plotCurves(params)

# Set up a button to have an animated loading indicator and a checkmark
# for better user experience
# Need to use with the corresponding `withBusyIndicator` server function
withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  div(
    `data-for-btn` = id,
    button,
    span(
      class = "btn-loading-container",
      hidden(
        img(src = "ajax-loader-bar.gif", class = "btn-loading-indicator"),
        icon("check", class = "btn-done-indicator")
      )
    ),
    hidden(
      div(class = "btn-err",
          div(icon("exclamation-circle"),
              tags$b("Error: "),
              span(class = "btn-err-msg")
          )
      )
    )
  )
}

prettyP <- function(pvec, digits = 3) {
  ifelse(round(pvec, digits) == 0, 
         paste0('<0.' ,paste0(rep('0', digits-1), collapse = ''), 1), 
         round(pvec, digits))
}

myTryCatch <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- append(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  list(value=value, warning=warn, error=err)
}

weibullPar <- div(
    p(
      'Weibull Parameterization: $$h(t) = \\lambda \\gamma t ^ {\\gamma - 1}$$'
    ),
    p(
      '$$E(T) = \\lambda ^ {-1/\\gamma} \\Gamma \\left(\\frac{\\gamma + 1}{\\gamma} \\right)$$'
    ),
    a('More information', href = 'http://www.itl.nist.gov/div898/handbook/apr/section1/apr162.htm', target =
        "_blank")
)


