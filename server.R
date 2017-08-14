# Author: Ryan Peterson
# Date Created: 6/28/2017
# Date Modified: 8/9/2017
# Description: Shiny server for Multistate Simulation Designer

list.of.packages <-
  c('base', "survival", 'mstate', 'diagram', "shiny", 'shinyjs',
    'colourpicker', 'xtable',  'shinyBS')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library(shiny)
library(survival)
library(shinyjs)
library(mstate)
library(colourpicker)
library(xtable)
library(diagram)
library(shinyBS)

# Load helper functions 
source('shiny_helpers.R')
source('plot_statefig.R')

server <- function(input, output, session) {
  
  ###### Multistate Structure  ###### 
  
  # reactiveValues for page 1 reactives
  rv <- reactiveValues()
  
  # State names UI
  output$tmatUI1.1 <- renderUI({
    lapply(seq(input$nstates),
           function(i) {
             textInput(paste0('n', i),
                       paste('Stage', i, 'name'),
                       value = paste('Stage', i))
           })
  })
  
  # State colors UI
  output$tmatUI1.2 <- renderUI({
    default_cols <- c('black', 'red', 'green3', 'blue', 'cyan')
    lapply(seq(input$nstates),
           function(i) {
             if (i == 1) {
               disabled(colourInput(
                 paste0('col', i),
                 paste('Stage', i, 'Color'),
                 value = 'white',
                 showColour = 'background'
               ))
             } else {
               colourInput(
                 paste0('col', i),
                 paste('Stage', i, 'Color'),
                 value = default_cols[input$nstates - i + 1],
                 showColour = 'background'
               )
             }
             
           })
  })
  
  # Store state names in reactive value
  rv$stateNames <- reactive({
    names <- c()
    for(i in 1:input$nstates) names <- c(names, input[[paste0('n', i)]])
    # In case a user inputs the same name twice
    dupIDX <- duplicated(names)
    names[dupIDX] <- paste0(names[dupIDX], '_', 1:sum(dupIDX))
    names
  })
  
  # Store state colors in reactive value
  rv$stateCols <- reactive({
    cols <- c()
    for(i in 1:input$nstates) cols <- c(cols, input[[paste0('col', i)]])
    cols
  })
  
  # Transition matrix UI (sequence of checkbox inputs)
  output$tmatUI2 <- renderUI({
    # Where should the to options be for each state?
    choices <- lapply(seq(input$nstates - 1) + 1, I)
    
    # Set names of choices
    if (length(rv$stateNames()))
      names(choices) <- paste0('To: ', rv$stateNames()[-1])
    
    # Set default checks
    selections <- lapply(seq(input$nstates - 1), 
                         function(i) (1 + i):input$nstates)
    
    # UI 
    lapply(seq(input$nstates - 1), function(i) {
      choice = choices[-i + 1]
      if (i == 1)
        choice = choices
      checkboxGroupInput(
        paste0('n', i, 'to'),
        label = paste0('From: ', rv$stateNames()[i]),
        choices = choice,
        selected = selections[[i]]
      )
    })
  })
  
  # reactive value for transition matrix
  rv$tmat <- reactive({
    res <- matrix(FALSE, nrow = input$nstates, ncol = input$nstates)
    rownames(res) <- colnames(res) <- rv$stateNames()
    for(i in 1:(input$nstates-1)) {
      res[i,as.numeric(input[[paste0('n', i, 'to')]])] <- TRUE 
    }
    res
  })
  
  # State fig plot (see plot_statefig.R)
  output$statefig <- renderPlot({
    plot_statefig(rv$tmat(), rv$stateCols())
  }, bg="transparent")
  
  ###### Baseline Hazards ######
  
  # reactiveValues for page 2 reactives
  rv2 <- reactiveValues()
  
  # reactive for number of total transitions
  rv2$ntrans <- reactive({
    sum(rv$tmat())
  })
  
  # reactive for names of each transition
  rv2$transNames <- reactive({
    rr <- try(expand.grid(to = rownames(rv$tmat()), from = rownames(rv$tmat()))[t(rv$tmat()),2:1], TRUE)
    paste('From', apply(rr, 1, paste, collapse = ' to '))
  })
  
  # Main UI for page 2
  output$bhrUI <- renderUI({
    fluidRow(
      column(6, 
             withMathJax(h5("Insert \\(\\lambda\\)'s")), 
             # Lambda inputs (staggered by default)
             try(
             lapply(seq(rv2$ntrans()), function(i) {
               numericInput(paste0('rr', i), rv2$transNames()[i], min = 0, 
                            value = 0.1 + i/100, step = .02)
             }), TRUE)
      ),
      column(6, 
             withMathJax(h5("Insert \\(\\gamma\\)'s")), 
             # Gamma inputs (1 by default for exponential)
             try(
             lapply(seq(rv2$ntrans()), function(i) {
               numericInput(paste0('shape', i), rv2$transNames()[i], min = .02, 
                            value = 1, step = .02)
             }), TRUE)
      )
    )
  })
  
  # reactive for the baseline hazard parameters
  rv2$params <- reactive({
    params <- data.frame(expand.grid(to = rownames(rv$tmat()), from = rownames(rv$tmat()))[t(rv$tmat()),2:1])
    params$lambdas <- unlist(lapply(seq(rv2$ntrans()), function(i) pmax(input[[paste0('rr',i)]], 1e-8)))
    params$shapes <- unlist(lapply(seq(rv2$ntrans()), function(i) input[[paste0('shape',i)]]))
    params$cols <- as.character(factor(params$to, levels = levels(params$to), labels = rv$stateCols()))
    params
  })
  
  # Baseline hazard plots (height is finicky)
  # See shiny_helpers.R for more details
  output$plotBHR <- renderPlot({
    plotCurves(rv2$params(), 
               max_time = input$max_t4plots, 
               curve_type = input$bhr_plot_type)
  }, res = 100, bg="transparent")
  
  ##### Treatment Effects #####
  
  # reactiveValues list for page 3
  rv3 <- reactiveValues()
  
  # Main UI for page 3 - list of sliders, 1 by default for no true effect
  output$trtUI <- renderUI({
    verticalLayout(
      try(lapply(seq(rv2$ntrans()), function(i) {
        sliderInput(paste0('cc', i), rv2$transNames()[i], .2, 8, 
                    value = 1, step = .01, ticks = F)
      }), TRUE),
      actionButton('resetTrtUI', 'Reset all to 1')
    )
  })
  
  observeEvent(input$resetTrtUI, {
    lapply(seq(rv2$ntrans()), function(i) updateNumericInput(session, paste0('cc', i), value = 1)) 
  })
  
  
  # reactive for params including the treatment effects, to be used in simulations
  rv3$params <- reactive({
    params <- rv2$params()
    params$trtHR <- unlist(lapply(seq(rv2$ntrans()), function(i) input[[paste0('cc', i)]]))
    params
  })
  
  # Plot output for page 3 
  # See shiny_helpers.R for more details
  output$plotTRT <- renderPlot({
    plotCurves(rv3$params(), 
               max_time = input$max_t4plotTRT, 
               curve_type = input$trt_plot_type)
  }, res = 100, bg="transparent")
  
  ##### Single Simulation #####
  rv4 <- reactiveValues()
  
  # Main UI for page 4
  output$sim1UI <- renderUI({
    verticalLayout(
      inputPanel(
        tipify(
          numericInput('N', 'Sample size (per group)', min = 2, value = 250, width = '100%'),
          'Select the number of subjects per group for a single simulated data set (total = N * 2)'
        ), 
        tipify(
          sliderInput('trtpct', 'Percent in treatment group', 10, 90, 50, width = '100%'), 
          'The balance of randomization between treatment and placebo'
        ),
        tipify(
          sliderInput('censorRate', 'Censor Rate (%)', 0, 90, 20, width = '100%'),
          paste0(
            'The target censoring percentage (exponentially generated, see ',
            'tutorial for more details. Observed rate may be different if the max ', 
            'time for followup is short relative to the events occuring)'
          )
        ),
        tipify(
          numericInput('maxfollowup', 'Max time for followup', min = 5, step = 5, value = 20, width = '100%'),
          'The maximum time on study after which point all subjects will be censored'
        ),
        tipify(
          numericInput('seed', 'Seed for simulation:', min = 1, step = 1, value = sample(1:100000000, 1), width = '100%'),
          paste0('Select the seed for this single simulation (future runs will produce the ',
                 'exact same values, given the same input parameters)')
        ),
        tipify(
          actionButton('runsim1', 'Run simulation', class = 'btn-primary'),
          'Run the simulation according to the parameters on this and previous pages'
        )
      ), br(),
      disabled(
        tipify(
          downloadButton('downloadsim1'), 
          paste0('Download the simulated data. The file downloaded is a .RData file containing ',
                 'a list named "sim1Results" with the contents: simResults (a data frame of ',
                 'observed transitions for all subjects), params (a dataframe of the survival parameters ',
                 'used to run the simulations), and simDetails (a list containing the additional ',
                 'simulation parameters)')
        )
      )
    )
  })
  
  # reactive value for sim 1 results 
  # Waits until input$runsim1 is pressed
  rv4$sim1Results <- eventReactive(input$runsim1, {
    set.seed(input$seed)
    
    # If people really try this they deserve an error
    if(input$N < rv2$ntrans()) {
      N <- rv2$ntrans()
      updateNumericInput(session, 'N', value = N)
    } else N <- input$N
    
    ## Update inputs for next simulation
    updateNumericInput(session,'N2', value = input$N)
    updateSliderInput(session,'trtpct2', value = input$trtpct)
    updateSliderInput(session,'censorRate2', value = input$censorRate)
    updateNumericInput(session,'maxfollowup2', value = input$maxfollowup)
    
    # Must protect people from themselves with these inputs
    if(input$maxfollowup < .1) updateNumericInput(session, 'maxfollowup', value = .1)
    maxfollowup <- max(.1, input$maxfollowup)
    
    ## Simulate event histories ##
    # The bulk of this process, see shiny_helpers.R
    res <- mysim2(rv3$params(), N, trtpct = input$trtpct, 
                  censorRate = input$censorRate, maxfollowup = maxfollowup)
  
    # Fix any machine-level time errors
    glitchIDX <- unique(res$id[(abs(res$entry - res$exit) < 1e-6)])
    res[res$id %in% glitchIDX,] <- NA
    res$etimes[res$etimes > 1e100] <- Inf
    
    # Create variables for survfit
    res$event <- factor(res$to, levels = c('censor', (rv$stateNames()[-1])),
                        labels = c('censor', rv$stateNames()[-1]))
    res$trt <- factor(res$x1, levels = c(0:1), labels = c('Placebo', 'Treatment'))
    fo <- Surv(time = entry, time2 = exit, event) ~ trt
    
    # check if nstates == 2
    if(length(levels(res$event)) < 3) {
      res$event <- ifelse(res$event == 'censor', 0, 1)
      fo <- update(fo, Surv(exit, event) ~ .)
    } 
    
    sfit <- survfit(fo, data = res, id = id, subset = (delta == 1))
    list(res = res, sfit = sfit)
  })
  
  # Sim 1 Table 1: Transitions
  output$sim1_transitions <- renderTable({
    df <- subset(rv4$sim1Results()$res, subset = (delta == 1))
    tab <- table(df$from, df$to)
    
    # reactive input for table 
    if(length(input$transRatesYN) && input$transRatesYN == 'Rates') 
      tab <- prop.table(tab, 1)
    
    # Clean it up
    dnames <- attr(tab, 'dimnames')
    dnames[[1]] <- paste0('From ', dnames[[1]])
    dnames[[2]] <- paste0('To ', dnames[[2]])
    dimnames(tab) <- dnames
    tab <- tab[1:(input$nstates-1),2:(input$nstates + 1)]
    if(!length(dim(tab))) {
      tab <- t(as.table(tab))
      dimnames(tab)[[1]] <- paste('From', rv$stateNames()[1])
    }
    xtable(tab)
  }, rownames = TRUE, na = '')
  
  # Sim 1 Table 2: Events
  output$sim1_events <- renderTable({
    df <- subset(rv4$sim1Results()$res, subset = (delta == 1))
    df$trt <- factor(df$x1, levels = 0:1, labels = c('Placebo', 'Treatment'))
    tab <- table(df$event, df$trt)
    if(length(input$transRatesYN) && input$eventRatesYN == 'Rates') 
      tab <- prop.table(tab, 1)
    xtable(tab)
  }, rownames = TRUE, na = '')
  
  # Sim 1 Table 3: survfit summary
  output$sim1_survfit <- renderTable({
    sfit <- rv4$sim1Results()$sfit
    sfit$states[length(sfit$states)] <- rv$stateNames()[1]
    tab <- as.data.frame(summary(sfit, rmean = input$rmean)$table)
    tab[,1] <- as.character(round(tab[,1]))
    tab[,2] <- as.character(round(tab[,2]))
    tab[,3] <- as.character(round(tab[,3], 4))
    names(tab) <- c('N', 'N events', 'Restricted Mean time in state')
    nn <- nrow(tab)
    
    # End table in proper order
    rbind(tab[(nn-1):nn,], tab[1:(nn-2),])
  }, rownames = TRUE, align = 'lccc')
  
  # Sim 1 Table 4: Cox Model
  output$sim1_cox <- renderTable({
    res <- rv4$sim1Results()$res
    res <- na.omit(res)
    res$event <- factor(res$to, levels = c('censor', (rv$stateNames()[-1])),
                        labels = c('censor', rv$stateNames()[-1]))
    
    if(length(levels(res$event)) < 3) {
      res$event <- ifelse(res$event == 'censor', 0, 1)
      df_mstate <- res
      fo <- Surv(exit, event) ~ x1
    } else {
      
      # If necessary, expand via mstate
      res <- res[res$to != 'censor',]
      class(res) <- c('msdata', 'data.frame')
      
      transMat <- rv$tmat()
      transMat[transMat] <- 1:sum(transMat)
      transMat[!transMat] <- NA
      
      attr(res, 'trans') <- transMat
      res$trans <- as.numeric(paste0(as.numeric(res$from), as.numeric(res$to)))
      res$status <- res$delta
      covs <- c('x1')
      df_mstate <- expand.covs(res, covs, append = T)
      fo <- as.formula(
        paste0('Surv(time = entry, time2 = exit, status) ~ strata(trans) + ', 
               paste0('x1.', 1:length(unique(res$trans)), collapse  = ' + '))
      )
      
    }
    
    fitMS <- coxph(fo, data = df_mstate, method = 'breslow')
    names(fitMS$coefficients) <- rv2$transNames()
    s <- as.data.frame(summary(fitMS)$coef)
    s$`Pr(>|z|)` <- prettyP(s$`Pr(>|z|)`)
    xtable(s)
  }, rownames = TRUE, digits = 3)
  
  # Combine simulation 1 tables
  output$sim1tabs <- renderUI({
    tabsetPanel(
      tabPanel('Transitions',
               tableOutput('sim1_transitions'),
               radioButtons('transRatesYN', NULL, c('Counts', 'Rates'), 'Counts', inline = TRUE)),
      tabPanel('Events', 
               tableOutput('sim1_events'), 
               radioButtons('eventRatesYN', NULL, c('Counts', 'Rates'), 'Counts', inline = TRUE)),
      tabPanel('Survfit', 
               tableOutput('sim1_survfit'), 
               tipify(
                 numericInput('rmean', 'Max time for restricted mean', min = 0, value = 15),
                 'Consult the help for print.survfit for details (see references)'
               )),
      tabPanel('Cox Model', tableOutput('sim1_cox'))
    )
  })
  
  # Once button is clicked, show sim1 plots and table
  observeEvent(!is.null(rv4$sim1Results()), {
    toggleState('downloadsim1')
    show('maxt4plotSim')
  })
  
  # Sim 1 main plot
  output$sim1plot <- renderPlot({
    
    ylabel <- 'Current State'
    if(input$nstates == 2) ylabel <- 'Survival'
    
    plot(
      rv4$sim1Results()$sfit,
      col = rep(rv$stateCols()[-1], each = 2),
      lwd = 2,
      lty = 1:2,
      xlab = "Time",
      ylab = ylabel, 
      xlim = c(0, input$maxt4plotSim)
    )

    legend('right',
           c(rv$stateNames()[-1], '','Treatment', 'Placebo'),
           col = c(rv$stateCols()[-1], NA, 1, 1), bty = 'n', lwd = 2,
           lty = c(rep(1, input$nstates), 2)
    )
  }, bg="transparent")
  
  # Helper for downloading the first simulation
  output$downloadsim1 <- downloadHandler(
    filename = function() {
      paste(
        paste('sim1Results', 
              strsplit(timestamp(prefix = '', suffix = ''), ' ')[[1]],
              collapse = '_'), 
        '.RData')
    },
    content = function(file) {
      # Write to a file specified by the 'file' argument
      sim1Results <- list(
        simResults = rv4$sim1Results(),
        params = rv3$params(),
        'simDetails' = list(
          npergroup = input$N,
          trtpct = input$trtpct,
          maxfollowup = input$maxfollowup,
          seed = input$seed1
        )
      )
      
      save(sim1Results, file = file)
    }
  )
  

  ##### Power simulation #####
  
  rv5 <- reactiveValues()
  
  # Main UI for simulation 2
  output$sim2UI <- renderUI({
    verticalLayout(
      inputPanel(
        tipify(
          numericInput('nSim', 'Number of Simulations', value = 10, min = 10, step = 10, width = '100%'),
          'The number of full data sets to simulate (this would be akin to running the study X amount of times)'
        ),
        tipify(
          numericInput('N2', 'Sample size (per group)', value = 350, width = '100%'),
          'Select the number of subjects per group for a single simulated data set (total = N * 2)'
        ),
        tipify(
          sliderInput('trtpct2', 'Percent in treatment group', 10, 90, 50, width = '100%'), 
          'The balance of randomization between treatment and placebo'
        ),
        tipify(
          sliderInput('censorRate2', 'Censor Rate (%)', 0, 90, 20, width = '100%'), 
          'The target censoring percentage (if there is no maximum time for followup)'
        ),
        tipify(
          numericInput('maxfollowup2', 'Max time for followup', min = 5, step = 5, value = 20, width = '100%'),
          'The maximum time on study after which point all subjects will be censored'
        ),
        tipify(
          radioButtons('indivAlphas', "Set alpha for transitions:", c('Together', 'Separately'), inline = T),
          paste(
            'For the power calculations, this refers to the significance level for a 2-sided',
            ' test of null hypothesis for each transition. Note that',
            ' it can be changed after the simulations have been run'
          )
        ),
        conditionalPanel(
          'input.indivAlphas ==  "Separately"',
          inputPanel(
            try(
              lapply(seq(rv2$ntrans()), function(i) {
                numericInput(paste0('alpha', i), rv2$transNames()[i], min = 0, 
                             value = 0.05, step = .001)
              }), TRUE)
          )
        ),
        conditionalPanel(
          'input.indivAlphas ==  "Together"',
          inputPanel(numericInput('alpha', 'For each transition', value = .05, 
                                  min = 0, step = .001, width = '100%'))
        ),
        tipify(
          numericInput('seed2', 'Seed for simulations:', min = 1, step = 1, value = sample(1:1000000, 1), width = '100%'),
          paste0('Select the seed for these simulations (future runs will produce the ',
                 'exact same values, given the same input parameters)')
        ),
        tipify(
          actionButton('runsim2', 'Run simulation', class = 'btn-primary'),
          'Run the simulation according to the parameters on this and previous pages'
        )
      ),
      br(),
      disabled(
        tipify(
          downloadButton('downloadsim2'),
          paste0('This button will download a list called "sim2Results" with the following components: ',
                 '"simResults", a data frame storing all the coefficients and pvalues from each simulated data set; ', 
                 '"params", a data frame of parameter values, and "simDetails", a list containing the simulation ',
                 'inputs on this page')
          
        )
      )
    )
  })
  
  # Store individual alphas in reactive value
  rv5$alphas <- reactive({
    alphas <- c()
    for(i in 1:rv2$ntrans()) alphas <- c(alphas, input[[paste0('alpha', i)]])
    matrix(alphas, nrow = input$nSim, ncol = length(alphas), byrow = T)
  })
  
  # Perform simulation, store as reactive
  rv5$sim2Results <- eventReactive(input$runsim2, {
    set.seed(input$seed2)
    
    # Check sample size is higher than # of transitions
    if(input$N2< rv2$ntrans()) {
      N <- rv2$ntrans()
      updateNumericInput(session, 'N2', value = N)
    } else N <- input$N2
    
    # If the tmat is invalid, stop
    if(any(colSums(as.matrix(rv$tmat()[-input$nstates, -1])) == 0) | 
       any(rowSums(as.matrix(rv$tmat()[-input$nstates, -1])) == 0)) return('Invalid Transition matrix')
    
    # Check time of followup
    if(input$maxfollowup2 < .1) updateNumericInput(session, 'maxfollowup2', value = .1)
    maxfollowup2 <- max(.1, input$maxfollowup2)
    
    ## Set up results matrix
    coefs <- pvals <- converged <- matrix(NA, ncol = rv2$ntrans(), nrow = input$nSim)
    
    ## Iterate through all sims
    withProgress(message = 'Running Simulations', value = 0, {
      for(i in 1:input$nSim) {
        
        # Simulate and clean event histories
        res <- mysim2(rv3$params(), N, trtpct = input$trtpct2, censorRate = input$censorRate2, maxfollowup = maxfollowup2)
        res[(abs(res$entry - res$exit) < 1e-6),] <- NA
        res <- na.omit(res)
        res$event <- factor(res$to, levels = c('censor', (rv$stateNames()[-1])),
                            labels = c('censor', rv$stateNames()[-1]))
        
        # Perform mstate expanding
        if(length(levels(res$event)) < 3) {
          res$event <- ifelse(res$event == 'censor', 0, 1)
          df_mstate <- res[res$delta == 1,]
          fo <- Surv(exit, event) ~ x1
        } else {
          res <- res[res$to != 'censor',]
          class(res) <- c('msdata', 'data.frame')
          
          transMat <- rv$tmat()
          transMat[transMat] <- 1:sum(transMat)
          transMat[!transMat] <- NA
          
          attr(res, 'trans') <- transMat
          res$trans <- as.numeric(paste0(as.numeric(res$from), as.numeric(res$to)))
          res$status <- res$delta
          covs <- c('x1')
          df_mstate <- expand.covs(res, covs, append = T)
          fo <- as.formula(
            paste0('Surv(time = entry, time2 = exit, status) ~ strata(trans) + ', 
                   paste0('x1.', 1:length(unique(res$trans)), collapse  = ' + '))
          )
          
        }
        
        # myTryCatch will catch errors + warning messages
        # This attempts a cox model
        fitMS = myTryCatch(coxph(fo, data = df_mstate, method = 'breslow'))
        
        # If error isn't null, there was an issue with the fit and
        # no coefficients converged
        if(length(fitMS$error)) {
          pvals[i,] <- coefs[i,] <- NA
          converged[i,] <- fitMS$error
        } else {
          s <- summary(fitMS$value)
          
          ## Build in handling when beta infinite
          pvals[i,] <- s$coefficients[,5]
          coefs[i,] <- coef(fitMS$value)
          converged[i,] <- 'yes'
          
          # Check for if likelihood converged for all variables
          if(length(fitMS$warning)) {
            w <- fitMS$warning[grep('Loglik', fitMS$warning)]
            bad <- as.logical(unlist(lapply(c(1:rv2$ntrans()), function(i) length(grep(i, w)))))
            # Store specific warning
            converged[i,bad] <- fitMS$warning[1]
          }
        }
        # Increment progress
        incProgress(1/input$nSim, detail = paste("Doing sim", i))
      }
    })
    
    list(coefs = coefs, pvals = pvals, converged = converged)
  })

  # Power table for simulation 2
  output$sim2tab <- renderTable({
    res <- rv5$sim2Results()
    if(is.character(res)) return(NULL)
    if(dim(res$pvals)[1] != input$nSim) return(NULL)
    
    # Calculate power + CI
    if(input$indivAlphas == 'Together') {
      rej <- res$pvals < input$alpha
    } else {
      rej <- res$pvals < rv5$alphas()
    }
    pctReject <- as.character(round(apply(rej, 2, function(x) mean(x, na.rm = T)), 3))
    pctRejectCI <- apply(rej, 2, function(x) {
      try1 <- binom.test(sum(x, na.rm = T), length(x))
      paste0('[', paste(round(try1$conf.int,3), collapse = ', '), ']')
    })
    
    # Power to detect any difference
    anyrej <- apply(rej, 1, any)
    anyrej_pretty <- as.character(round(mean(anyrej, na.rm = T), 3))
    try1 <- binom.test(sum(anyrej, na.rm = T), length(anyrej))
    anyrejCI <- paste0('[', paste(round(try1$conf.int,3), collapse = ', '), ']')
    
    # Calculate mean/sd of coef estimates
    meanEffect <- as.character(round(apply(res$coefs, 2, function(x) (exp(mean(x, na.rm = T)))), 3))
    seEffect <- as.character(round(apply(res$coefs, 2, function(x) exp(sd(x, na.rm = T))), 3))
    
    # Make pretty the true values
    trueDiff <- as.character(round(rv3$params()$trtHR,2))
    
    # Check convergence
    nconverged <- colSums(res$converged == 'yes')
    
    # Amalgamate
    tab <- cbind(trueDiff, meanEffect, seEffect, pctReject, pctRejectCI, nconverged)
    tab <- rbind(tab, c('', '', '', anyrej_pretty, anyrejCI, ''))
    
    # Make pretty
    colnames(tab) <- c('True HR', 'Geometric mean estimated HR', 'Geometric SD', 
                       'Proportion p < alpha', '95% Clopper-Pearson CI', 'Number converged')
    rownames(tab) <- c(rv2$transNames(), 'Any effect')
    tab[,c('True HR', 'Geometric mean estimated HR', 'Geometric SD', 'Number converged', 
           'Proportion p < alpha', '95% Clopper-Pearson CI')]
  }, rownames = TRUE, align = 'c')
  
  # Plot for simulation 2
  output$sim2plot <- renderPlot({
    if(is.character(rv5$sim2Results())) {
      plot.new()
      legend('center', 'Invalid Transition Matrix', bty = 'n')
      return(NULL)
    }
    
    # Adjust margins depending on length of transnames
    marAdj <- max(nchar(rv2$transNames()))
    par(mar = c(5,marAdj/2,4,2))
    
    # Don't attempt to plot coefs that didn't converge
    yvals <- ifelse(
      rv5$sim2Results()$converged == 'yes',
      rv5$sim2Results()$coef,
      NA
    )
    
    yrange <- range(1, exp(yvals), na.rm = T)
    
    boxplot(
      exp(yvals[,rv2$ntrans():1]),
      lwd = 2,
      xlab = "Estimated Hazard Ratios",
      horizontal = TRUE, 
      ylabs = 'Transition', 
      yaxt = 'n',
      ylim = yrange
    )
    
    # plot the truth
    points(y = rv2$ntrans():1, x = rv3$params()$trtHR, col = 3, pch = 20, cex = 1.5)
    
    # Warn if not all coefs plotted
    notPlotted <- colSums(rv5$sim2Results()$coef > 1 & rv5$sim2Results()$converged != 'yes')
    text(y = (rv2$ntrans():1)[notPlotted > 0], x = max(yrange), labels = 'Not all coefs plotted', pos = 2)
    
    # Plot axes
    axis(2, rv2$ntrans():1, rv2$transNames(), las = 1)
    abline(v = 1, lty = 2)
    
  }, bg="transparent")
  
  # Download button for sim2 
  output$downloadsim2 <- downloadHandler(
    filename = function() {
      paste('sim2Results',
        paste(strsplit(timestamp(prefix = '', suffix = ''), ' ')[[1]], collapse = ''), 
        '.RData')
    },
    content = function(file) {
      # Write to a file specified by the 'file' argument
      sim2Results <- list(
        simResults = rv5$sim2Results(), 
        'params' = rv3$params(), 
        'simDetails' = list(
          nSim = input$nSim,
          npergroup = input$N2,
          trtpct = input$trtpct2,
          maxfollowup = input$maxfollowup2,
          alpha = input$alpha,
          seed = input$seed2
        )
      )
      save(sim2Results, file = file)
    }
  )
  
  observeEvent(!is.null(rv5$sim2Results()), enable('downloadsim2'))
  
  ## If user changes param on prior page, 
  # hide plots + tables until "runsim" pushed again
  # Also disable download
  hidePlots <- reactive({
    try(list(
      nstates = input$nstates,
      lambdas = (lapply(seq(rv2$ntrans()), function(i) pmax(input[[paste0('rr',i)]], 1e-8))),
      shapes = (lapply(seq(rv2$ntrans()), function(i) pmax(input[[paste0('shape',i)]], 1e-8))),
      lambdas = (lapply(seq(rv2$ntrans()), function(i) pmax(input[[paste0('rr',i)]], 1e-8))),
      covs = (lapply(seq(rv2$ntrans()), function(i) input[[paste0('cc', i)]])),
      tmat = (unname(rv$tmat()))
    ), silent = TRUE)
  })
  observeEvent({
    hidePlots()
    }, {
      hide('sim1plot')
      hide('sim1tabs')
      disable('downloadsim1')
      hide('sim2plot')
      hide('sim2tab')
      disable('downloadsim2')
  })
  
  # enable download /show output upon hitting of runsim1 button
  observeEvent(input$runsim1, {
    show('sim1plot')
    show('sim1tabs')
    enable('downloadsim1')
  })
  
  # enable download /show output upon hitting of runsim2 button
  observeEvent(input$runsim2, {
    show('sim2plot')
    show('sim2tab')
    enable('downloadsim2')
  })
  
  ##### References #####
  
  ## Add tutorial download handler for link in references
  # output$tutorialDL <- downloadHandler(
  #   filename = 'tutorial.pdf',
  #   content = function(file) {
  #     file.copy("www/tutorial.pdf", file)
  #   }
  # )
  
  output$refs <- renderUI({
    citesHTML <- c()
    for(i in list.of.packages) {
      citesHTML <- c(citesHTML, 
                     paste('<strong> <code>', i, '</strong> </code>     ', 
                           paste(capture.output(print(citation(i), style = 'html')), collapse = ' '), 
                           collapse = ' ')
                     )
    }
    HTML(
      paste(
        '<ul> <li> ', 
        paste(citesHTML, collapse = '</li> <li>'),
        '</li> </ul>'
      )
    )
  })
  
  ##### Overall functionality #####
  
  ## Make sure all ui's render together
  outputOptions(output, 'bhrUI', suspendWhenHidden = FALSE)
  outputOptions(output, 'trtUI', suspendWhenHidden = FALSE)
  outputOptions(output, 'plotBHR', suspendWhenHidden = FALSE)
  outputOptions(output, 'plotTRT', suspendWhenHidden = FALSE)
  outputOptions(output, 'sim1UI', suspendWhenHidden = FALSE)
  outputOptions(output, 'sim2UI', suspendWhenHidden = FALSE)
  
  ## add popovers
  addPopover(
    session,
    'statefig', '',
    paste0('Verify that this is the structure of the multistate design ',
           'after checking the relevant boxes and renaming the states in your model',
            'on the left')
  )
  addPopover(
    session,
    'tmatUI2', '',
    paste0('Select the transitions that are possible and that you wish to model. ',
           'The first state is assumed to be the entry state, <i> to </i> which subjects cannot ',
           'transition. The last state is the final state <i> from </i> which subjects cannot transition')
  )
  addPopover(
    session,
    'plotBHR',
    '',
    paste0(
      'These plots indicate the assumptions made about each',
      ' transition probability given the inputs on the left'
    ),
    placement = 'bottom'
  )
  addPopover(
    session,
    'plotTRT',
    '',
    paste0(
      'The dotted line refers to the treatment group, the solid line refers to the placebo group'
    )
  )
  addPopover(
    session,
    'sim1plot',
    '',
    paste0(
      'This is a "Current State" plot - Each line refers to the probability that a subject ',
      'is in that state at each time point.'
    ),
    placement = 'top'
  )
  addPopover(
    session,
    'sim2plot',
    '',
    paste0(
      'This plot shows the estimated hazard ratios from a cox regression for each simulated data set'
    ),
    placement = 'top'
  )
  addPopover(
    session,
    'bhrUI',
    '',
    paste0(
      'Choose the lambdas and the gammas for each transition ',
      'corresponding to the formula above. Note: setting gamma = 1 ' ,
      'signifies an exponential distribution with rate lambda. Verify the ', 
      'distributions look correct in the plots on the right'
    ),
    placement = 'bottom'
  )
  addPopover(
    session,
    'trtUI',
    '',
    paste0(
      'For each transition, choose the "true" hazard ratio between the ',
      'treatment and placebo group corresponding to the formula above. ',
      'Verify they look correct in the plots on the right'
    ),
    placement = 'bottom'
  )
  
  addPopover(
    session,
    'sim2tab',
    '',
    paste0(
      'Summary results for all of the simulations are presented here. ', 
      'Note that if the number of converged coefficients is not ',
      'equal to the number of simulations, there could be problems with ', 
      'some of the fits. The downloaded file contains error/warning codes ',
      'for further information as to why some fits failed'
    ),
    placement = 'bottom'
  )
  
  # Session info
  output$sessionInfo <- renderPrint(sessionInfo())
}