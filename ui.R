# Author: Ryan Peterson
# Date Created: 6/28/2017
# Date Modified: 8/9/2017
# Description: Shiny UI for Multistate Simulation Designer

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

ui <- fluidPage(
  
  # Meta-information
  theme = "bootstrap.css", # Gives app unique feel (Copyright (c) 2013 Thomas Park)
  useShinyjs(),  # allows use of shinyjs package
  withMathJax(), # allows use of mathematical symbols
  titlePanel('Multistate Simulation Designer'),
  
  ### Main body ###
  tabsetPanel(
    id = 'tabs',
    
    # Tab 1: Multistate Structure
    tabPanel(
      'Multistate Structure',
      fluidRow(
        column(
          width = 4,
          sliderInput('nstates', 'Number of states in model', min = 2, max = 5,
                      value = 4, step = 1, round = T, ticks = F),
          bsTooltip(
            "nstates",
            "The number of states (changes to this will reset all other inputs)",
            options = list(container = 'body')
          ),
          fluidRow(column(6, uiOutput('tmatUI1.1')),
                   column(5, uiOutput('tmatUI1.2')))
        ), 
        column(3, 
               h4('For each "from" group, select possible transitions'),
               uiOutput('tmatUI2')),
        column(5, plotOutput('statefig', height = '650px', width = '450px'))
      )
    ),
    
    # Tab 2: Baseline Hazards
    tabPanel(
      'Baseline Hazards',
      fluidRow(
        column(4, uiOutput('bhrUI')),
        column(8, 
               fluidRow(
                 column(4, weibullPar), # See shiny_helpers.R
                 column(4,
                        radioButtons(
                          'bhr_plot_type',
                          'Select Plot Type',
                          choices = list(
                            'Survival' = 'Surv',
                            'Hazard' = 'Haz',
                            'Cumulative Hazard' = 'CHaz'
                          ),
                          selected = 'Surv'
                        )),  
                 column(
                   4, 
                   tipify(
                     numericInput('max_t4plots', 'Max time plotted', value = 15),
                     paste0(
                       'Enter the maximum time on these plots. In this app, time is ',
                       'all relative to these hazards; it can be interpreted ',
                       'on any scale so long as it is consistent across all plots in the app'
                     )
                   ))
               ),
               plotOutput('plotBHR', width = '100%', height = '700px'))
      )
    ), 
    
    # Tab 3: Treatment effects
    tabPanel(
      'Treatment effects',
      fluidRow(
        column(
          3,
          h4('Input Hazard Ratios for treatment'),
          uiOutput('trtUI')
        ),
        column(9, 
               fluidRow(
                 column(4, weibullPar), 
                 column(4,
                        radioButtons(
                          'trt_plot_type',
                          'Select Plot Type',
                          choices = list(
                            'Survival' = 'Surv',
                            'Hazard' = 'Haz',
                            'Cumulative Hazard' = 'CHaz'
                          ),
                          selected = 'Surv'
                        )),
                 column(4, numericInput('max_t4plotTRT', 'Max time plotted', value = 15))
               ),
               plotOutput('plotTRT', width = '100%', height = '700px'))
      )
    ),
    
    # Tab 4: Single Simulation
    tabPanel(
      'Single Simulation', 
      fluidRow(
        column(
          4, 
          h4('Enter simulation info'),
          uiOutput('sim1UI')
        ),
        column(
          8,
          hidden(tipify(
            numericInput(
              'maxt4plotSim',
              'Max time plotted',
              min = 5,
              step = 5,
              value = 20
            ),
            paste0(
              'Enter the maximum time on these plots. In this app, time is ',
              'all relative to the hazards on prior pages; it can be interpreted ',
              'on any scale so long as it is consistent across all plots in the app'
            )
          )),
          plotOutput('sim1plot'),
          uiOutput('sim1tabs')
        )
      )
    ),
    
    # Tab 5: Power simulation
    tabPanel(
      'Power Simulation',
      fluidRow(column(4, 
                      h4('Enter simulation info'),
                      uiOutput('sim2UI')),
               column(8,
                      plotOutput('sim2plot'),
                      tableOutput('sim2tab')))
    ),
    
    # Tab 6: References
    tabPanel(
      'References',
      # fluidRow(
      #   column(
      #     12,
      #     align = 'center',
      #     tags$h5(
      #       paste(
      #         'This application is coded entirely in R. For further',
      #         'assistance, the tutorial can be downloaded from the link below.'
      #       )
      #     ),
      #     downloadLink('tutorialDL', label = "Download tutorial", class = NULL)
      #   )), 
      tags$h5(paste0(
        'The application relies on several R packages:'
      )),
      tags$br(),
      uiOutput('refs'),
      tags$br(), tags$h5('The session info is presented below'),
      verbatimTextOutput('sessionInfo')
    )
  ),
  br(),
  
  # Footer
  hr(
    HTML('Version 1.0 ---- &#169; 2017 Mayo Foundation for Medical Education and Research'),
    p(
      'App developed by R. A. Peterson',
      br(),
      'Notice a bug? Please ',
      a(href = 'mailto:Peterson.Ryan1@mayo.edu', 'email me')
      # , 
      # br(),
      # HTML('Last update: ', format(Sys.Date(), '%m/%d/%Y'))
    )
  )
)
