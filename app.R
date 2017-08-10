# Author: Ryan Peterson
# Date Created: 6/27/2017
# Date Modified: 7/18/2017
# Description: Shiny Application for multistate simulation design

## Makes sure packages are installed
list.of.packages <-
  c('base', "survival", 'mstate', 'diagram', "shiny", 'shinyjs',
    'colourpicker', 'xtable',  'shinyBS')
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')

library(shiny)
library(survival)
library(shinyjs)
library(mstate)
library(colourpicker)
library(xtable)
library(diagram)
library(shinyBS)

# setwd('rpgm/simMSM_shiny')

remove(list = objects())

source('shiny_helpers.R')
source('plot_statefig.R')

runApp('.')
