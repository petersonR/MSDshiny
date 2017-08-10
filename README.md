# MSDshiny -- The Multistate Simulation Designer `shiny` Application

The Multistate Simulation Designer application provides a useful and streamlined way to plan and power clinical trials with multistate outcomes. 

![](figures/multistate_title_page.PNG)

# Background

This `shiny` app was developed with the goal of aiding in the planning and powering of clinical trials with multistate outcomes. The app allows for users to explore various multistate structures, and within them to explicitly visualize the assumptions they are making when it comes to baseline hazards and assumed treatment effects. Once all of the assumptions are made, it is straightforward to perform a simulation of either a single study (which provides a snapshot of the results from an actual study if it was to be performed), or a large set of studies (which provides information on the power to detect treatment effects and the variability in the estimates from the study). 

# Getting Started

It is straightforward to initiate an instance of the Multistate Simulation Designer using the 3 steps below (it is assumed that R is installed):

1) Open an R session
2) Make sure `shiny` is installed and attached:

`if(!require(shiny)) install.packages('shiny')`

`library(shiny)`

3) Run the code: 

`
runGitHub("petersonRA/MSDshiny")
`


## Multistate Structure

This tab is for setting the multistate structure

## Baseline Hazards

This tab is for making assumptions about the baseline hazards for each transition

## Treatment Effects

This tab is for the assumed treatment effects on each transition

## Single Simulation

This will run a single "simulated study", and present the results.

## Power Simulation

This will run the simulation many times in order to find the operating characteristics of the app.

## References

The References tab cites the most important packages used in the development of the application, as well as provides the session info to aid in reproducability. 