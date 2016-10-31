library(dplyr)
library(tidyr)
library(scales)
library(ggplot2)
library(shiny)

source("./functions.R")

HPI_AT_metro <- Download_HPI_AT_metro()

years <- sort(unique(HPI_AT_metro$Year))
states <- sort(unique(HPI_AT_metro$State))
