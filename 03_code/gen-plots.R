##################################################################
##                     Proyecto: Logo plots                     ##
##################################################################
##
## Descipción:     
##                 
##
## Author:         Javier Mtz.-Rdz.  
##
## Creation date:  2024-04-06
##
## Email:          javier.mr@stat.ubc.ca
##
## ---------------------------
## Notes:          
## ---------------------------

# Setup ----
## Packages to use ----
pacman::p_load(tidyverse, janitor, writexl, 
               readxl, scales)

## Set theme ------
theme_set(theme_void())

options(ggplot2.discrete.colour = c("#041F3F", "#072D5C", "#7A90A2",
                                    "#D4DBDB", "white"),
        ggplot2.discrete.fill = c("#041F3F", "#072D5C", "#7A90A2",
                                  "#D4DBDB", "white"))

## Specify locale ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8")

## Disable scientific notation ----
options(scipen = 999)