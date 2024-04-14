##################################################################
##                     Proyecto: Logo plots                     ##
##################################################################
##
## Descipción:     This script generates the plots used to create 
##                 the 40th-anniversary logo for UBC’s Department 
##                 of Statistics.
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


# Bars on 0 ----------
bars <- tibble(x = seq(0,5-0.5, 0.5),
               y = dexp(seq(0,5-0.5, 0.5), 0.4))

bars %>% 
  ggplot(aes(x = x, 
             y = y)) +
  geom_col(fill = "#041F3F") 

walk(c("png", "svg"),
     ~ggsave(paste0("02_figs/bars.",
                    .),
             bg = "transparent",
             width = 120,
             height = 120,
             units = "mm",
             dpi = 500))
