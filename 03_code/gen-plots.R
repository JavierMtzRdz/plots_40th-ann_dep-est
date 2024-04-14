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


# Mountains -----
## V1----
tibble(x = 1:7,
       m1 = c(0, 30, 20, 50, 20, 30, 0)+30,
       m2 = c(0, 30, 20, 50, 20, 30, 0)+30,
       m5 = c(0, 30, 20, 50, 20, 30, 0)) %>% 
  pivot_longer(-x) %>% 
  ggplot(aes(x=x, y=value, fill=name)) + 
  geom_area(alpha=0.75 , linewidth=1, colour="#041F3F") +
  scale_y_continuous(expand = expansion(mult = c(0.0, 2))) +
  scale_fill_manual(values = c("#D4DBDB", #"#D4DBDB",
                               "#7A90A2",
                               "#041F3F"),
                    guide = "none") +
  theme_void()

walk(c("png", "svg"),
     ~ggsave(paste0("02_figs/Mountains.",
                    .),
             bg = "transparent",
             width = 100,                 # Ancho de la gráfica
             height = 100,
             units = "mm",
             dpi = 500))

## V2----
set.seed(123)
tibble(x = 1:7,
       m0 = rnorm(7, 0.6, 0.1),
       m1 = rnorm(7, 1, 0.1),
       m2 = c(0.2, 0.7, 0.5, 2, .6, 0.6, 0)*1.2,
       m3 = c(1.5, 1.4, 1, 0, 0, 0, 0)*1.1,
       m4 = c(0, 0, 0, 0.5, 0.8, 1.4, 2)+1) %>% 
  pivot_longer(-x) %>% 
  ggplot(aes(x=x, y=value, fill=name)) + 
  geom_area(alpha=0.75 , linewidth=2, colour="#041F3F") +
  scale_y_continuous(expand = expansion(mult = c(0.0, 2))) +
  scale_x_continuous(expand = expansion(mult = c(0.0, 0))) +
  scale_fill_manual(values = c("white",
                               "#D4DBDB",
                               "#7A90A2",
                               "#072D5C",
                               "#041F3F"),
                    guide = "none") 

walk(c("png", "svg"),
     ~ggsave(paste0("02_figs/mountains-v2.",
                    .),
             bg = "transparent",
             width = 100,                 # Ancho de la gráfica
             height = 100,
             units = "mm",
             dpi = 500))


## V3----
tibble(x = 1:19,
       m0 = c(0, NA, NA, NA, NA, 4, 3, NA, NA, 6,
              NA, NA, 3, 4, NA, NA, NA, NA, 0) + 2,
       m1 = c(0, NA, NA, NA, NA, 4, 3, NA, NA, 6,
              NA, NA, 3, 4, NA, NA, NA, NA, 0) + 1,
       m2 = c(0, NA, NA, NA, NA, 4, 3, NA, NA, 6,
              NA, NA, 3, 4, NA, NA, NA, NA, 0),
       m3 = c(3, NA, 3.5, NA, NA,  2.2, NA, NA, 1.8,
              NA, NA, NA, 0, NA, NA, NA, NA, NA, NA)*1.3,
       m4 = c(0, NA, NA, NA, 0, NA, 0.2, NA, 1, NA, 1.5, 
              NA, 1.4, NA, 3, NA, 3.3, NA, 3.4)*1.3) %>% 
  pivot_longer(-x) %>% 
  ggplot(aes(x=x, y=value, fill=name)) + 
  geom_area(alpha=1,
            position = 'identity',
            linewidth=2, 
            linejoin = 'mitre',
            colour="#041F3F") +
  scale_y_continuous(expand = expansion(mult = c(0.0, 2))) +
  # scale_x_continuous(expand = expansion(mult = c(0.0, 0))) +
  scale_fill_manual(values = c("white",
                               "#D4DBDB",
                               "#7A90A2",
                               "#072D5C",
                               "#041F3F"),
                    guide = "none") 

walk(c("png", "svg"),
     ~ggsave(paste0("02_figs/mountains-v3.",
                    .),
             bg = "transparent",
             width = 100,                 # Ancho de la gráfica
             height = 100,
             units = "mm",
             dpi = 500))



## V4----
tibble(x = 1:19,
       m0 = c(3, 4, 3, 4, 3, 4, 3, NA, NA, 6,
              NA, NA, 3, 4, 3, 4, 3, 4, 3) + 2,
       m1 = c(3, 4, 3, 4, 3, 4, 3, NA, NA, 6,
              NA, NA, 3, 4, 3, 4, 3, 4, 3) + 1,
       m2 = c(0, NA, NA, NA, NA, 4, 3, NA, NA, 6,
              NA, NA, 3, 4, NA, NA, NA, NA, 0),
       m3 = c(3, NA, 3.5, NA, NA,  2.2, NA, NA, 1.8,
              NA, NA, NA, 0, NA, NA, NA, NA, NA, NA)*1.3,
       m4 = c(0, NA, NA, NA, 0, NA, 0.2, NA, 1, NA, 1.5, 
              NA, 1.4, NA, 3, NA, 3.3, NA, 3.4)*1.3) %>% 
  pivot_longer(-x) %>% 
  ggplot(aes(x=x, y=value, fill=name)) + 
  geom_area(alpha=1,
            position = 'identity',
            linewidth=2, 
            linejoin = 'mitre',
            colour="#041F3F") +
  scale_y_continuous(expand = expansion(mult = c(0.0, 2))) +
  # scale_x_continuous(expand = expansion(mult = c(0.0, 0))) +
  scale_fill_manual(values = c("white",
                               "#D4DBDB",
                               "#7A90A2",
                               "#072D5C",
                               "#041F3F"),
                    guide = "none") 

walk(c("png", "svg"),
     ~ggsave(paste0("02_figs/mountains-v4.",
                    .),
             bg = "transparent",
             width = 100,                 # Ancho de la gráfica
             height = 100,
             units = "mm",
             dpi = 500))

## V5----
tibble(x = 1:19,
       m0 = c(3, 4, 3, 2, 3, 4, 3, NA, NA, 6,
              NA, NA, 3, 4, 3, 2, 3, 4, 3) + 2,
       m1 = m0 - 1,
       m2 = m0 -2,
       m3 = c(4, NA, 3.5, NA, NA,  2.2, NA, NA, 1.8,
              NA, NA, NA, 0, NA, NA, NA, NA, NA, NA)*1.3,
       m4 = c(0, NA, NA, NA, 0, NA, 0.2, NA, 1, NA, 1.5, 
              NA, 1.4, NA, 3, NA, 3.3, NA, 4)*1.3) %>% 
  pivot_longer(-x) %>% 
  ggplot(aes(x=x, y=value, fill=name)) + 
  geom_area(alpha=1,
            position = 'identity',
            linewidth=2, 
            linejoin = 'mitre',
            colour="#041F3F") +
  scale_y_continuous(expand = expansion(mult = c(0.0, 1.7))) +
  # scale_x_continuous(expand = expansion(mult = c(0.0, 0))) +
  scale_fill_manual(values = c("white",
                               "#D4DBDB",
                               "#7A90A2",
                               "#072D5C",
                               "#041F3F"),
                    guide = "none")

walk(c("png", "svg"),
     ~ggsave(paste0("02_figs/mountains-v5.",
                    .),
             bg = "transparent",
             width = 100,                 # Ancho de la gráfica
             height = 100,
             units = "mm",
             dpi = 500))
