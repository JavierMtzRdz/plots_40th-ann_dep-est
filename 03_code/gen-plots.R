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
               readxl, scales, qqman, arm,
               ggarrow)

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

# Manhattan plot - sky
don <- gwasResults %>% 
  group_by(CHR) %>% 
  summarise(chr_len=max(BP)) %>% 
  mutate(tot=cumsum(chr_len)-chr_len) %>%
  left_join(gwasResults, ., by=c("CHR"="CHR")) %>%
  arrange(CHR, BP) %>%
  mutate( BPcum=BP+tot)

don %>% 
  # sample_n(1000) %>% 
  ggplot(aes(x=BPcum, y=log10(P))) +
  geom_point(aes(color=as.factor(CHR)), 
             alpha=.8, size=0.5,stroke = 0) +
  scale_color_manual(values = rep(c("#335887",
                                    "#072D5C"), 22),
                     guide = "none") +
  coord_radial(start = -2.05,
               end = 2.05) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_void()

walk(c("png", "svg"),
     ~ggsave(paste0("02_figs/sky.",
                    .),
             bg = "transparent",
             width = 100,                 # Ancho de la gráfica
             height = 100,
             units = "mm",
             dpi = 500))

# Markov Chain -----

n <- network(rgraph(3, tprob = 0.60),
             directed = FALSE) %>% 
  ggnetwork(arrow.gap = 0.2)

dot1 <- c(0.0000000,   1.0000000)
dot2 <- c(0.6000000,   0.8000000)
dot3 <- c(0.9000000,   0.0000000)
anx_sep <- c(0.2, 0.2)
tribble(
         ~x,          ~y,       ~xend,       ~yend,      ~name_to,
     dot1[1],    dot1[2],      dot2[1]-0.12,    dot2[2]-0.03,    "1-2",
     dot2[1],    dot2[2],      dot1[1]+0.12,    dot1[2]+0.03,    "2-1",
     dot3[1],    dot3[2],      dot2[1]+0.09,    dot2[2]-0.1,    "3-2",
     dot2[1],    dot2[2],      dot3[1]-0.12,    dot3[2]+0.05,    "2-3",
     dot2[1],    dot2[2],      dot2[1],    dot2[2]+0.001,    "2-2"
) %>% 
ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_arrow_curve(color = "grey50", size = 2,
             curvature = 0.5,
             arrow_head = arrow_head_wings(offset = 30, 
                                           inset = 50),
             length = 4) +
  geom_arrow_curve(aes(x = dot2[1]+0.02, y = dot2[2], 
                       xend = dot2[1]-0.01, yend = dot2[2]+.15),
                   color = "grey50", size = 2,
                   angle = 90,
                   curvature = 4.5,
                   arrow_head = arrow_head_wings(offset = 30, 
                                                 inset = 50),
                   length = 5) +
  geom_arrow_curve(aes(x = dot3[1]+0.02, y = dot3[2], 
                       xend = dot3[1]-0.01, yend = dot3[2]-.15),
                   color = "grey50", size = 2,
                   curvature = -5,
                   angle = -90,
                   arrow_head = arrow_head_wings(offset = 30, 
                                                 inset = 50),
                   length = 5) +
  geom_point(color = "#041F3F", size = 20) +
  scale_x_continuous(expand = c(0.2, 0.2)) + 
  scale_y_continuous(expand = c(0.2, 0.2)) 

walk(c("png", "svg"),
     ~ggsave(paste0("02_figs/mc-sd.",
                    .),
             bg = "transparent",
             width = 100,                 # Ancho de la gráfica
             height = 100,
             units = "mm",
             dpi = 500))

# Lines -----
n <- 20
subject <- seq(1:n)
time <- 0:10
cat <- rbinom(n, 1, .5)
grid <- data.frame(expand.grid(subject = subject, time= time))
groupIntercept <- 50
sdIntercept <- 10
grouptimeBeta <- 4
sdtimeBeta <- 2
withinSd <- 1

multi <- data.frame(cbind(subject, cat)) %>%
  left_join(., grid, by = 'subject')

multi_subs <- multi %>% 
  group_by(subject) %>%
  summarise(subIntercept = rnorm(1, groupIntercept, sdIntercept), 
            subSlope = rnorm(1, grouptimeBeta, sdtimeBeta))

multi <- left_join(multi, multi_subs)%>% 
  mutate(happy = subIntercept + time*subSlope  + 
           rnorm(nrow(multi), 0, withinSd) + rnorm(nrow(multi), cat*20, 5),
         bin = invlogit((happy-rnorm(nrow(multi), 100, 30))/100),
         boughtIceCream = ifelse(bin > .5, 1, 0),
         cat = as.factor(cat))

ggplot(multi, 
       aes(x = time, y = happy, 
           color = cat)) +
  geom_point(alpha=0.8, size=2.4,
             stroke = 0) +
  geom_line(aes(group = subject),
            linewidth = 1.2,
            alpha = 0.8) +
  scale_x_continuous(breaks = 0:10,
                     expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(1, 0.))) + 
  scale_color_manual(values = c("#7A90A2",
                                "#072D5C"),
                     guide = "none") +
  theme_void() 

walk(c("png", "svg"),
     ~ggsave(paste0("02_figs/lines.",
                    .),
             bg = "transparent",
             width = 100,                 # Ancho de la gráfica
             height = 100,
             units = "mm",
             dpi = 500))

