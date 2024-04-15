##################################################################
##                     Project: Logo plots                      ##
##################################################################
##
## Description:    This script generates the plots used to create 
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
               ggarrow, ggformula)

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
             width = 100,
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
             width = 100,
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
             width = 100,
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
             width = 100,
             height = 100,
             units = "mm",
             dpi = 500))

## V5----
tibble(x = 1:19,
       m0 = c(3.5, 4, NA, 3, NA, 4, 3, NA, NA, 6,
              NA, NA, 3, 4, NA, 3, NA, 4, 3.5) + 2,
       m1 = m0 - 0.85,
       m2 = m0 -2,
       m3 = c(3.7, 4, 3.5, NA, 2.2,  NA, NA, 1.8, NA,
              NA, NA, NA, 0, NA, NA, NA, NA, NA, NA)*1.3-0.2,
       m4 = c(0, NA, NA, NA, 0, NA, 0.2, NA, 1, NA, 1.5, 
              NA, 1.4, NA, 3, NA, 3.3, NA, 4)*1.3-0.1) %>% 
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
             width = 100,
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
             width = 100,
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
             width = 100,
             height = 100,
             units = "mm",
             dpi = 500))

# Lines -----
## V1 -----
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
             width = 100,
             height = 100,
             units = "mm",
             dpi = 500))

## V2 -----

ggplot(multi, 
       aes(x = time, y = happy, 
           color = cat)) +
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
             width = 100, 
             height = 100,
             units = "mm",
             dpi = 500))


## V2 -----
### Functions needed ----
#### Nadaraya–Watson regression

nw_r <- function(Y_data, X_data, h = 0.05, K = dnorm, ci = NULL, alpha = 0.05) {
  
  Y <- as.vector(Y_data)
  X <- as.vector(X_data)
  
  fn <- function(x) {
    
    Kx <- sapply(X, function(Xi) K((x - Xi) / h) / h)
    W <- Kx / rowSums(Kx) 
    r_x <- as.vector(W %*% Y)
    
    a <-  W %*% Y
    
    if(!is.null(ci)) {
      
      sigma_2 <- sum((lead(Y)-Y)^2,
                     na.rm = T) / (2*(length(Y) - 1))
      
      m <- (max(X) - min(X))/h
      
      q <- qnorm((1+(1-alpha)^(1/m))/2)
      
      se <- sqrt(sigma_2)*sqrt(rowSums(W^2))
      
      
      if(!(ci %in% c("Upper","Lower"))) {
        stop('`ci` should be "Upper" or "Lower"')
      }
      
      add <- q*se
      
      if(ci == "Lower") r_x <- r_x - add
      
      if(ci == "Upper") r_x <- r_x + add
      
    }
    
    return(r_x)
    
  }
  
  return(fn)  
}

#### Cross validation

c_v <- function(Y, X, h, K = dnorm) {
  
  values <- sapply(h,
                   function(h_v){
                     
                     fun <- nw_r(Y, X, h_v, K = K)
                     
                     denomin <- (1 - K(0) / 
                                   colSums(K(
                                     sapply(X, function(x){X - x})
                                     / h_v)))
                     
                     sum(((Y - fun(x = X)) /
                            denomin)^2,
                         na.rm = T)
                   })
  
  return(values)
}

## Load data ----

waves <- read.table("https://lambda.gsfc.nasa.gov/data/map/dr5/dcp/spectra/wmap_tt_spectrum_9yr_v5.txt", 
                    header=TRUE, skip=4) %>% 
  clean_names() %>% 
  tibble() %>% 
  transmute(ell = x2,
            cl = x150_6398,
            id = 1) %>% 
  filter(ell < 1200,
         ell > 200)
waves %>% 
  ggplot(aes(x = ell)) +
  geom_point(aes(y = cl),
             alpha = 0.5) 



Y <- waves$cl
X <- waves$ell

#### Estimate h^*

range <- range(X)

cv_df <- tibble(h = seq(2, range[2]- range[1], l = 500),
                est_risk = c_v(Y, X, h = h))

h_select <- cv_df %>% 
  filter(est_risk == min(est_risk))
h_select
h_opt <- h_select$h-10

cv_df %>% 
  ggplot() +
  geom_line(aes(h, est_risk)) +
  geom_vline(xintercept = h_opt)

## Plot -----
data <- tibble(ell = seq(min(X), max(X), length.out = 1000),
               nw_r = nw_r(Y, X, h = h_opt)(ell),
               nw_r_upper = nw_r(Y, X, h = h_opt, ci = "Upper",
                                 alpha = 0.001)(ell),
               nw_r_lower = nw_r(Y, X, h = h_opt, ci = "Lower",
                                 alpha = 0.001)(ell),
)


waves %>% 
  ggplot(aes(x = ell)) +
  # geom_point(aes(y = cl),
  #            alpha = 0.5) +
  stat_function(data = data,
                fun = nw_r(Y, X, h = h_opt),
                n = 1000,
                color="#041F3F", 
                size=2) +
  geom_ribbon(data = data,
              aes(ymin = nw_r_lower,
                  ymax = nw_r_upper),
              fill = "#041F3F",
              alpha = 0.2) +
  geom_smooth(aes(ell, cl),
              se=F,
              size=1.3,
              span=0.02,
              color="#072D5C",
              linejoin = 'mitre') +
  geom_smooth(aes(ell, cl),
              se=F,
              size=2,
              span=0.7,
              color="#7A90A2",
              linejoin = 'mitre') +
  scale_x_continuous(breaks = 0:10,
                     expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(mult = c(0.5, 0.2))) 

walk(c("png", "svg"),
     ~ggsave(paste0("02_figs/lines-v2.",
                    .),
             bg = "transparent",
             width = 100,        
             height = 100,
             units = "mm",
             dpi = 500))


# Robust rg 
ggplot() +
  xlim(-1, 1) +
  ylim(0, 4) +
  geom_function(fun = ~log(1+.x^2), colour = "#072D5C", linewidth = 4,
                alpha = 0.7) +
  geom_function(fun = ~.x^2, colour = "#072D5C", linewidth = 4,
                alpha = 0.7) +
  geom_function(fun = ~abs(.x), colour = "#072D5C", linewidth = 4,
                alpha = 0.7) +
  theme_void()

walk(c("png", "svg"),
     ~ggsave(paste0("02_figs/robust.",
                    .),
             bg = "transparent",
             width = 100, 
             height = 100,
             units = "mm",
             dpi = 500))


