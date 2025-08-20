# Setup ----
rm(list=ls())
library(tidyverse)
library(ggpubr)
library(boot)
library(showtext) # Font

base_path <- "E:/Datos"

font_add_google("Montserrat", "Montserrat")
showtext_auto() 

my_theme <- theme_bw() +
  theme(
    panel.grid.major = element_line(colour = "#f0f0f0"),
    panel.grid.minor = element_blank(), # panel.border = element_blank()
    axis.line = element_line(),
    text = element_text(size = 25, family="Montserrat"),
    plot.title = element_text(face="bold"), 
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(hjust = 0, face = "italic")
  )

# Import ----

file <- paste0(base_path, "/Resultados/indices_metpy.csv")
indices_metpy <- read.csv(file=file)

indices_metpy_m <- indices_metpy %>% 
  mutate(date = as.Date(paste0(year, "-", month, "-", day)))

# 0.  Management ----

# 0.1. Drop obs. with NA in TT and KI
#indices_metpy_m <- indices_metpy_m %>% 
#  filter(!is.na(TT) & !is.na(KI))
  
# 0.2. Drop duplicates (00 or 12), based on which has more observations in a year
indices_metpy_m <- indices_metpy_m %>% 
  group_by(year) %>% 
  mutate(num_0 = sum(hour == 0),        
         num_12 = sum(hour == 12)) %>%
  mutate(more = if_else(num_0 >= num_12, 0, 12)) %>% 
  filter(more == hour) %>% 
  select(!c(num_0,num_12,more)) %>% 
  ungroup() %>% 
  as.data.frame()

# 0.3. Create Modified SI and LI (MSI, MSLI)
indices_metpy_m <- indices_metpy_m %>% 
  mutate(LI=-1*LI,
         SI=-1*SI) %>% 
  rename(MLI=LI,
         MSI=SI)

# 0.4. El Niño (Fuerte (f), Moderado (m) y Extrarodinario (e)) and 
# La Niña (Fuerte (f) y Moderado (m)) events

nino_76    <- expr(date >= as.Date("1976-05-01") & date <= as.Date("1976-11-30"))
nino_82_83 <- expr(date >= as.Date("1982-07-01") & date <= as.Date("1983-11-30"))
nino_86_87 <- expr(date >= as.Date("1986-12-01") & date <= as.Date("1987-12-31"))
nino_91_92 <- expr(date >= as.Date("1991-07-01") & date <= as.Date("1992-06-30"))
nino_93    <- expr(date >= as.Date("1993-03-01") & date <= as.Date("1993-09-30"))
nino_97_98 <- expr(date >= as.Date("1997-03-01") & date <= as.Date("1998-09-30"))
nino_15_16 <- expr(date >= as.Date("2015-04-01") & date <= as.Date("2016-07-31"))
nino_16_17 <- expr(date >= as.Date("2016-12-01") & date <= as.Date("2017-05-31"))

nina_73_74 <- expr(date >= as.Date("1973-05-01") & date <= as.Date("1974-02-28"))
nina_74_75 <- expr(date >= as.Date("1974-11-01") & date <= as.Date("1975-01-31"))
nina_75_76 <- expr(date >= as.Date("1975-06-01") & date <= as.Date("1976-01-31"))
nina_85    <- expr(date >= as.Date("1985-03-01") & date <= as.Date("1985-09-30"))
nina_88    <- expr(date >= as.Date("1988-05-01") & date <= as.Date("1988-11-30"))
nina_96    <- expr(date >= as.Date("1996-04-01") & date <= as.Date("1996-07-31"))
nina_07    <- expr(date >= as.Date("2007-04-01") & date <= as.Date("2007-12-31"))
nina_10    <- expr(date >= as.Date("2010-08-01") & date <= as.Date("2010-11-30"))
nina_13    <- expr(date >= as.Date("2013-04-01") & date <= as.Date("2013-08-31"))

indices_metpy_m <- indices_metpy_m %>% 
  mutate(nino = case_when(eval(nino_76) ~ "m",
                          eval(nino_82_83) ~ "e",
                          eval(nino_86_87) ~ "m",
                          eval(nino_91_92) ~ "m",
                          eval(nino_93) ~ "m",
                          eval(nino_97_98) ~ "e",
                          eval(nino_15_16) ~ "f",
                          eval(nino_16_17) ~ "m"),
         nina = case_when(eval(nina_73_74) ~ "m",
                          eval(nina_74_75) ~ "m",
                          eval(nina_75_76) ~ "f",
                          eval(nina_85) ~ "m",
                          eval(nina_88) ~ "m",
                          eval(nina_96) ~ "m",
                          eval(nina_07) ~ "m",
                          eval(nina_10) ~ "m",
                          eval(nina_13) ~ "f")
         ) %>% 
  mutate(event = case_when(!is.na(nino) ~ "niño",
                           !is.na(nina) ~ "niña")) 

# 0.5. Filter only periods with El Niño or La Niña
indices_metpy_m <- indices_metpy_m %>% 
  filter(!is.na(event)) %>% 
  mutate(event =  as.factor(event))

# 1. Descriptive analysis ----

# 1.1. Total usable observations per year and 00 UTC y 12 UTC obs per year
des_data <- indices_metpy_m %>%  
  group_by(year) %>%
  summarise(
    num = n(),                     
    num_0 = sum(hour == 0),        
    num_12 = sum(hour == 12)       
  ) %>% 
  ungroup()

# 1.2. Correlation
indices <- c("TT", "KI", "MLI", "MSI", "GDI")

cor(indices_metpy_m[,indices], use = "complete.obs", method = "pearson")

# 1.3. Mean of stability indices during El Niño and La Niña
mean_nino <- indices_metpy_m %>%
  filter(!is.na(nino)) %>%
  group_by(nino) %>% 
  summarise(mean_TT = mean(TT, na.rm = TRUE),
            mean_KI = mean(KI, na.rm = TRUE),
            mean_MLI = mean(MLI, na.rm = TRUE),
            mean_MSI = mean(MSI, na.rm = TRUE),
            mean_GDI = mean(GDI, na.rm = TRUE)) %>% 
  ungroup()

mean_nina <- indices_metpy_m %>%
  filter(!is.na(nina)) %>%
  group_by(nina) %>%
  summarise(mean_TT = mean(TT, na.rm = TRUE),
            mean_KI = mean(KI, na.rm = TRUE),
            mean_MLI = mean(MLI, na.rm = TRUE),
            mean_MSI = mean(MSI, na.rm = TRUE),
            mean_GDI = mean(GDI, na.rm = TRUE)) %>% 
  ungroup()

mean_event <- indices_metpy_m %>%
  group_by(event) %>%
  summarise(mean_TT = mean(TT, na.rm = TRUE),
            mean_KI = mean(KI, na.rm = TRUE),
            mean_MLI = mean(MLI, na.rm = TRUE),
            mean_MSI = mean(MSI, na.rm = TRUE),
            mean_GDI = mean(GDI, na.rm = TRUE)) %>% 
  ungroup()

# 1.4. Normality test
norm_test <- data.frame(index = indices,
                        p_nino = NA,
                        p_nina = NA)
i <- 1
for (index in indices){
  a <- shapiro.test(indices_metpy_m[,index][indices_metpy_m$event=="niño"])
  b <- shapiro.test(indices_metpy_m[,index][indices_metpy_m$event=="niña"])
  
  norm_test$p_nino[i] <- a$p.value
  norm_test$p_nina[i] <- b$p.value
  i <- i + 1
}

# 2. Graphs ----

# 2.1. Time series - examples ----

# Niño 1982-1983
p_nino_82_83 <- indices_metpy_m %>%
  filter(eval(nino_82_83)) %>% 
  ggplot(aes(date, KI)) +
  geom_line(color="#00BFC4") +
  scale_x_date(date_breaks = "2 month", date_labels = "%b-%y") +
  labs(x = "Fecha", y = "Índice KI", 
       title = "El Niño 1982-1983") +
  my_theme 

# Niña 1975-1976
p_nina_75_76 <- indices_metpy_m %>%
  filter(eval(nina_75_76)) %>% 
  ggplot(aes(date, KI)) +
  geom_line(color="#F8766D") +
  scale_y_continuous(n.breaks = 4, limits = function(x){
    c(min(x), max(x)+4)
  }) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
  labs(x = "Fecha", y = "Índice KI", 
       title = "La Niña 1975-1976") +
  my_theme

examples <- ggarrange(p_nino_82_83, p_nina_75_76, 
          labels = c("a)", "b)"),
          ncol = 1,
          font.label = list(size = 25))

ggsave("E:/Graficos/examples.png", 
       plot = examples,
       width = 6, 
       height = 6, 
       units = "in", 
       dpi = 220)

# 2.2. Histograms -----
p_TT <- indices_metpy_m %>%
  ggplot(aes(TT, fill= event)) + 
  geom_density(alpha = 0.5) +
  geom_vline(data = mean_event,
             aes(xintercept = mean_TT, color = event),
             linetype = "dashed",
             linewidth = 0.8) +
  labs(title = "", y = "", color = "", fill = "") +
  scale_fill_discrete(labels = c("La Niña", "El Niño")) +
  scale_color_discrete(labels = c("La Niña", "El Niño")) +
  my_theme

p_KI <- indices_metpy_m %>%
  ggplot(aes(KI, fill= event)) + 
  geom_density(alpha = 0.5) +
  geom_vline(data = mean_event,
             aes(xintercept = mean_KI, color = event),
             linetype = "dashed",
             linewidth = 0.8) +
  labs(title = "", y = "", color = "", fill = "") +
  scale_fill_discrete(labels = c("La Niña", "El Niño")) +
  scale_color_discrete(labels = c("La Niña", "El Niño")) +
  my_theme

p_MLI <- indices_metpy_m %>%
  ggplot(aes(MLI, fill= event)) + 
  geom_density(alpha = 0.5) +
  geom_vline(data = mean_event,
             aes(xintercept = mean_MLI, color = event),
             linetype = "dashed",
             linewidth = 0.8) +
  labs(title = "", y = "", color = "", fill = "") +
  scale_fill_discrete(labels = c("La Niña", "El Niño")) +
  scale_color_discrete(labels = c("La Niña", "El Niño")) +
  my_theme

p_MSI <- indices_metpy_m %>%
  ggplot(aes(MSI, fill= event)) + 
  geom_density(alpha = 0.5) +
  geom_vline(data = mean_event,
             aes(xintercept = mean_MSI, color = event),
             linetype = "dashed",
             linewidth = 0.8) +
  labs(title = "", y = "", color = "", fill = "") +
  scale_fill_discrete(labels = c("La Niña", "El Niño")) +
  scale_color_discrete(labels = c("La Niña", "El Niño")) +
  my_theme

p_GDI <- indices_metpy_m %>%
  ggplot(aes(GDI, fill= event)) + 
  geom_density(alpha = 0.5) +
  geom_vline(data = mean_event,
             aes(xintercept = mean_GDI, color = event),
             linetype = "dashed",
             linewidth = 0.8) +
  labs(title = "", y = "", color = "", fill = "") +
  scale_fill_discrete(labels = c("La Niña", "El Niño")) +
  scale_color_discrete(labels = c("La Niña", "El Niño")) +
  my_theme

density <- ggarrange(p_TT, p_KI, p_MLI,p_MSI, p_GDI,
          labels = c("a)", "b)", "c)", "d)", "e)"),
          ncol = 2, nrow=3, 
          common.legend = TRUE, 
          legend = "bottom",
          font.label = list(size = 25))

ggsave("E:/Graficos/density.png", 
       plot = density,
       width = 6, 
       height = 6, 
       units = "in", 
       dpi = 200)

# 3. Hypothesis test ----
indices <- c("TT", "KI", "MLI", "MSI", "GDI")

# 3.1. Bootstrap difference in medians and means
ic_values <- data.frame(index = indices,
                        lower_ci_md = NA,
                        upper_ci_md = NA,
                        diff_estimate_md = NA,
                        lower_ci_m = NA,
                        upper_ci_m = NA,
                        diff_estimate_m = NA)
i <- 1
for (index in indices) {
  
  # Median diff.
  md.diff <- function(d, i) {
    tmp <- d[i,] 
    median(tmp[,index][tmp$event=="niño"], 
           na.rm = TRUE) - 
      median(tmp[,index][tmp$event=="niña"], 
             na.rm = TRUE)
  }
  
  set.seed(123)
  boot.out <- boot(data = indices_metpy_m, 
                   statistic = md.diff,
                   strata = indices_metpy_m$event,
                   R = 2000)
  
  ci.boot <- boot.ci(boot.out, type = "bca", conf = 0.95)
  
  ic_values$lower_ci_md[i] <- ci.boot$bca[,4]
  ic_values$upper_ci_md[i] <- ci.boot$bca[,5]
  ic_values$diff_estimate_md[i] <- ci.boot$t0

  # Mean diff.
  m.diff <- function(d, i) {
    tmp <- d[i,] 
    mean(tmp[,index][tmp$event=="niño"],
         na.rm = TRUE) - 
      mean(tmp[,index][tmp$event=="niña"],
           na.rm = TRUE)
  }
  
  set.seed(123)
  boot.out <- boot(data = indices_metpy_m, 
                   statistic = m.diff,
                   strata = indices_metpy_m$event,
                   R = 2000)
  
  ci.boot <- boot.ci(boot.out, type = "bca", conf = 0.95)
  
  ic_values$lower_ci_m[i] <- ci.boot$bca[,4]
  ic_values$upper_ci_m[i] <- ci.boot$bca[,5]
  ic_values$diff_estimate_m[i] <- ci.boot$t0
  
  i <- i + 1
}

# 3.2. CI PLOT
ic_values <- ic_values %>% 
  mutate(index  = factor(index, 
                         levels = c("GDI", "MSI", "MLI", "KI", "TT"))) 

# Median
p_md <- ggplot(ic_values, aes(x = diff_estimate_md, y = index)) +  
  geom_point(colour = "#717171") +
  geom_errorbarh(aes(xmin = lower_ci_md, xmax = upper_ci_md), 
                 height = 0.2, 
                 colour = "#717171", 
                 linewidth = 0.7) +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "red", 
             linewidth = 1) +
  labs(x = "Diferencia de Medianas", y = "") +
  my_theme

# Mean
p_m <- ggplot(ic_values, aes(x = diff_estimate_m, y = index)) +  
  geom_point(colour = "#717171") +
  geom_errorbarh(aes(xmin = lower_ci_m, xmax = upper_ci_m), 
                 height = 0.2, 
                 colour = "#717171", 
                 linewidth = 0.7) +
  geom_vline(xintercept = 0, 
             linetype = "dashed", 
             color = "red", 
             linewidth = 1) +
  labs(x = "Diferencia de Medias", y = "") +
  my_theme

ci <- ggarrange(p_m, p_md,
          labels = c("a)", "b)"),
          ncol = 2, 
          nrow=1,
          font.label = list(size = 25))

ggsave("E:/Graficos/ci.png", 
       plot = ci,
       width = 6, 
       height = 4, 
       units = "in", 
       dpi = 200)

