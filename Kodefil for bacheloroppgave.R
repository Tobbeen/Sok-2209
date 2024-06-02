rm(list=ls())
Sys.setlocale("LC_ALL", "no_NO.UTF-8")

library(ggplot2)
library(tidyverse)
library(car)
library(dplyr)
library(vtable)
library(stats)
library(sjPlot)
library(sjmisc)
library(utf8)
library(effects)
library(broom)
library(ggeffects)
library(ggrepel)


# Leser inn datasettet
data <- read.csv("C:/Users/Tobben/Downloads/2298/Norsk medborgerpanel runde 26, 2023/NSD3127.csv", encoding = "UTF-8")
#bytter navn til tillit for tillitsvariabel.
data$tillit <- data$r26_pccoo

#filtrerer ut observasjoner med 97&98
dataset <- subset(data, tillit != 97 & tillit != 98)

#Lager dummyvariabel for kjønn
dataset$mann <- ifelse(dataset$r26P1 == 1, 1, 0)

#Oppretter kategoriske variabler for aldersgruppene
dataset <- dataset %>%
  mutate(født = case_when(
    r26P5_2 == 1 ~ '1959_og_før',
    r26P5_2 == 2 ~ '1960_1989',
    TRUE ~ '1990_og_etter'
  )) %>%
  mutate(født = factor(født, levels = c('1959_og_før', '1960_1989', '1990_og_etter')))

# Hær bruker vi r26P4_1 variabel i datasettet for å kategorisere utdanningsnivåer inn til tre kategorier.
dataset <- dataset %>%
  mutate(utdanning = factor(r26P4_1, 
                            levels = c(1, 2, 3),
                            labels = c("grunnskole", "videregående", "høyere"),
                            exclude = 97)) %>%
  mutate(utdanning = fct_relevel(utdanning, "grunnskole", "videregående", "høyere"))

#Inkluderer endrer navn på inntektsvariabler, og gir de ordinale verdier.
dataset <- dataset %>%
  mutate(inntekt = factor(r26k2_bginc, 
                          levels = c(1, 2, 3, 4, 5, 6, 7, 8),
                          labels = c("under 150k", "150k-300k", "300k-400k", "400k-500k", "500-600k", "600k-700k", "700k-1m", "mer enn 1m"),
                          exclude = 97)) %>%
  mutate(inntekt = fct_relevel(inntekt, "under 150k"))

#Drar alle ønskede variabler inn i et eget datasett.
df <- dataset %>%
  select(tillit, mann, født, utdanning, inntekt)


#Gir kjønn navn
df$kjønn <- ifelse(df$mann == 1, "Mann", "Kvinne")

# Fjerner NA's
df1 <- df %>% 
  filter(!is.na(utdanning))

#genererer gjennomsnitt funksjon, og ignorer na
mean_fun <- function(x) {
  return(mean(x, na.rm = TRUE))
}



## deskriptiv statistikk 
df1 %>%
  ggplot(aes(x = utdanning, y = tillit, fill = kjønn)) +
  geom_boxplot() +
  labs(title = "Sosial tillit basert på utdanningsgrad",
       x = "Utdanningsgrad",
       y = "Sosial tillit") +
  theme_minimal()

## Utfører regresjon av alle fire modeller.

model1 <- lm(tillit ~ utdanning + mann + født + inntekt, data = df1)
model2 <- lm(tillit ~ utdanning, data = df1)
model3 <- lm(tillit ~ utdanning + mann + født, data = df1)
model4 <- lm(tillit ~ utdanning +inntekt, data = df1)
summary(model1)

plot(model1, which = 1, main = "Model fit")

plot(model1, which = 2, main = "Model fit")

plot(model1, which = 3, main = "Model fit")

plot(model1, which = 5, main = "Model fit")


## Legger alle fire modeller til i en tabell.

tab_model(model2, model3,model4,model1, encoding = "UTF-8")

## Interaksjon mellom alder og utdanning?
df1 %>%
  ggplot(aes(x = utdanning, y = tillit, group = født, color = født)) +
  geom_line(stat = "summary", fun = mean) +
  geom_point(stat = "summary", fun = mean, color= "black", size =1.5) +
  geom_text(stat = "summary", aes(label = round(..y.., 2)), 
            vjust = -.7, size = 3) +
  labs(title = "Mulig interaksjon mellom alder og utdanning",
       x = "Utdanningsnivå",
       y = "Tillit") +
  theme_minimal()


## Graf for utdanningsgrad

predict1 <- ggpredict(model1, terms = c("utdanning"))

predict1$x <- fct_reorder(predict1$x, predict1$predicted)

ggplot(predict1, aes(x, predicted)) +
  geom_point(aes(color = x), size = 2, alpha = 1) +
  geom_errorbar(aes(ymin=predicted-std.error, ymax=predicted+std.error), width=.05) +
  geom_line(aes(x=x, y=predicted, group = 1), linetype = "solid") +
  scale_color_manual(values=c("blue", "red", "orange")) +
  geom_text_repel(aes(label = round(predicted, 2)), size = 3, nudge_x = 0.1) +
  labs(title = "Forventet verdi tillit for hver utdanninsgrad",
       subtitle = "Verdier hentet fra regresjon",
       x = "Utdanningsgrad",
       y = "Tillit",
       color = "Utdanningsgrad") +
  theme_light()

## Graf for aldersgrupper
predict2 <- ggpredict(model1, terms = c("født"))

predict2$x <- fct_reorder(predict2$x, predict2$predicted)

ggplot(predict2, aes(x, predicted)) +
  geom_point(aes(color = x), size = 2, alpha = 1) +
  geom_errorbar(aes(ymin=predicted-std.error, ymax=predicted+std.error), width=.05) +
  geom_line(aes(x=x, y=predicted, group = 1), linetype = "solid") +
  scale_color_manual(values=c("blue", "red", "orange")) +
  geom_text_repel(aes(label = round(predicted, 2)), size = 3, nudge_x = 0.1) +
  labs(title = "Forventet verdi tillit for hver aldersgruppe",
       subtitle = "Verdier hentet fra regresjon",
       x = "Aldersgruppe",
       y = "Tillit",
       color = "Aldersgrupper")+
  theme_bw()


#-----------------------
## Graf for utdanning og alder (ikke brukt)
predict3 <- ggpredict(model1, terms = c("utdanning", "født"))

ggplot(predict3, aes(x = x, y = predicted, color = group)) +
  geom_point(size = 3.5, alpha = 0.8) +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error), width = 0.1) +
  geom_line(aes(group = group), linetype = "dashed") +
  geom_text_repel(aes(label = round(predicted, 2)), size = 3, nudge_x = 0.2, color = "black") +
  labs(title = "Forventet tillit basert på utdanning og alder",
       subtitle = "Hentet fra regresjon",
       x = "Utdanningsgrad",
       y = "Forventet tillit",
       color = "Aldersgruppe") +
  theme_light()



#-------
## Graf for utdanning og kjønn (ikke brukt)
df1 <- na.omit(df1)

df1$predicted <- predict(model1, newdata = df1)

#----------------------------------

predict4 <- ggpredict(model1, terms = c("utdanning", "mann"))

ggplot(predict4, aes(x = x, y = predicted, color = group)) +
  geom_point(size = 3.5, alpha = 0.8) +
  geom_errorbar(aes(ymin = predicted - std.error, ymax = predicted + std.error), width = 0.1) +
  geom_line(aes(group = group), linetype = "dashed") +
  geom_text_repel(aes(label = round(predicted, 2)), size = 3, nudge_x = 0.2, color = "black") +
  labs(title = "Forventet verdi for tillit basert på utdanning og kjønn",
       subtitle = "Hentet fra regresjon",
       x = "Utdanningsgrad",
       y = "Forventet tillit",
       color = "kjønn") +
  theme_light()


## Test for å se om det er statistisk signifkant forskjell fra 0 mellom aldersgruppene ved grunnskoleutdanning

alderstest <- df1 %>%
  mutate(født = factor(født, levels = c('1990_og_etter', '1960-1989','1959_og_før')))

alderstest <- df1 %>%
  mutate(utdanning = factor(utdanning, levels = c('grunnskole','videregående','høyere')))

modell5 <- lm(tillit ~ født * utdanning, data = alderstest)
summary(modell5)
tab_model(modell5)


