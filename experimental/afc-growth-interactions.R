


# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

df <- daten %>% 
  filter(Ort != "Flawil")

# Entferne Zeilen mit NA in 'growth' oder 'afc' für die Korrelationsanalyse
data_complete <- df %>%
  filter(!is.na(growth) & !is.na(afc))

#----- 1. Berechne das lineare Modell
model <- lm(growth ~ afc, data = data_complete)

# Extrahiere die Gleichung und den R-squared Wert
equation <- paste("y =", round(coef(model)[2], 2), "x +", round(coef(model)[1], 2))
r_squared <- paste("R^2 =", round(summary(model)$r.squared, 2))

# Erstelle den Scatterplot mit ggplot2
ggplot(data_complete, aes(x = afc, y = growth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + # Füge die Regressionslinie hinzu
  annotate("text", x = max(data_complete$afc) * 0.9, y = max(data_complete$growth) * 0.9, label = equation, hjust = 1) + # Füge die Gleichung hinzu
  annotate("text", x = max(data_complete$afc) * 0.9, y = max(data_complete$growth) * 0.8, label = r_squared, hjust = 1) + # Füge den R-squared Wert hinzu
  labs(title = "Korrelation zwischen afc und growth",
       x = "afc",
       y = "growth") +
  theme_bw() # Optional: Ein schönes Theme


#---- 2. Globales lineares Modell und Einfluss Standort anschauen
library(dplyr)
global_model <- lm(growth ~ afc, data = data_complete )
global_equation <- paste("Global: y =", round(coef(global_model)[2], 2), "x +", round(coef(global_model)[1], 2))
global_r_squared <- paste("Global: R^2 =", round(summary(global_model)$r.squared, 2))

# Erstelle den Scatterplot mit farblicher Unterscheidung nach Standort
ggplot(data_complete, aes(x = afc, y = growth, color = Ort)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = Ort), alpha = 0.5) + # Regressionslinien pro Standort
  annotate("text", x = max(data_complete$afc, na.rm = TRUE) * 0.9, y = max(data_complete$growth, na.rm = TRUE) * 0.95, label = global_equation, hjust = 1) +
  annotate("text", x = max(data_complete$afc, na.rm = TRUE) * 0.9, y = max(data_complete$growth, na.rm = TRUE) * 0.88, label = global_r_squared, hjust = 1) +
  labs(title = "Korrelation zwischen afc und growth nach Standort",
       x = "afc",
       y = "growth",
       color = "Standort") +
  theme_bw()



#----- 3. Einfluss Jahreszeit
# Korrigierte Funktion zur Bestimmung der Jahreszeit mit case_when
get_season <- function(date) {
  month <- as.integer(format(date, "%m"))
  case_when(
    month >= 3 & month <= 5 ~ "spring",
    month >= 6 & month <= 8 ~ "summer",
    month >= 9 & month <= 11 ~ "autumn",
    TRUE ~ "winter" # Für alle anderen Monate
  )
}

# Erstelle eine neue Spalte für die Jahreszeit
data_complete <- data_complete %>%
  mutate(Jahreszeit = get_season(date))

# Scatterplot mit farblicher Unterscheidung nach Jahreszeit
ggplot(data_complete, aes(x = afc, y = growth, color = Jahreszeit)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = Jahreszeit), alpha = 0.5) + # Regressionslinien pro Jahreszeit
  labs(title = "Korrelation zwischen afc und growth nach Jahreszeit",
       x = "afc",
       y = "growth",
       color = "Jahreszeit") +
  theme_bw()



#------ 4. Einfluss Jahreszeit und Standort

# Kombinierte Variable für Standort und Jahreszeit
data_complete <- data_complete %>%
  mutate(Standort_Jahreszeit = paste(Ort, Jahreszeit, sep = "_"))

# Scatterplot mit farblicher Unterscheidung nach Standort und Jahreszeit
ggplot(data_complete, aes(x = afc, y = growth, color = Standort_Jahreszeit)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, aes(group = Standort_Jahreszeit), alpha = 0.3) +
  labs(title = "Korrelation zwischen afc und growth nach Standort und Jahreszeit",
       x = "afc",
       y = "growth",
       color = "Standort_Jahreszeit") +
  theme_bw() +
  theme(legend.text = element_text(size = 8)) # Anpassung der Legenden-Textgröße


# Facetten nach Standort
ggplot(data_complete %>% filter(!is.na(growth) & !is.na(afc)), aes(x = afc, y = growth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Ort) +
  labs(title = "Korrelation zwischen afc und growth nach Standort",
       x = "afc",
       y = "growth") +
  theme_bw()

# Facetten nach Jahreszeit
ggplot(data_complete %>% filter(!is.na(growth) & !is.na(afc)), aes(x = afc, y = growth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ Jahreszeit) +
  labs(title = "Korrelation zwischen afc und growth nach Jahreszeit",
       x = "afc",
       y = "growth") +
  theme_bw()

# Facetten nach Standort und Jahreszeit
ggplot(data_complete %>% filter(!is.na(growth) & !is.na(afc)), aes(x = afc, y = growth)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_grid(Jahreszeit ~ Ort) +
  labs(title = "Korrelation zwischen afc und growth nach Standort und Jahreszeit",
       x = "afc",
       y = "growth") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
        axis.text.y = element_text(size = 6))



# Lineares Modell mit Standort als Faktor
model_standort <- lm(growth ~ afc + Ort, data = data %>% filter(!is.na(growth) & !is.na(afc)))
summary(model_standort)

# Lineares Modell mit Jahreszeit als Faktor
model_jahreszeit <- lm(growth ~ afc + Jahreszeit, data = data %>% filter(!is.na(growth) & !is.na(afc)))
summary(model_jahreszeit)

# Lineares Modell mit Interaktion zwischen afc und Standort
model_interaktion_standort <- lm(growth ~ afc * Ort, data = data %>% filter(!is.na(growth) & !is.na(afc)))
summary(model_interaktion_standort)

# Lineares Modell mit Interaktion zwischen afc und Jahreszeit
model_interaktion_jahreszeit <- lm(growth ~ afc * Jahreszeit, data = data %>% filter(!is.na(growth) & !is.na(afc)))
summary(model_interaktion_jahreszeit)



#----- alte Idee
library(ggpubr)
afc_plot <- ggplot(df, aes(x = afc, y = growth)) +
  #geom_point(aes(shape=Ort), size=2) + 
  geom_point(size=1,  show.legend = TRUE, aes(color = Ort)) +
  #stat_summary(fun = "mean", geom = "line") +
  geom_smooth(method = 'lm', se = FALSE, formula = y ~ x) +
  xlim(0, 2000) +
  #stat_smooth(method='lm', formula = y~poly(x,2)) +
  #stat_cor(label.x = 30, label.y = 130, size = 4) +
  #stat_regline_equation(label.x = 30, label.y = 150, size = 4)+
  labs(x = "AFC Average Farm Cover (kg TS/ha)", y = "Graswachstum (kg TS/ha/Tag)",
       title = "Graswachstum in Abhängigkeit des Grasvorrates ") +
  theme_minimal() +
  theme(legend.position = "right")

afc_plot
df$x <- df$afc
df$y <- df$growth

afc_lm <-  lm(growth ~ Ort + afc, df)
summary(afc_lm)$r.squared


lm_eqn <- function(df){
  m <- lm(y ~ x  , df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
#df %>% filter(Ort == "Sorens") %>% lm_eqn()

afc_plot_2 <- afc_plot + geom_text(x = 600, y = 125, label = lm_eqn(df), parse = TRUE)

afc_plot_2


# ggplotly(afc_plot, )
# 
# ggplotly(afc_plot, tooltip = "text") %>% 
#   highlight(on = 'plotly_click', off = 'plotly_doubleclick', 
#             opacityDim = .05)
# afcfile <- paste("outputs/AFC_", Jahr, ".svg", sep="")
# ggsave(file=afcfile, width=10, height=7.5)
# 

# 
# plot(df$afc, df$growth, col=df$Ort)
# 
# interaction.plot(
#   x.factor = df$afc,
#   response = df$growth,
# )
