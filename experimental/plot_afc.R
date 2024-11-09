library(tidyr)
  str(daten)
df <- tibble(daten)
df |> select(Ort,date,growth,afc,weeknum) -> df
df |> drop_na() -> df
df |> filter(Ort != "Landquart") -> df
#df |> filter(Ort != "Benken") -> df
#df |> filter(Ort != "Muri AG") -> df




afc_curve <- ggplot(df, aes(x = weeknum, y = afc, color = Ort)) +
  #geom_point(aes(shape=Ort), size=2) + 
  geom_point(size=1,  show.legend = TRUE) +
  geom_line(linewidth = 0.5) +
  #stat_summary(fun = "mean", geom = "line") +
  labs(x = "Kalenderwoche", y = "AFC Average Farm Cover (kg TS/ha)",
       title = "Average Farm Cover 2024") +
  #scale_colour_manual(name = "",
  #                    values = "Dodger Blue 3",
  #                   labels = "c") +
  #guides(color = "none", fill = "none") +
  xlim(0,52) +
  #geom_line(aes(y = durchschnitt), color = "black", size = 2, linetype = "dotted") +
  theme_minimal() +
  theme(legend.position = "right")

afc_curve
ggplotly(afc_curve)
ggplotly(afc_curve, tooltip = "text") %>% 
  highlight(on = 'plotly_click', off = 'plotly_doubleclick', 
            opacityDim = .05)

afcfile <- paste("outputs/AFC_", Jahr, ".svg", sep="")
ggsave(file=afcfile, width=10, height=7.5)


# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA



library(ggpubr)
afc_plot <- ggplot(df, aes(x = afc, y = growth)) +
  #geom_point(aes(shape=Ort), size=2) + 
  geom_point(size=1,  show.legend = TRUE, aes(color = Ort, shape= Ort)) +
  #stat_summary(fun = "mean", geom = "line") +
  geom_smooth(method = 'lm', se = FALSE, formula = y ~ x) +
  #stat_smooth(method='lm', formula = y~poly(x,2)) +
  #stat_cor(label.x = 30, label.y = 130, size = 4) +
  #stat_regline_equation(label.x = 30, label.y = 150, size = 4)+
  labs(x = "AFC Average Farm Cover (kg TS/ha)", y = "Graswachstum (kg TS/ha/Tag)",
       title = "Graswachstumskurven 2024") +
  theme_minimal() +
  theme(legend.position = "right")

afc_plot
df$x <- df$afc
df$y <- df$growth

afc_lm <-  lm(growth ~ Ort + afc, df)
summary(afc_lm)$r.squared


lm_eqn <- function(df){
  m <- lm(y ~ x , df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}
df %>% filter(Ort == "Sorens") %>% lm_eqn()

afc_plot_2 <- afc_plot + geom_text(x = 600, y = 125, label = lm_eqn(df), parse = TRUE)

afc_plot_2
ggplotly(afc_plot, )

ggplotly(afc_plot, tooltip = "text") %>% 
  highlight(on = 'plotly_click', off = 'plotly_doubleclick', 
            opacityDim = .05)
afcfile <- paste("outputs/AFC_", Jahr, ".svg", sep="")
ggsave(file=afcfile, width=10, height=7.5)



plot(df$afc, df$growth, col=df$Ort)

interaction.plot(
  x.factor = df$afc,
  response = df$growth,
)



