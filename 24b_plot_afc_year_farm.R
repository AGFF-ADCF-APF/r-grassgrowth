
library(plotly)
library(dplyr)
#library(viridis)



average_growth <- jahresdaten %>%
  group_by(weeknum) %>%
  summarise(mean_growth = mean(growth, na.rm = TRUE))

average_afc <- jahresdaten %>%
  group_by(weeknum) %>%
  summarise(mean_afc = mean(afc, na.rm = TRUE))





# Erstelle den interaktiven Plot
fig <- plot_ly(    hovertemplate = paste('Wachstum %{y:.0f} kg TS/ha/Tag' )
  ) %>%
  add_trace(
    data = jahresdaten,
    x = ~weeknum,
    y = ~growth,
    color = ~Ort,
    type = 'scatter',
    mode = 'lines+markers',
    line = list(width = 2),
    marker = list(size = 5),
    connectgaps = T,
    name = ~Ort,
    hovertemplate = paste('Wachstum %{y:.0f} kg TS/ha/Tag' )
  ) %>% 
  add_trace(
    data = average_growth,
    x = ~weeknum,
    y = ~mean_growth,
    type = 'scatter',
    mode = 'lines',
    connectgaps = T,
    line = list(color = 'black', dash = 'dashdot', width = 5),
    name = 'Mittleres Wachstum  (dieses Jahr)'
  ) %>%
  add_trace(
    data = standardkurven,
    x = ~weeknum,
    y = ~Durchschnitt...700.m.ü.M...tiefgründig..frisch,
    type = 'scatter',
    mode = 'lines',
    line = list(color = 'red', dash = 'dot', width = 3),
    name = 'Durchschnitt Mittelland'
  ) %>%
  add_trace(
    data = jahresdaten,
    x = ~weeknum,
    y = ~afc,
    color = ~Ort,
    type = 'scatter',
    mode = 'markers',
    name = ~Ort,
    marker = list(symbol = 'diamond-wide', line = list(width=2, color = 'black'), size=10),
    yaxis = 'y2', 
    hovertemplate = paste('AFC %{y:.0f} kg TS/ha' ),
    showlegend = T, 
    visible = F
  ) %>% 
  add_trace(
    data = average_afc,
    x = ~weeknum,
    y = ~mean_afc,
    type = 'scatter',
    mode = 'markers',
    marker = list(symbol = 'diamond-wide', line = list(width=2, color = 'black'), size=10),
    yaxis = 'y2', 
    name = 'Mittleres AFC (dieses Jahr)', 
    hovertemplate = paste('AFC %{y:.0f} kg TS/ha' )
  )

fig
# Layout anpassen
fig <- fig %>%
  layout(
    title = 'Graswachstum und AFC je Standort',
    xaxis = list(title = "Kalenderwoche"),
    yaxis = list(title = "Graswachstum [kg TS/ha/Tag] ", 
                 range = c(0,200)),
    legend = list(title = list(text = "Standort")),
    hovermode = "x unified",
    yaxis2 = list(
      range=c(0,2000),
      title =  "AFC available [kg TS/ha]",
      overlaying = 'y',
      side = 'right',
      showgrid = FALSE
    ),
    updatemenus = list(
      list(
        type = "buttons",
        direction = "right",
        x = 1.4,
        y = 0,
        buttons = list(
          list(method = "update",
               args = list(list(visible =  list(T, T, T, T))),
               label = "AFC anzeigen")
        )
      )
    )
  ) %>% 
  layout(legend = list(x = 1.1, y = 1))

  
  

# Diagramm anzeigen
fig



#---------------
library(patchwork) # To display 2 charts together
library(hrbrthemes)

coeff <- 10

grassgrowth_curve <- ggplot(jahresdaten, aes(x = weeknum, y = growth, color = Ort)) +
  #geom_point(aes(shape=Ort), size=2) + 
  geom_point(size=1,  show.legend = TRUE) +
  geom_line(linewidth = 0.5) +
  #stat_summary(fun = "mean", geom = "line") +
  geom_line(stat = "summary", fun = "mean", linetype="dashed", color="black", 
            linewidth=1, aes(color="mean"), show.legend = F) +
  geom_line(data=standardkurven, aes(
    x = weeknum, 
    y = Durchschnitt...700.m.ü.M...tiefgründig..frisch
  ), color="red", linetype="dotted", linewidth=1, show.legend = F) +
  geom_point( aes(y=afc / coeff),   size=5,  show.legend = TRUE, shape="-") + # Divide by 10 to get the same range than the temperature
  labs(x = "Kalenderwoche", y = "Graswachstum (kg TS/ha/Tag)",
       title = paste0("Graswachstumskurven ",Jahr)) +
  xlim(0,52) +
  scale_y_continuous(
    name = "Graswachstum (kg TS/ha/Tag)",
    sec.axis = sec_axis( ~.*coeff, name="AFC Average Farm Cover (kg TS/ha)"),
  ) +
  #ylim(0, 200) + 
  #geom_line(aes(y = durchschnitt), color = "black", size = 2, linetype = "dotted") +
  theme_minimal() +
  theme(legend.position = "right")

grassgrowth_curve

ay <- list(
  tickfont = list(size=11.7),
  titlefont=list(size=14.6),
  overlaying = "y",
  nticks = 5,
  side = "right",
  title = "Second y axis"
)

ggplotly(grassgrowth_curve) %>%
  add_lines(x=~weeknum, y=~afc, colors=NULL, yaxis="y2", 
           data=jahresdaten, showlegend=FALSE, inherit=FALSE) %>%
  layout(yaxis2 = ay)

ggplotly(grassgrowth_curve)
ggplotly(grassgrowth_curve, tooltip = "text") %>% 
  highlight(on = 'plotly_click', off = 'plotly_doubleclick', 
            opacityDim = .05)

afcfile <- paste("outputs/AFC_", Jahr, ".svg", sep="")
ggsave(file=afcfile, width=10, height=7.5)




afc_curve <- ggplot(df, aes(x = weeknum, y = afc, color = place)) +
  #geom_point(aes(shape=Ort), size=2) + 
  geom_point(size=5,  show.legend = TRUE, shape="-") +
  #geom_line(linewidth = 0.5) +
  #stat_summary(fun = "mean", geom = "line") +
  labs(x = "Kalenderwoche", y = "AFC Average Farm Cover (kg TS/ha)",
       title = paste0("Average Farm Cover "),Jahr) +
  #scale_colour_manual(name = "",
  #                    values = "Dodger Blue 3",
  #                   labels = "c") +
  #guides(color = "none", fill = "none") +
  xlim(0,52) +
  #geom_line(aes(y = durchschnitt), color = "black", size = 2, linetype = "dotted") +
  theme_minimal() +
  theme(legend.position = "right")
  #theme(legend.position = "none")

  

afc_curve

# patchwork doesn't work?!
grassgrowth_curve + afc_curve



ggplotly(afc_curve)
ggplotly(afc_curve, tooltip = "text") %>% 
  highlight(on = 'plotly_click', off = 'plotly_doubleclick', 
            opacityDim = .05)

afcfile <- paste("outputs/AFC_", Jahr, ".svg", sep="")
ggsave(file=afcfile, width=10, height=7.5)






