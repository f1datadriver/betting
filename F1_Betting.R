driver = c("Verstappen", "Leclerc", "Sainz", "Hamilton", "Russell", 
            "Perez", "Norris", "Alonso", "Ricciardo", "Ocon",
            "Gasly", "Albon", "Zhou", "Magnussen", "Stroll",
            "Schumacher", "Vettel", "Tsunoda", "Bottas", "Latifi")

spreads = c(-215, 450, 1100, 1400, 2000,
            2000, 30000, 35000, 50000, 50000,
            75000, 200000, 200000, 200000, 200000,
            200000, 200000, 200000, 250000, 400000)

df = data.frame(driver, spreads)

underdog_prob = function(spread) {
  prob = (100 / ((spread + 100)))
  return(prob)
}

fave_prob = function(spread) {
  prob = (-1*spread) / ((-1*spread) + 100)
  return(prob)
}

df$prob = ifelse(sign(df$spreads) == -1, 
                     fave_prob(df$spreads), 
                     underdog_prob(df$spreads))

sum_prob = sum(df$prob)

library(ggplot2)

plot = ggplot(data=df, aes(x=reorder(driver, -prob), y=prob)) +
  geom_bar(stat="identity", fill="blue1") +
  xlab("Driver") +
  ylab("Win Probability") +
  ggtitle("Win Probability Implied by Betting Market: Singapore Grand Prix") +
  theme(plot.title=element_text(hjust=0.5)) +
  ylim(0,1) +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  theme(panel.background = element_blank()) 

plot

df_sing_22 = df

# get team symbol onto chart (use powerpoint?)
# driver nationality flag below them on x axis