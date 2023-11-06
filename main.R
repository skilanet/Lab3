install.packages('arules')
install.packages('arulesViz')
install.packages('ggplot2')



library('arules')
library('arulesViz')
library('ggplot2')



data <- read.delim(file = 'lab3_1.csv', sep = ',', header = TRUE, row.names = 1, encoding  = 'UTF-8')



dataN <- data[, -11]



isCredit <- data$`Кредит`
isCredit[isCredit == 1] <- "With credit"
isCredit[isCredit == 0] <- "Without credit"



itemlist <- sapply(seq_len(nrow(dataN)), function(i) paste(c(isCredit[i], colnames(dataN[i, dataN[i,] == 1])), collapse = ',', sep = ';'))
head(itemlist)
write(itemlist, 'basket.csv')



trans <- read.transactions('basket.csv', format = 'basket', sep = ',')
inspect(trans)
summary(trans)



itemFrequencyPlot(trans, cex.names = 0.8)
rules <- apriori(trans, parameter = list(support = 0.1, confidence = 0.5))
summary(rules)
rulesdf <- as(rules, 'data.frame')
View(rulesdf[order(rulesdf$lift, decreasing = TRUE), ])



rulesWithCredit <- subset(rules, subset = rhs %in% 'With credit')
rulesWithCreditdf <- as(rulesWithCredit, 'data.frame')
rulesWithCreditdf[order(rulesWithCreditdf$lift, decreasing = TRUE), ]
plot(head(sort(rulesWithCredit, by = 'support'), 10), method = 'paracoord')
plot(head(sort(rulesWithCredit, by = "support"), 10), method = "graph",
     control = list(edges = ggraph::geom_edge_link(
       end_cap = ggraph::circle(4, 'mm'),
       start_cap = ggraph::circle(4, 'mm'),
       color = 'black',
       arrow = arrow(length = unit(2, 'mm'), angle = 30, type = 'closed'),
       alpha = .5
     )))



rulesWithoutCredit <- subset(rules, subset = rhs %in% "Without credit")
rulesWithoutCreditdf <- as(rulesWithoutCredit, 'data.frame')
rulesWithoutCreditdf[order(rulesWithoutCreditdf$lift, decreasing = TRUE), ]
plot(rulesWithoutCredit, method = 'paracoord')
plot(head(sort(rulesWithoutCredit, by = 'support'), 10), method = 'paracoord')
plot(head(sort(rulesWithoutCredit, by = 'support'), 10), method = 'graph',
     control = list(edges = ggraph::geom_edge_link(
       end_cap = ggraph::circle(4, 'mm'),
       start_cap = ggraph::circle(4, 'mm'),
       color = 'black',
       arrow = arrow(length = unit(2, 'mm'), angle = 20, type = 'closed'),
       alpha = .5
     )))