other_prices <- read.csv("Other_p.csv")

head(other_prices)
data.frame(other_prices,"X2003","X2004","X2005","X2006","X2007","X2008","X2009","X2010","X2011","X2012","X2013","X2014","X2015","X2016")
install.packages('reshape')
library(reshape)
other_prices <- melt(other_prices, id=(c("Crop")))
write.csv(other_prices,"other_p")


colours <- c("red", "orange", "blue", "yellow", "green","black","purple","brown","white","violet")

new_graph <- other_prices[other_prices[,1]=="JOWAR",]
other_prices$Crop
barplot(as.matrix(select(new_graph,value)), main="My Barchart", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4, beside=TRUE)

ggplot(data = other_prices, mapping = aes(x = other_prices$variable, y = other_prices$value,color=other_prices$Crop)) +
  geom_line()
