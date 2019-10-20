cropping_pattern<- read.csv("cropping_pattern.csv")
dooa<- read.csv("DISPERSAL OF OPERATED AREA.csv")
gca<- read.csv("gross cropped area.csv")
irr_status<- read.csv("IRRIGATION STATUS.csv")
land_use<- read.csv("LAND USE.csv")
src_irr<- read.csv("sources of irrigation.csv")
WELL_tube<- read.csv("WELLS & TUBEWELLS.csv")


plot(dooa)
library(ggplot2)
ss <- subset(Crop_Data,Crop_Data$CROP=="MANGOES")
ggplot(Crop_Data, aes(x = SIZE_CLASS, y = area_avail)) + 
  geom_bar(color = "#FC4E07", size = 2)

ggplot(ss, aes(x = SIZE_CLASS, y = Crop_Data$CROP )) + 
  geom_area(aes(color="red", fill = "blue"), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))
o.stat_smooth()

Crop_Data<- Crop_Data[!Crop_Data[,2]==0,]
plot(Crop_Data$CROP,Crop_Data$area_avail)
install.packages('ggpubr')
install.packages("reshape2")
library(reshape2)
library(ggpubr)
library(devtools)
bp <- ggbarplot(Crop_Data, x = Crop_Data$CROP, y = Crop_Data$area_avail,
                fill = "cyl",               # change fill color by cyl
                color = "white",            # Set bar border colors to white
                palette = "jco",            # jco journal color palett. see ?ggpar
                sort.val = "asc",           # Sort the value in ascending order
                sort.by.groups = TRUE,      # Sort inside each group
                x.text.angle = 90           # Rotate vertically x axis texts
)
bp + font("x.text", size = 8)


#1
ggplot(data, aes( fill=Crop_Data$CROP,x=Crop_Data$YEAR, y=Crop_Data$area_avail )) + 
  geom_bar(position="dodge", stat="identity")
data <- data.frame(cropping_pattern$SizeClass,Crop_Data$CROP,Crop_Data$area_avai)

ggplot(data, aes( fill=Crop_Data$SIZE_CLASS,x=Crop_Data$irr_ar, y=Crop_Data$unirr_ar )) + 
  geom_bar(position="dodge", stat="identity")
#2
ggplot(land_use, aes(fill=land_use$SizeClass, x=land_use$YEAR, y=land_use$total_area )) + 
  geom_bar(position="dodge", stat="identity")
#3
ggplot(land_use, aes(fill=irr_status$total_area, x=irr_status$YEAR, y=irr_status$Wholly_Irrigated_Holdings_ar )) + 
  geom_contour(position="dodge", stat="identity")
ggplot(land_use, aes(fill=irr_status$total_area, x=irr_status$YEAR, y=irr_status$Wholly_Irrigated_Holdings_ar )) + 
  geom_bar(position="dodge", stat="identity")
#4
ggplot(WELL_tube, aes(fill=WELL_tube$total_area, x=irr_status$YEAR, y=WELL_tube$total_hold )) + 
  geom_bar(position="dodge", stat="identity")
df <- data.frame(
  size=WELL_tube$SizeClass,
  hold=WELL_tube$total_hold
)
df <- melt(df ,  id.vars = 'size', variable.name = 'well')
ggplot(df, aes(size,value)) + geom_line(aes(colour = well))

df2 <- data.frame(year=WELL_tube$YEAR,
                  holding=WELL_tube$total_hold)
df2 <- melt(df2 ,  id.vars = 'year', variable.name = 'wells')

ggplot(df2, aes(year,value)) + geom_line(aes(colour = wells))
#5
library(dplyr)
colours <- c("red", "orange", "blue", "yellow", "green")
barplot(as.matrix(select(src_irr,canal_hd,tank_hd,well_hd,tubewel_hd,other_hd)), main="My Barchart", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)
#6
colours <- c("red", "orange", "blue", "yellow", "green","black","purple","brown","white","violet")
barplot(as.matrix(select(dooa,Entirely_in_the_Village_no.)), main="My Barchart", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)
#7
colours <- c("red", "orange", "blue", "yellow", "green","black","purple","brown","white","violet")
barplot(as.matrix(select(gca,Gr_irr_ar,Gr_unirr_ar)), main="My Barchart", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4,col=colours, beside=TRUE)
#8
#mango_p$Arrival_Date <- as.Date(mango_p$Arrival_Date,
#                               format = "%m/%d/%Y")
mango_p <- read.csv("E:/Users/ABC/Desktop/MY BID PROJECT/MainProject/Mango_Price(2010-19).csv")
mango_p <- mango_p[mango_p[,1]==c("Maharashtra"),]

ggplot(data = mango_p, mapping = aes(x = mango_p$Arrival_Date, y = mango_p$Modal.Price,color=mango_p$Variety)) +
  geom_line()

grey_theme <- theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
                    axis.text.y = element_text(colour = "grey20", size = 12),
                    text = element_text(size = 16))

ggplot(mango_p, aes(x = mango_p$Variety, y = mango_p$Modal.Price)) +
  geom_boxplot() +
  grey_theme

#9


install.packages("viridis")
install.packages("hrbrthemes")
library(viridis)
library(hrbrthemes)
ggplot(WELL_tube, aes(fill=WELL_tube$well_Diesel, y=WELL_tube$wells_Electric, x=WELL_tube$SizeClass)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = F) +
  ggtitle("Dependency wells has decreased") +
  theme_ipsum() +
  xlab("")




library(dplyr)
colours <- c("red", "orange", "blue", "yellow", "green")
barplot(as.matrix(select(src_irr,well_hd)), main="My Barchart", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)

plot(x$Arrival_Date,x$Modal.Price)

x <- mango_p[mango_p[,5]=="Hapus(Alphaso)",]

colours <- c("red", "orange", "blue", "yellow", "green","black","purple","brown","white","violet")
barplot(as.matrix(select(dooa,Entirely_in_the_Village_no.)), main="My Barchart", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4, beside=TRUE, col=colours)

colours <- c("red", "orange", "blue", "yellow", "green","black","purple","brown","white","violet")
barplot(as.matrix(select(gca,Gr_irr_ar,Gr_unirr_ar)), main="My Barchart", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4,col=colours, beside=TRUE)

barplot.default(as.matrix(select(gca,Gr_irr_ar,Gr_unirr_ar)), main="My Barchart", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4,col=colours, beside=TRUE)



plot(gca)

barplot(as.matrix(select(mango_p,diff,Modal.Price)), main="My Barchart", ylab = "Numbers", cex.lab = 1.5, cex.main = 1.4,col=colours, beside=TRUE)

diff <- mango_p$Max.Price-mango_p$Min.Price
plot(diff,mango_p$Modal.Price)
View(diff)
ggplot(df, aes(size,value)) + geom_line() + facet_grid(well ~ .)

df2 <- data.frame(year=WELL_tube$YEAR,
                  holding=WELL_tube$total_hold
                 )

df2 <- melt(df2 ,  id.vars = 'year', variable.name = 'wells')

ggplot(df2, aes(year,value)) + geom_line(aes(colour = wells))
ggplot(df2, aes(year,value)) + geom_bar() + facet_grid(wells ~ .)

 WELL_tube[WELL_tube[,1]==2011,7]

c <- Crop_Data[Crop_Data[,2]==c("MANGOES"),]
c <- Crop_Data[Crop_Data[,6]==0.5,][Crop_Data[Crop_Data[,6]==0.5,][,2]==c("MANGOES"),]
hist(Crop_Data[Crop_Data[,6]==0.5,][Crop_Data[Crop_Data[,6]==0.5,][,2]==c("MANGOES"),])
c <- Crop_Data[Crop_Data[,6]==0.5,]

plot(c$YEAR,c$area_avail,main = "plotting")
d <- Crop_Data[Crop_Data[,2]==c("MANGOES"),]
dfm <- melt(d[,c("YEAR","CROP")],id.vars = 1)
plot(Crop_Data[6:10])
ggplot(dfm,aes(x=YEAR,y = value )) + 
  geom_bar(aes(fill = variable),stat = "identity",position = "dodge") 


install.packages('XML')
library(XML)
p <- readHTMLTable("https://www.worldweatheronline.com/lang/en-in/ratnagiri-weather-averages/maharashtra/in.aspx",which = 1)
plot(d$CROP,d$area_avail)
mango <- xmlToList("Mango_2015.xml")
install.packages("ggpubr")
install.packages("devtools")
install.packages('dplyr')
install.packages('easyGgplot2')



df3 <- data.frame(
  canal=src_irr$canal_hd,
  tank=src_irr$tank_hd,
  well=src_irr$well_hd,
  tube_well=src_irr$tubewel_hd,
  other=src_irr$other_hd
)
df3 <- melt(df ,  id.vars = 'size', variable.name = 'stc')

sum(src_irr[src_irr[,1]==1996,9])


library(ggplot2)
df = melt(data.frame(c(a=sum(src_irr[src_irr[,1]==1996,9]),sum(src_irr[src_irr[,1]==1996,11]))),
          metric="X",
          variable_name="Z")
ggplot(df, aes(fill=variable,experiment, value)) + 
  geom_bar(position="dodge")

df = melt(data.frame(A=c(sum(src_irr[src_irr[,1]==1996,9]), sum(src_irr[src_irr[,1]==2001,9])), B=c(sum(src_irr[src_irr[,1]==1996,11]), sum(src_irr[src_irr[,1]==2001,11])), 
                     experiment=c("X", "X & Y")),
          variable_name="metric")
ggplot2.barplot(data=src_irr[6:15], xName="time", yName='total_bill')
plot(gca)
plot(land_use)
plot(dooa[6:14])
plot(WELL_tube, WELL_tube$well_Diesel, type = "l", frame = FALSE, pch = 19, 
     col = "red", xlab = "x", ylab = "y")
line(WELL_tube,WELL_tube$wells_Electric,type="l",lty=1)
plot(WELL_tube[6:10])


ggplot(data = mango_p, mapping = aes(x = mango_p$Arrival_Date, y = diff,color=mango_p$Variety)) +
  geom_line()





library(XML)
doc <- xmlParse("Mango_2014.xml")




plot(mango_p$Arrival_Date,mango_p$Max.Price)


plot(mango_p)


ggplot(mango_p, aes(fill=mango_p$Variety, x=mango_p$Arrival_Date, y=mango_p$Max.Price)) + 
  geom_bar(position="dodge", stat="identity")



ggplot(data = yearly_counts, mapping = aes(x = year, y = n, color = genus)) +
  geom_line()


max(mango_p$Modal.Price)




 
 grey_theme <- theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
                     axis.text.y = element_text(colour = "grey20", size = 12),
                     text = element_text(size = 16))
 
 # create a boxplot with the new theme
 ggplot(mango_p, aes(x = mango_p$Variety, y = mango_p$Modal.Price)) +
   geom_point() +
   grey_theme

 hapus <- mango_p[mango_p[,5]=="Hapus(Alphaso)",]

 cashew_p <- read.csv("Cashew_price.csv")
 cashew_p$Arrival_Date <- as.Date(cashew_p$Arrival_Date,format="%d/%m/%Y")
 
 other_prices$Arrival_Date <- as.Date(other_prices$Arrival_Date,format="%d/%m/%Y")
 
 
 install.packages("plotly")
 library(plotly)
 ?icon
 install.packages('emojifont')
 library(emojifont)
 
p <- ggplot(hapus, aes(x = hapus$Arrival_Date, y = hapus$Modal.Price)) +
   geom_point()
p <- p + geom_point() + stat_smooth()
ggplotly(p)
yuy
print()
