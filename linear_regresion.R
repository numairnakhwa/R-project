head(Crop_Data)

class_size <-c(0.5,1.0,2.0,3.0,4.0,5.0,7.5,10.0,20.0,30.0) 
for (x in class_size) {
  
  
size <- Crop_Data[Crop_Data[,6]==3.0,]

crops <- c("CASHEW","PADDY","JOWAR","MANGOES")
result <- 0
fruit <- NULL
for (i in crops) {
  
  which_crp <- size[size[,2]==i,]
  if(i=="MANGOES"){
    what_crp <- mango_p[mango_p[,5]=="Hapus(Alphaso)",]
    what_crp$Arrival_Date <- format(as.Date(what_crp$Arrival_Date,format="%d-%m-%Y"),"%Y")
    what_crp$Arrival_Date <- as.numeric(what_crp$Arrival_Date)
  }
  else if(i=="CASHEW"){
    what_crp <- cashew_p
    what_crp$Arrival_Date <- format(as.Date(what_crp$Arrival_Date,format="%d-%m-%Y"),"%Y")
    what_crp$Arrival_Date <- as.numeric(what_crp$Arrival_Date)
  }
  else{
    what_crp <- other_prices[other_prices[,1]==i,]
    what_crp$Arrival_Date <- format(as.Date(what_crp$Arrival_Date,format="%d-%m-%Y"),"%Y")
    
  }
  
  
  x_p <- what_crp$Arrival_Date
  y_p <- what_crp$Modal.Price
  
  relation_p <- lm(y_p~x_p)
  x_c <- which_crp$YEAR
  y_c<- which_crp$total_ar
  relation_c <- lm(y_c~x_c)
  a <- data.frame(x_c=2022)
  b <- data.frame(x_p=2022)
  print(a)
  if(result<predict(relation_c,a)*predict(relation_p,b)){
    result <- predict(relation,a)*predict(relation_p,b)
    fruit <- i
  }
}
print(fruit)
}

head(which_crp)

result
print(summary(relation))
relation
plot(x_p,y_p)
abline(lm(x~y),cex = 1.3,pch = 16,xlab = "Weight in Kg",ylab = "Height in cm")




















