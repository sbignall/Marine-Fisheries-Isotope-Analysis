
##Load libraries as follows:
library(ggplot2)
library(klaR)
library(MASS)
library(readxl)


##Import reference data

known<-read.csv("Final reference data.csv", header = TRUE)


##boxplots##
par(mfrow=c(2,1))
boxplot(known$d13C~known$ï..Location, cex.axis=0.5, cex.names=0.5)
boxplot(known$d15N~known$ï..Location, cex.axis=0.5, cex.names=0.5)

##scatter plot##
ggplot(known, aes(d13C, d15N, color = known$ï..Location)) + geom_point(size=1)+ theme_classic()

##filled ellipses
ggplot(known, aes(d13C, d15N, color = known$ï..Location, fil=known$ï..Location, ))+ 
  stat_ellipse(geom="polygon", alpha=1/2, aes(fill=ï..Location))+
  geom_point(size=1)+ theme_classic()

##elipses breakdown by sea
ggplot(known, aes(d13C, d15N, color = known$ï..Location, fil=known$ï..Location, ))+ 
  stat_ellipse(geom="polygon", alpha=1/2, aes(fill=ï..Location))+
  geom_point(size=1)+ theme_classic()+
  facet_grid(ï..Location~.)

##Read our data file
data<-read.csv("Mackerel_3017_2020.csv", header = TRUE)

##Overlay Our Data
ggplot(NULL, aes(d13C, d15N))+
  stat_ellipse(data=known, geom="polygon", alpha=1/2, aes(fill=ï..Location))+
  theme_minimal()+ ggtitle("Mackerel Origins")+ 
  theme(legend.position = "right")+
  geom_point(data=data[data$Location=="NE Atlantic", ], aes(d13C, d15N), size=1, shape=4) +
  geom_point(data=data[data$Location=="North Sea", ], aes(d13C, d15N), size=1, shape=15 ) +
  geom_point(data=data[data$Location=="Atlantic", ], aes(d13C, d15N), size=1, shape=5 ) +
  geom_point(data=data[data$Location=="Scotland", ], aes(d13C, d15N), size=1, shape=18 ) 

#There was no origin column on the reference data so I changed origin to location and tidied up your
#code a bit. 

##correct code
ggplot(NULL, aes(d13C, d15N))+
  stat_ellipse(data=known, geom="polygon", alpha=1/2, aes(fill=ï..Location))+
  geom_point(data=data, aes(d13C, d15N, pch = data$Location,), colour = 'black', size=2) +
  theme_classic() +
  labs(pch = "Labelled Origin", title = "Assigned Mackerel Origins")


### data analysis options

#cluster analysis on known origin (5 known groups). 
unique(known$ï..Location)

known.Cluster <- kmeans(known[, 3:4], 5, nstart = 20)
known.Cluster

table(known.Cluster$cluster, known$ï..Location)
known.Cluster$cluster <- as.factor(known.Cluster$cluster)
ggplot(known, aes(d13C, d15N, color = known.Cluster$cluster)) + geom_point(size=3)


#### discriminant function 

# Linear Discriminant Analysis with Jacknifed Prediction 

lindis <- lda(ï..Location ~ d13C + d15N, data=known,
              na.action="na.omit", CV=TRUE)
lindis.lda<-lda(ï..Location ~ d13C + d15N,data=known)
lindis # show results 

# Assess the accuracy of the prediction
# percent correct for each category of G
ct <- table(known$ï..Location, lindis$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

# Panels of histograms and overlayed density plots
# for 1st discriminant function
plot(lindis.lda, dimen=1, type="both") # fit from lda 


#predict origin of unknown using lda
pred.data=data.frame(data$d13C, data$d15N)

pred<-predict(lindis.lda, newdata=data)
pred$class
data$Location

