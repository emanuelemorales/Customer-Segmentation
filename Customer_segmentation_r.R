#Custumers' annual spending amounts of diverse product
#categories for internal structure. 
#Description of the variation in different types of customers
#that a wholesale distributor interacts with.

library(ggplot2)
library(dplyr)
library(factoextra)
library(stats)
library(ggfortify)
library(MASS)
library(mclust)
library(psych)
library(plot3D)

customers <- read.csv("customers.csv")

#Exploratory Data Analysis
head(customers)

#1. check for missing data
sum(is.na(customers))

#2. Summary of dataset
customers_2 = subset(customers, select = Fresh:Delicatessen)
summary(customers_2)

#Data Exploration:_____________________________________________________________________________

#Histogram:

cat = colnames(customers[3:8])

for(i in 1:length(cat)) {
  
  nam <- paste("p", i, sep = "")
  
  p = ggplot(customers, aes_string(cat[i])) +
    geom_histogram(binwidth = 1000) +
    ggtitle(paste("Histogram", cat[i], sep = " "))
  
  assign(nam, p)
  
  p = 0
  
}
gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

#Boxplot:

for (i in 1:length(cat)){
  
  nam <- paste("p", i, sep = "")
  
  p = ggplot(customers, aes_string(cat[i])) +
    geom_boxplot(outlier.alpha = .25) +
    scale_y_log10(
      labels = scales::dollar, 
      breaks = quantile(customers$cat[i])
    )
  
  assign(nam, p)
  
  p = 0
}

gridExtra::grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3)

#Scatterplot matrix and correlation matrix: multicollinearity problem.

upper.panel<-function(x, y){
  points(x,y, pch=19)
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}

pairs(customers[,3:8], lower.panel = NULL, upper.panel = upper.panel) 


pairs.panels(customers[,3:8], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

#Now it is necessary to rescale data and reduce the skewness of data

customers_2$Fresh=log(customers_2$Fresh)
customers_2$Milk=log(customers_2$Milk)
customers_2$Grocery=log(customers_2$Grocery)
customers_2$Frozen=log(customers_2$Frozen)
customers_2$Detergents_Paper=log(customers_2$Detergents_Paper)
customers_2$Delicatessen=log(customers_2$Delicatessen)


for(i in 1:length(cat)) {
  
  nam <- paste("p", i, sep = "")
  
  p = ggplot(customers_2, aes_string(cat[i])) +
    geom_histogram(binwidth = 0.2) +
    ggtitle(paste("Histogram", cat[i], sep = " "))
  
  assign(nam, p)
  
  p = 0
  
}
gridExtra::grid.arrange(p1, p2,p3,p4,p5,p6, ncol = 2)

#Outliers detection and trimming

MD = mahalanobis(customers_2, colMeans(customers_2), cov(customers_2))
customers_2$MD = round(MD,3)

MNT = qplot(customers_2$MD,
      geom="histogram",
      binwidth = 2,  
      main = "Histogram for MD", 
      xlab = "Mahalanobis distance",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2)
      #xlim=c(20,50)
)

Q1 =  quantile(customers_2$MD, 0.15)
Q3 = quantile(customers_2$MD, 0.85)

step = 1.5*(Q3 -Q1)

customers_2$outlier_maha = FALSE
customers_2$outlier_maha[customers_2$MD <= Q1-step] = TRUE
customers_2$outlier_maha[customers_2$MD >= Q3+step] = TRUE

customers_trim = customers_2[customers_2$outlier_maha == FALSE,]
head(customers_trim)

MT = qplot(customers_trim$MD,
      geom="histogram",
      binwidth = 1,  
      main = "Histogram for MD", 
      xlab = "Mahalanobis distance trimmed",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2)
      #xlim=c(20,50)
)

gridExtra::grid.arrange(MNT, MT, ncol = 2)

for(i in 1:length(cat)) {
  
  nam <- paste("p", i, sep = "")
  
  p = ggplot(customers_trim, aes_string(cat[i])) +
    geom_histogram(binwidth = 0.2) +
    ggtitle(paste("Histogram", cat[i], sep = " "))
  
  assign(nam, p)
  
  p = 0
  
}
gridExtra::grid.arrange(p1, p2,p3,p4,p5,p6, ncol = 2)


#Implement Multidimensional scaling in order to reduce the dimensionality
#and have the possibility to have a plot of our data. 
#In this case the more indicated is Euclidean distance.

matrix = as.matrix(customers_trim[,1:6])

#MULTIDIMENSIONAL SCALING

dist = dist(scale(matrix))

mod = cmdscale(dist, eig = TRUE)
mod1 = cmdscale(dist, k = 3)

eig <- mod$eig
dims <- 1:attr(dist, "Size")
eig <- data.frame(dimension = dims, Eigenvalues = eig)
ggp <- ggplot(eig[1:8,], aes(x = dimension, y = Eigenvalues)) +
  geom_point() +
  geom_line() +
  scale_x_continuous("Dimension", breaks = 1:attr(dist, "Size")) +
  geom_hline(yintercept = 0, linetype = "dashed")
#print(ggp)


scatter3D(mod1[,1], mod1[,2], mod1[,3], colvar = NULL, col = "blue",
          pch = 19, cex = 0.5)
ggp



#PCA________________________________________________________________________________

pca = prcomp(matrix, scale = TRUE)

#screeplot
fviz_eig(pca)

#biplot: it puts on the same plot the PC1 and PC2 scores and
#on the top axis the loadings on PC 1 and the loadings on PC2
biplot(pca, scale = 0, xlabs=rep("o", nrow(customers_trim)))

ggp_pca= ggplot(pca$x, aes(PC1, PC2))+
  geom_point()
print(ggp_pca)

summary(pca)


#kernel density:per il riconoscimento di pattern e per la classificazione attraverso una stima di densità negli spazi metrici, o spazio delle feature. 
#Per ogni x all'interno dello spazio delle feature, 
#l'algoritmo permette di calcolare la probabilità di appartenere ad una classe C, 
#considerando la densità di C in un intorno k del punto x. 
#Il metodo si basa su un intorno di dimensione fissa calcolata in funzione al numero di osservazione N.

dens_pca = ggplot(pca$x, aes(PC1, PC2))+
  geom_point(shape = 19, col = "black") +
  labs(y="PC1", x = "PC2")+
  geom_density2d()

dens_mds = ggplot(mod1, aes(mod1[,1], mod1[,2]))+
  labs(y="Dimension 1", x = "Dimension 2")+
  geom_point(shape = 19, col = "black") + geom_density2d()

dens_pca
dens_mds


customers_trim = cbind(customers_trim, pca$x[,1:2]) 
customers_trim = cbind(customers_trim, dim1 = mod1[,1], dim2 = mod1[,2] )
data_comp = customers_trim[, c(9:10)]


#k-mean analysis:

#WSS Plot Function

set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max = 15
data = data_comp
wss = sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

results = kmeans(data_comp, 2)

library(ggplot2)
theme_set(
  theme_minimal() +
    theme(legend.position = "right")
)

k = ggplot() +
  geom_point(data = data_comp, 
             mapping = aes(x = PC1,  #dim1
                           y = PC2, #dim2
                           colour = results$cluster)) +
  geom_point(mapping = aes_string(x = results$centers[, "PC1"], 
                                  y = results$centers[, "PC2"]),
             color = "red", size = 4) +
  theme(legend.position = "none") 

k

results$size
results$centers

#Model based analysis :making the cluster shapes more likely than what is generally
#the case if traditional clustering methods are used looking for the winning model, the number of clusters

#winning model
mc = Mclust(data_comp)
plot(mc, what = "BIC")

plot(mc, what = "classification")
#plot(mc, what = "uncertainty")
plot(mc, what = "density", type= "persp")

summary(mc)

mc$parameters

#force to have another number of cluster,not the best one
mc.3 = Mclust(data_comp, G = 4)
plot(mc.3)


k
plot(mc, what = "classification")
