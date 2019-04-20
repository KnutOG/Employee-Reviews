library(readr)
employee_reviews <- read_csv("C:/Users/Mehdi/Downloads/employee_reviews_clean.csv")
View(employee_reviews)
str(employee_reviews)
summary(employee_reviews)


rating <- employee_reviews[,10:15]
round(cor(rating,use='complete.obs'), 3)

library(ggcorrplot)
ggcorrplot(cor(rating),colors = c('red','white','green'),hc.order = T,type = 'lower')


##Bartlett's test of shpericity

library(psych)
cortest.bartlett(cor(rating),n = nrow(rating))


##KMO Measire of Sampling Adequacy

KMO(cor(rating))
##  If the variables are strongly related, partial correlations should be small and MSA close to 1

##Determin number of components
#Scree Plot 


library(FactoMineR)
pca_facto = PCA(rating,graph = F)
library(factoextra)
fviz_eig(pca_facto,ncp=11,addlabels = T)

##Using prcomp

pca = prcomp(rating,scale. = T)
fviz_eig(pca,ncp = 11,addlabels = T)
## Make it into a a data frame
scores_df <- as.data.frame(pca$x)
head(scores_df[1:2])



ramp <- colorRamp(c("yellow", "blue"))
colours_by_sum <- rgb( 
  ramp( as.vector(rescale(rowSums(scores_df),c(0,1)))), 
  max = 255 )
## plot both PC1 and PC2 and apply the cluster
ggplot(scores_df, aes(PC1, PC2)) + geom_jitter(size= 0.5)
set.seed(1234)
existing_clustering <- kmeans(scores_df, centers = 3)
existing_cluster_groups <- existing_clustering$cluster

ggplot(scores_df, aes(PC1, PC2)) + geom_jitter(size= 0.3, color= existing_cluster_groups)

##Lets check the # of cluster

set.seed(617)
km = kmeans(x = employee_reviews[,10:15],centers = 3,iter.max=10000,nstart=25)
table(km$cluster)

within_ss = sapply(1:10,FUN = function(x) kmeans(x = employee_reviews[,10:15],centers = x,iter.max = 1000,nstart = 25)$tot.withinss)
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#visualize

k_segments = km$cluster
table(k_segments)
library(cluster)
clusplot(employee_reviews[,10:15],
         k_segments,
         color=T,shade=T,labels=4,lines=0,main='k-means Cluster Plot')

temp = data.frame(cluster = factor(k_segments),
                  factor1 = fa(employee_reviews[,10:15],nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(employee_reviews[,10:15],nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

##Combine the clusters with the original data
data2 = cbind(employee_reviews, k_segments)

library(dplyr); library(ggplot2); library(tidyr)
data2 %>%
  select(overall_rating:senior_mgmt_stars,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,overall_rating:senior_mgmt_stars)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
###Lets find the proportion of the companies 
prop.table(table(data2$k_segments,data2[,1]),1)


library(ggplot2)
tab = prop.table(table(data2$k_segments,data2[,1]),1)
tab2 = data.frame(round(tab,2))
library(RColorBrewer)
ggplot(data=tab2,aes(x=Var2,y=Var1,fill=Freq))+
  geom_tile()+
  geom_text(aes(label=Freq),size=6)+
  xlab(label = '')+
  ylab(label = '')+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))
##Find proportion of the cluster in relation to their employee-stauts

prop.table(table(data2$k_segments,data2[,4]),1)

library(ggplot2)
tab = prop.table(table(data2$k_segments,data2[,4]),1)
tab2 = data.frame(round(tab,2))
library(RColorBrewer)
ggplot(data=tab2,aes(x=Var2,y=Var1,fill=Freq))+
  geom_tile()+
  geom_text(aes(label=Freq),size=6)+
  xlab(label = '')+
  ylab(label = '')+
  scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))

##


##Eigen Value
pca_facto$eig

##Eigen Value only greater than 1

pca_facto$eig[pca_facto$eig[,'eigenvalue']>1,]

data.frame(component = 1:length(pca$sdev), eigen_value = (pca$sdev)^2)



##Parallel Analysis
library(psych)
fa.parallel(rating,fa='pc', show.legend = T )


## The scree plot suggest 3 or 4 componenets we will go with 3 becasue it would have 82.9
## The eigen value suggest 1 componenet 
## THe Parallel analysis suggest 1 components

##First lets do a analysis with 3 componenets 

pca_facto = PCA(rating,scale.unit = T,ncp = 3,graph = F)
pca_facto$var$contrib %>%
  round(2)

charts = lapply(1:3,FUN = function(x) fviz_contrib(pca_facto,choice = 'var',axes = x,title=paste('Dim',x)))
grid.arrange(grobs = charts,ncol=3,nrow=2)


fviz_pca_var(X = pca_facto,col.var = 'contrib',gradient.cols = c('red'),col.circle = 'steelblue',repel = T)





####Combinning clusters with PCA

pca_existing <- prcomp(employee_reviews[,10:15], scale. = TRUE)
plot(pca_existing)
scores_existing_df <- as.data.frame(pca_existing$x)
# Show first two PCs for head countries
head(scores_existing_df[1:2])

plot(PC1~PC2, data=scores_existing_df, 
     main= "Existing TB cases per 100K distribution",
     cex = .1, lty = "solid")
text(PC1~PC2, data=scores_existing_df, 
     labels=rownames(employee_reviews$company),
     cex=.8)

##
set.seed(1234)
existing_clustering <- kmeans(employee_reviews[,10:15], centers = 3)

existing_cluster_groups <- existing_clustering$cluster
plot(PC1~PC2, data=scores_existing_df, 
     main= "Existing TB cases per 100K distribution",
     cex = .1, lty = "solid", col=existing_cluster_groups)
text(PC1~PC2, data=scores_existing_df, 
     labels=rownames(employee_reviews),
     cex=.8, col=existing_cluster_groups)