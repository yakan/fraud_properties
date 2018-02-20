setwd("~/Documents/USC_code/fraud_demo/project1")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(lubridate)
library(h2o)

######################################################################

data = readRDS("ny_set.rds")

# class(data)
data = data[,17:61]

# do PCA
pr.out=prcomp(data,scale=TRUE)
pr.out$center
pr.out$scale

### get the result of PCA
pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)

plot(pve,xlab='Principal Component',ylan='Proportion of Variance Explained',
     ylim=c(0,0.2),type='b',main='Scree Plot')
plot(cumsum(pve), xlab="Principal Component ", ylab=" Cumulative Proportion of Variance Explained ", 
     ylim=c(0,1), type='b')

## PC1-8 
pca=pr.out$rotation 
pca=data.frame(pca)

## selecting only PC1 to PC8
pcaneed=pca %>%
  select(PC1:PC8)

## Transform the z scaled variables into PC space 
## by multiplying 2 matrix
finaldata=as.matrix(data) %*% as.matrix(pcaneed)
finaldata=data.frame(finaldata)
saveRDS(finaldata, file = "finaldata.rds")

## calculate fraud score
mean = colMeans(finaldata)
cov = cov(finaldata)
ma_dist = mahalanobis(finaldata, mean, cov)
ma_dist = data.frame(cbind(1:length(ma_dist), ma_dist))
colnames(ma_dist) = c("Record", "ma_dist")

summary(ma_dist$ma_dist)
quantile(ma_dist$ma_dist, 0.99)

## plot a histogram
ggplot(data = ma_dist, aes(x = ma_dist)) +
  geom_histogram(bins = 50) +
  coord_cartesian(xlim=c(0,500000)) +
  scale_y_log10() +
  ggtitle("Fraud Score of PCA/Mahalanobis") +
  xlab("Fraud Score") +
  ylab("count") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))

## find the index of top 10% and 1% records
cut_off = round(dim(ma_dist)[1] * 0.1)
top10_index_ma = order(ma_dist$ma_dist, decreasing = TRUE)[1:cut_off]
cut_off2 = round(dim(ma_dist)[1] * 0.01)
top1_index_ma = order(ma_dist$ma_dist, decreasing = TRUE)[1:cut_off2]

## Autoencoder using h2o
localH2O = h2o.init()

finaldata.hex = as.h2o(finaldata)

finaldata.dl = h2o.deeplearning(x = names(finaldata.hex), training_frame = finaldata.hex,
                                autoencoder = TRUE,
                                reproducible = F)

## Reconstruct
finaldata.anon = h2o.anomaly(finaldata.dl, finaldata.hex, per_feature=TRUE)
err = as.data.frame(finaldata.anon)
# the "err" data frame contains all reconstruction errors of each record

## Plot the reconstructed Squared Error for the first 3 PCs
plot(err$reconstr_PC1.SE, main='Reconstruction Error - PC1', 
     ylab = "Reconstruction Error")
plot(err$reconstr_PC2.SE, main='Reconstruction Error - PC2', 
     ylab = "Reconstruction Error")
plot(err$reconstr_PC3.SE, main='Reconstruction Error - PC3', 
     ylab = "Reconstruction Error")

auto_score = data.frame(rowSums(err))
auto_score = cbind(1:dim(err)[1], auto_score)
colnames(auto_score) = c("Record", "auto_score")

summary(auto_score$auto_score)
quantile(ma_dist$ma_dist, 0.99)

## histogram of the scores
ggplot(data = auto_score, aes(x = auto_score)) +
  geom_histogram(bins = 100) +
  coord_cartesian(xlim=c(1e-10,1)) +
  scale_y_log10() +
  ggtitle("Fraud Score of Autoencoder") +
  xlab("Fraud Score") +
  ylab("count") +
  theme(plot.title = element_text(size = 20, hjust = 0.5),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18))

ggplot(data = err, aes(x = reconstr_PC1.SE)) +
  geom_histogram() +
  scale_x_log10()

top10_index_auto = order(auto_score$auto_score, decreasing = TRUE)[1:cut_off]
top1_index_auto = order(auto_score$auto_score, decreasing = TRUE)[1:cut_off2]

######################################################################