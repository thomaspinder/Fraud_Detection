library("Rtsne")
?read.csv
features<-read.csv("features.csv")
features.smote<-read.csv("features_smoted.csv")
labels<-read.csv("labels.csv", header = F)
labels.smote<-read.csv("labels_smoted.csv", header = F)

features<-features[,-1]
features.smote<-features.smote[,-1]
labels<-labels[,-1]
labels.smote<-labels.smote[,-1]

#No SMOTE

features<-cbind(features, labels)

unique.features<-unique(features)
cols<-cut(unique.features$labels, 2, c("cadetblue2", "red"))
class(unique.features$labels)
unique.features$labels<-as.factor(unique.features$labels)
class(unique.features$labels)


NoSmote.tsne <- Rtsne(unique.features[1:49], dims = 2, perplexity=30, verbose=TRUE, max_iter = 500)

plot(NoSmote.tsne$Y, t='n', main="tsne")
text(NoSmote.tsne$Y, labels=unique.features$label, col=alpha(cols,0.5))

#SMOTE

features.smote<-cbind(features.smote, labels.smote)
unique.features.smote<-unique(features.smote)
cols<-cut(unique.features.smote$labels, 2, c("cadetblue2", "red"))
class(unique.features.smote$labels)
unique.features.smote$labels.smote<-as.factor(unique.features.smote$labels.smote)
class(unique.features.smote$labels)

Smote.tsne <- Rtsne(unique.features.smote[1:49], dims = 2, perplexity=22, verbose=TRUE, max_iter = 500)

plot(Smote.tsne$Y, t='n', main="tsne")
text(Smote.tsne$Y, labels=unique.features.smote$label, col=alpha(cols,0.5))
