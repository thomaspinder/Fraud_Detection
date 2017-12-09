library(plotly)

customers_main<-read.csv("customers_main.csv")
head(customers_main)
heat<-read.csv("dataset_metrics.csv")
# p <- plot_ly(
# # x = heat$, y = c("d", "e", "f"),
#   z = m, type = "heatmap"
# )
heat
row.names(heat) <- heat$Name
View(heat)
heat <- heat[,2:6]
heat
heat_matrix <- data.matrix(heat)
heat_heatmap <- heatmap(heat_matrix, Rowv=NA, Colv=NA, col = heat.colors(256), scale="column")
coords <- locator(1)
legend(coords,      
       legend = unique(heat_heatmap),
       col = unique(as.numeric(heat_heatmap)), 
       lty= 1,             
       lwd = 5,           
       cex=.7
)

levelplot(heat_heatmap, col.regions=heat.colors)
library(plotly)
library(ggplot2)
p <- plot_ly(x = c("accuracy","recall","precision","f1","auc"), y=c("fold1","fold2","fold3","fold4","fold5","fold6","fold7","fold8","fold9","fold10"),z = heat, type = "heatmap")
p
chart_link = plotly_POST(p, filename="heatmap/simple")
chart_link
midwest
volcano
class(volcano)
class(heat)
heat<-data.matrix(heat)
names(midwest)
str(heat)
heat$fold<-as.numeric(heat$fold)
ncol(heat)
heat<-heat[;1:4]
nba_heatmap <- heatmap(heat, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column")
