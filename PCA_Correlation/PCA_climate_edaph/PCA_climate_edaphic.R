library(plyr)
library("factoextra")
library("FactoMineR")
library(ggplot2)
library(ggfortify)
library(knitr)
library(kableExtra)

setwd("/Users/ocornejo/Pullman/Trainees/PhD/Joel_Nelson/cacao/2018/bayenv")
#climatic data
data <- data.frame(read.table("Climatic_data_4Joel.tab",header=T))
data2 <- data[,-c(1:2)]
data3 <- scale(data2)
clim_names <- data.frame(read.table("climate_subset.names",header=F))
data3 <- data3[,as.character(data3[,1])]


# Edaphic data
data <- data.frame(read.table("cacao.edaphic.var.env",header=T))
data4 <- scale(data)
eda_names <- data.frame(read.table("Edaphic_variables_2work.tab",header=F))
data4 <- data4[,as.character(eda_names[,1])]

### Principal Components climate and soil data

clim.pca <- PCA(data3, scale.unit=TRUE, graph=FALSE, ncp=5)
eda.pca <- PCA(data4, scale.unit=TRUE, graph=FALSE, ncp=18)


## Identify PCs that contribute 90% or more of cumulative variance

eda.pca$eig
clim.pca$eig

clim.eign <- clim.pca$eig
clim.eign[,1] <- round(clim.eign[,1], dig= 4) 
clim.eign[,2] <- round(clim.eign[,2], dig= 1) 
clim.eign[,3] <- round(clim.eign[,3], dig= 1) 

clim.eign %>% kable() %>% kable_styling(bootstrap_options = "striped", full_width = F)

eda.eign <- eda.pca$eig
eda.eign[,1] <- round(eda.eign[,1], dig= 4) 
eda.eign[,2] <- round(eda.eign[,2], dig= 1) 
eda.eign[,3] <- round(eda.eign[,3], dig= 1) 

eda.eign %>% kable() %>% kable_styling(bootstrap_options = "striped", full_width = F)


fviz_screeplot(eda.pca, ncp=10)
fviz_screeplot(clim.pca, ncp=5)



################
## contributions 
################

# climatic
fviz_contrib(clim.pca, choice = "var", axes = 1, top = 5)
fviz_contrib(clim.pca, choice = "var", axes = 2, top = 5)
fviz_contrib(clim.pca, choice = "var", axes = 3, top = 5)

## edaphic 

fviz_contrib(eda.pca, choice = "var", axes = 1, top = 18)
fviz_contrib(eda.pca, choice = "var", axes = 2, top = 18)
fviz_contrib(eda.pca, choice = "var", axes = 3, top = 18)
fviz_contrib(eda.pca, choice = "var", axes = 4, top = 18)
fviz_contrib(eda.pca, choice = "var", axes = 5, top = 10)
fviz_contrib(eda.pca, choice = "var", axes = 6, top = 10)


#### Plot of PCA (different version to the one presented in the paper)

fviz_pca_ind(eda.pca, col.ind="cos2", axes=c(1,2),select.var= list(contrib = 7)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_ind(clim.pca, col.ind="cos2", axes=c(1,2),select.var= list(contrib = 7)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()


############
# Correlations of contributing variables to each combinations of PCs
############

fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 5), axes=c(1,2)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 6), axes=c(1,3)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 4), axes=c(1,4)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 4), axes=c(1,5)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 4), axes=c(1,6)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 4), axes=c(2,3)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 4), axes=c(2,4)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 2), axes=c(2,5)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 3), axes=c(2,6)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 3), axes=c(3,4)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 3), axes=c(3,5)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 2), axes=c(3,6)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 4), axes=c(4,5)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 4), axes=c(4,6)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(eda.pca, col.var="cos2", select.var= list(contrib = 4), axes=c(5,6)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()



fviz_pca_var(clim.pca, col.var="cos2", select.var= list(contrib = 5), axes=c(1,2)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(clim.pca, col.var="cos2", select.var= list(contrib = 5), axes=c(1,3)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()
fviz_pca_var(clim.pca, col.var="cos2", select.var= list(contrib = 2), axes=c(2,3)) + scale_color_gradient2(low="white", mid="blue",high="red", midpoint=0.5) + theme_minimal()


###############################
## PC final figures
###############################

data3.1 <- data3

populations=c("Amelonado","Contamana","Criollo","Curaray","Guianna","Iquitos","Maranon","Nacional","Nanay","Purus")
p <- autoplot(prcomp(data3), label = FALSE, shape=FALSE, label.size = 6.2, loadings.label = TRUE, loadings.label.size=2.7, loadings.colour = "royalblue2", loadings.label.colour = "royalblue2", loadings.label.repel =TRUE) + scale_x_continuous(limits=c(-0.7,0.7)) + scale_y_continuous(limits=c(-0.7,0.7)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text.x = element_text(colour="grey20",size=12,hjust=.5,vjust=.5,face="plain"), axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"))
p + geom_text(aes(label=populations),hjust=0, vjust=0, colour="black")


data4.1 <- data4

p2 <- autoplot(prcomp(data4), label = FALSE, shape=FALSE, label.size = 3.2, loadings.label = TRUE, loadings.label.size=2.7, loadings.colour = "darkseagreen4", loadings.label.colour = "darkseagreen4", loadings.label.repel =TRUE) + scale_x_continuous(limits=c(-0.7,0.7)) + scale_y_continuous(limits=c(-0.7,0.7)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black")) + theme(axis.text.x = element_text(colour="grey20",size=12,hjust=.5,vjust=.5,face="plain"), axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"))
p2 + geom_text(aes(label=populations),hjust=0, vjust=0, colour="black")

