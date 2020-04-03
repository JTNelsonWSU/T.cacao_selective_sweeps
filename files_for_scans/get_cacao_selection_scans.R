
#####The following script has been designed to generate final selection scans containing the ICS data from Nelson et al. (under review).
#####The different patterns of greyscale represents the different chromosomes and the red dots are putative outliers. Please open an issue or contact joel.t.nelson@wsu.edu for any questions.

library(ggplot2)


Z2 <- read.table(paste("Z2.ICS.ame.txt"), header = T)   #You will need to change the file name depending on which population you wish to view.


ggplot(data = Z2, aes(x = midpoint, y = Zlog,color=color)) + geom_point(size=1.0) + 
  theme(axis.text.x= element_text(size = rel(1.1), hjust = 1, vjust = 1)) + ylab(expression(log(ICS_Distance))) + 
  guides(colour=FALSE) +
  scale_color_manual(values = c("darkred", "grey20", "grey58")) +
  ggtitle("Population") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank()) +
  theme(plot.title = element_text(hjust =0.5)) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(axis.text.y = element_text(size = rel(1.3), color = "black")) +
  theme(axis.title.y = element_text(hjust = 0.5, size = 14)) +
  theme(plot.title = element_text(face = "bold", size = 20))
