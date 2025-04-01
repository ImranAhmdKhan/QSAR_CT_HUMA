# load data 
setwd("H:/New folder (2)/A/")# Load required libraries
library(prospectr)  # For Kennard-Stone
library(ggplot2)    # For PCA and plotting
library(ggfortify)  # For plotting PCA results

# Step 1: Load the dataset
data <- read.csv("A.csv")
# create trainingset and testset id using kenStone on euclidian distance 
library(prospectr) 
xspace <- data[,-1] 
ks <- kenStone(as.matrix(xspace), k=12, metric = "mahal",pc=0.99, .center = 
                 TRUE, .scale = FALSE) 
ks$test 
trainid <- ks$test 
# assign testset and trainingset 
trainingset <- data[trainid,] 
testset <- data[-trainid,] 
x_train <- as.matrix(trainingset[-1]) 
y_train <- data.matrix(trainingset[1]) 
x <- x_train 
y <- y_train 
x_test <- as.matrix(testset[-1]) 
y_test <- as.matrix(testset[1]) 
data_pca <- data 
data_pca$Response[trainid]=0 
data_pca$Response[-trainid]=1 
pc <- prcomp(data_pca[,-1],scale. = TRUE) 
summary(pc) 
plot(pc, type="lines") 
library(rgl) 
library(ggplot2) 
library(ggfortify) 
library(magrittr) 
# design figure frame and axis tick 
tick_frame <-  
  data.frame(ticks = seq(-20, 20, length.out = 5),  
             zero=0) %>% 
  subset(ticks != 0) 
lab_frame <- data.frame(lab = seq(-20, 20), 
                        zero = 0) %>% 
  subset(lab != 0) 
tick_sz <- (tail(lab_frame$lab, 1) -  lab_frame$lab[1]) / 128 
pc_plot <- cbind(data_pca[,1],pc$x) 
# PLOT ---- 
ggplot(pc_plot, aes(x=pc_plot[,2],y=pc_plot[,3])) +labs(x = 'PC1 ', y 
                                                        = 'PC2 ')+
  # y axis line 
  geom_segment(x = 0, xend = 0,  
               y = lab_frame$lab[1], yend = tail(lab_frame$lab, 1), 
               size = 1.5) + 
  # x axis line 
  geom_segment(y = 0, yend = 0,  
               x = lab_frame$lab[1], xend = tail(lab_frame$lab, 1), 
               size = 1.5) + 
  # x ticks 
  geom_segment(data = tick_frame,  
               aes(x = ticks, xend = ticks,  
                   y = zero, yend = zero + tick_sz),size =1.5) + 
  # y ticks 
  geom_segment(data = tick_frame,  
               aes(x = zero, xend = zero + tick_sz,  
                   y = ticks, yend = ticks),size =1.5) +  
  
  # labels 
  geom_text(data=tick_frame, aes(x=ticks, y=zero, label=ticks), 
            vjust=1.5,size = 6) + 
  geom_text(data=tick_frame, aes(x=zero, y=ticks, label=ticks), 
            hjust=1.5,size = 6) + 
  # legends 
  scale_color_discrete(name  ="dataset", 
                       labels=c("Trainingset", "Testset"))+ 
  # THE DATA POINT 
  geom_point(aes(color = factor(V1)),size = 4,alpha =.6) + 
  scale_color_manual(labels = c("Training set", "Test set"), values = 
                       c("dodgerblue", "red2"))+ 
  # title 
  ggtitle("PCA Analysis")+ 
  
  theme_bw()+ 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())+ 
  theme(axis.ticks.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank())+ 
  theme(axis.title = element_text(size = 15, face = "bold"))+ 
  theme(plot.title = element_text(hjust = 0.5)) +theme(plot.title = 
                                                         element_text(size = 15, face = "bold"))+ 
  theme(legend.title = element_blank(), 
        legend.text = element_text(color = "black",size = 10,face = 
                                     "bold"))+theme(legend.position="none") 

write.csv(trainingset, "trainingset.csv", row.names = FALSE)
write.csv(testset, "testset.csv", row.names = FALSE)
ggsave("datasplit.tiff", units="in", width=6, height=6, dpi=600)