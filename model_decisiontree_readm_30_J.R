library(foreign)
library(smotefamily)
library(spFSR)
library(rminer)
library(corrplot)
library(dplyr)
library(tidyr)
library(mlbench)
library(pROC)
library(purrr)
library(fpc)
library(ggraph)
library(igraph)
library(caret)
library(rpart.plot)
##############################
#### MODEL DECISION TREE #####
####### READMISSION_30 ####### despres
##############################

#Load data
dataset <- read.csv( "~/Escritorio/Internship/HDOM_09_18/Generated_Files/dataset_imputed.csv")
head(dataset)
#attach(dataset)
#remove variables to predict
#colnames(dataset)
predicted_v <- dataset[,c ("mort_hh_30", "readm_hh_30", "readm_30", "mort_30")]
#dataset<-dataset[ , !(names(dataset) %in% c("mort_hh_30", 'readm_hh_30', "readm_30", "mort_30"))]
dataset<-dataset[ , !(names(dataset) %in% c("mort_hh_30", 'readm_hh_30', "entr_g", "mort_30"))]
results <- data.frame(Sensitivity = rep(NA,10),Specificity = rep(NA,10),
                      Score = rep(NA, 10),AUC = rep(NA, 10))

### Data partitioning
for (i in 1:10){
  set.seed(73*i)
  h<-holdout(dataset$readm_30,ratio=.70,mode="stratified")
  data_train <- dataset[h$tr,] # train
  data_test  <- dataset[h$ts,] # test
  
  print(table(data_train$readm_30))
  print(table(data_test$readm_30))
  
  ##Decision tree
  set.seed(1234) 
  ctrl <- caret::trainControl(method = "repeatedcv", number = 4, repeats=10, 
                              savePredictions = TRUE,classProbs = TRUE, sampling="down")
  
  #mod_rf <- caret::train(readm_30~., data=data_train, method="rpart",ntree=500, trControl = ctrl,metric="Kappa")
  mod_rf <- caret::train(readm_30~., data=data_train, method="rpart", trControl = ctrl,metric="Kappa")
  
  #mod_rf <- caret::train(readm_30~., data=data_train, method="rf", trControl = ctrl, preProc=c("center","scale"),metric="Kappa")
  #varImp(mod_rf)
  
  
  # Test the new model on new and unseen Data for reproducibility
  pred = predict(mod_rf, newdata=data_test)
  data_test$readm_30<-as.factor(data_test$readm_30)
  cm_under<-confusionMatrix(pred,data_test$readm_30,positive="si")
  #draw_confusion_matrix(cm_under)
  test_roc <- function(model, data) {
    roc(data$readm_30, predict(model, data, type = "prob")[, "si"])
  }
  mod_rf %>% test_roc(data = data_test) %>% auc()
  
  results$Sensitivity[i]<-cm_under$byClass["Sensitivity"]
  results$Specificity[i]<-cm_under$byClass["Specificity"]
  results$Score[i]<-(cm_under$byClass["Sensitivity"]+cm_under$byClass["Specificity"])/2
  results$AUC[i]<-mod_rf %>% test_roc(data = data_test) %>% auc()
  
  #tree_num <- which(mod_rf$finalModel$forest$ndbigtree == median(mod_rf$finalModel$forest$ndbigtree))[1]
  #tree_func(final_model = mod_rf$finalModel, tree_num)
}

mean(results$AUC)
mean(results$Sensitivity)
mean(results$Specificity)
mean(results$Score)

sd(results$AUC)
sd(results$Sensitivity)
sd(results$Specificity)
sd(results$Score)

max(results$AUC)

#ALL
mod_readm_30_DT <- mod_rf
pred_readm_30_DT <- pred

rpart.plot(mod_rf$finalModel)

tree_num <- which(mod_rf$finalModel$forest$ndbigtree == median(mod_rf$finalModel$forest$ndbigtree))[1]
tree_func(final_model = mod_rf$finalModel, tree_num)


tree_func <- function(final_model =  mod_rf$finalModel, 
                      tree_num) {
  
  # get tree by index
  tree <- randomForest::getTree(mod_rf$finalModel, 
                                k = tree_num, 
                                labelVar = TRUE) %>%
    tibble::rownames_to_column() %>%
    # make leaf split points to NA, so the 0s won't get plotted
    mutate(`split point` = ifelse(is.na(prediction), `split point`, NA))
  
  # prepare data frame for graph
  graph_frame <- data.frame(from = rep(tree$rowname, 2),
                            to = c(tree$`left daughter`, tree$`right daughter`))
  
  # convert to graph and delete the last node that we don't want to plot
  graph <- graph_from_data_frame(graph_frame) %>%
    delete_vertices("0")
  
  # set node labels
  V(graph)$node_label <- gsub("_", " ", as.character(tree$`split var`))
  V(graph)$leaf_label <- as.character(tree$prediction)
  V(graph)$split <- as.character(round(tree$`split point`, digits = 2))
  
  # plot
  plot <- ggraph(graph, 'dendrogram') + 
    theme_bw() +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = node_label), na.rm = TRUE, repel = TRUE) +
    geom_node_label(aes(label = split), vjust = 2.5, na.rm = TRUE, fill = "white") +
    geom_node_label(aes(label = leaf_label, fill = leaf_label), na.rm = TRUE, 
                    repel = TRUE, colour = "white", fontface = "bold", show.legend = FALSE) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.background = element_rect(fill = "white"),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 18))
  
  print(plot)
}


draw_confusion_matrix <- function(cm_under) {
  
  total <- sum(cm_under$table)
  res <- as.numeric(cm_under$table)
  
  # Generate color gradients. Palettes come from RColorBrewer.
  greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
  redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  getColor <- function (greenOrRed = "green", amount = 0) {
    if (amount == 0)
      return("#FFFFFF")
    palette <- greenPalette
    if (greenOrRed == "red")
      palette <- redPalette
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
  }
  
  # set the basic layout
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  classes = colnames(cm$table)
  rect(150, 430, 240, 370, col=getColor("green", res[1]))
  text(195, 435, classes[1], cex=1.2)
  rect(250, 430, 340, 370, col=getColor("red", res[3]))
  text(295, 435, classes[2], cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=getColor("red", res[2]))
  rect(250, 305, 340, 365, col=getColor("green", res[4]))
  text(140, 400, classes[1], cex=1.2, srt=90)
  text(140, 335, classes[2], cex=1.2, srt=90)
  
  # add in the cm results
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm_under$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm_under$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm_under$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm_under$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm_under$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm_under$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm_under$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm_under$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm_under$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm_under$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm_under$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm_under$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm_under$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm_under$overall[2]), 3), cex=1.4)
}

test_roc <- function(model, data) {
  roc(data$emergency_room_30, predict(model, data, type = "prob")[, "Yes"])
}
