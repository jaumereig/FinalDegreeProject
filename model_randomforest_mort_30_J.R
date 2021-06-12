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
##############################
#### MODEL RANDOM FOREST #####
####    MORTALITY_30     ##### despres
##############################

#Load data
rm(dataset, data_test, data_train, mod_rf, cm_under, results, ctrl, h, predicted_v)
dataset <- read.csv( "~/Escritorio/Internship/HDOM_09_18/Generated_Files/dataset_imputed.csv")
head(dataset)
#attach(dataset)
#remove variables to predict
#colnames(dataset)
predicted_v <- dataset[,c ("mort_hh_30", "readm_hh_30", "readm_30", "mort_30")]
#dataset<-dataset[ , !(names(dataset) %in% c("mort_hh_30", 'readm_hh_30', "readm_30", "mort_30"))]
dataset<-dataset[ , !(names(dataset) %in% c("mort_hh_30", 'readm_hh_30', "Episodi", "readm_30"))]
results <- data.frame(Sensitivity = rep(NA,10),Specificity = rep(NA,10),
                      Score = rep(NA, 10),AUC = rep(NA, 10))

##################
# ONLY HDOM DATA #
##################
# remove functional/frailty data and GMA data
variables.to.remove <- dataset[,c("Leu", "Lym", "Hb", "RDW", "Glu", "Cr", "Na", "K", 
                                  "GMA_cat", "GMA", "Pstrat", "Cstrat")]
dataset<-dataset[ , !(names(dataset) %in% c("Leu", "Lym", "Hb", "RDW", "Glu", "Cr", "Na", "K", 
                                            "GMA_cat", "GMA", "Pstrat", "Cstrat"))]
##########################
# HDOM + FUNCTIONAL DATA #
##########################
# remove GMA data
variables.to.remove <- dataset[,c( "GMA_cat", "GMA", "Pstrat", "Cstrat")]
dataset<-dataset[ , !(names(dataset) %in% c("GMA_cat", "GMA", "Pstrat", "Cstrat"))]

##########################
# HDOM + POPULATION DATA #
##########################
# remove functional/frailty data
variables.to.remove <- dataset[,c("Leu", "Lym", "Hb", "RDW", "Glu", "Cr", "Na", "K")]
dataset<-dataset[ , !(names(dataset) %in% c("Leu", "Lym", "Hb", "RDW", "Glu", "Cr", "Na", "K"))]

### Data partitioning
for (i in 1:10){
  set.seed(73*i)
  h<-holdout(dataset$mort_30,ratio=.70,mode="stratified")
  data_train <- dataset[h$tr,] # train
  data_test  <- dataset[h$ts,] # test
  
  print(table(data_train$mort_30))
  print(table(data_test$mort_30))
  
  ##Random forest, random under-sampling
  set.seed(1234) 
  ctrl <- caret::trainControl(method = "repeatedcv", number = 4, repeats=10, 
                              savePredictions = TRUE,classProbs = TRUE, sampling="down")
  
  #mod_rf <- caret::train(readm_30~., data=data_train, method="rf", trControl = ctrl,metric="Kappa")
  mod_rf <- caret::train(mort_30~., data=data_train, method="rf",ntree=500, trControl = ctrl,metric="Kappa")
  
  #mod_rf <- caret::train(readm_30~., data=data_train, method="rf", trControl = ctrl, preProc=c("center","scale"),metric="Kappa")
  #varImp(mod_rf)
  
  
  # Test the new model on new and unseen Data for reproducibility
  pred = predict(mod_rf, newdata=data_test)
  data_test$mort_30<-as.factor(data_test$mort_30)
  cm_under<-confusionMatrix(pred,data_test$mort_30,positive="si")
  #draw_confusion_matrix(cm_under)
  test_roc <- function(model, data) {
    roc(data$mort_30, predict(model, data, type = "prob")[, "si"])
  }
  mod_rf %>% test_roc(data = data_test) %>% auc()
  
  results$Sensitivity[i]<-cm_under$byClass["Sensitivity"]
  results$Specificity[i]<-cm_under$byClass["Specificity"]
  results$Score[i]<-(cm_under$byClass["Sensitivity"]+cm_under$byClass["Specificity"])/2
  results$AUC[i]<-mod_rf %>% test_roc(data = data_test) %>% auc()
  
  #tree_num <- which(mod_rf$finalModel$forest$ndbigtree == median(mod_rf$finalModel$forest$ndbigtree))[1]
  #tree_func(final_model = mod_rf$finalModel, tree_num)
}

#ONLY HDOM
mod_mort_30_hdom <- mod_rf
pred_mort_30_hdom <- pred
#HDOM + FUNCTIONAL
mod_mort_30_hdom_funct <- mod_rf
pred_mort_30_hdom_funct <- pred
#HDOM + POPULATION
mod_mort_30_hdom_pop <- mod_rf
pred_mort_30_hdom_pop <- pred
#ALL
mod_mort_30 <- mod_rf
pred_mort_30 <- pred

plot_mort <- varImp(mod_mort_30)
plot_mort <- plot_mort[["importance"]]

df.plot_mort <- plot_mort %>%
  arrange(desc(Overall)) %>%
  slice(1:20)
names <- c("m_state", "adm_HH_si", "p_state", "year_cat", "barthel", "RDW", "Lym", "Na", "Glu",
           "Cr", "follow_dis_nadie", "walk_si", "charlson", "pills_day", "age", "GMA", "days_tot",
           "days_HH", "follow_pre_nadie", "vis_nurs")
df.plot_mort <- cbind(names, df.plot_mort)
space <- c(0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4)
#par(mfrow=c(1,2))
### HORIZONTAL PLOT
df.plot_mort <- df.plot_mort[seq(dim(df.plot_mort)[1],1),] #rotate for horizontal plot
par(mar = c(4.5, 4.5, 2.5, 1.5)) #bottom,left, top, right
bp <- barplot(height = df.plot_mort$Overall, horiz = TRUE, las = 1, space=space,
              main = "RM4 - 20 Most Important Variables", xlab = "Percentage")
rounded <- round(df.plot_mort$Overall, 2)
text(x = df.plot_mort$Overall, y = bp, label = rounded, pos = 2, cex = 1, col = "black")
yax <- axis(2, at=bp, labels=df.plot_mort$names, tick = FALSE, las=2, 
            line=-1, cex.axis = 0.86)

mean(results$AUC)
mean(results$Sensitivity)
mean(results$Specificity)
mean(results$Score)

sd(results$AUC)
sd(results$Sensitivity)
sd(results$Specificity)
sd(results$Score)

max(results$AUC)

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




### ROC Plot
# fer el roc plot i en un mateix gràfic utilitzant 
# linies de diferents colors  posar (HDOM, HDOM+lab, HDOM + GMA, ALL)
#par(mfrow=c(1,2))
roc.mort.rf <- roc(data_test$mort_30, predict(mod_mort_30, data_test, type = "prob")[, "si"])
lines(roc.mort.rf, col = "red")
roc.mort.hdom <- roc(data_test$mort_30, predict(mod_mort_30_hdom, data_test, type = "prob")[, "si"])
plot(roc.mort.hdom, col = "blue")
roc.mort.funct <- roc(data_test$mort_30, predict(mod_mort_30_hdom_funct, data_test, type = "prob")[, "si"])
lines(roc.mort.funct, col = "magenta")
roc.mort.pop <- roc(data_test$mort_30, predict(mod_mort_30_hdom_pop, data_test, type = "prob")[, "si"])
lines(roc.mort.pop, col = "goldenrod4")
title(main = "RM4", line = 2.5)
# 3 algorithms
plot(roc.mort.rf, col = "red")
roc.rf_mort_DT <- roc(data_test$mort_30, predict(mod_mort_DT, data_test, type = "prob")[, "si"])
lines(roc.rf_mort_DT, col = "blue")
roc.mod_mort_LR <- roc(data_test$mort_30, predict(mod_mort_LR, data_test, type = "prob")[, "si"])
lines(roc.mod_mort_LR, col = "goldenrod4")
title("Algorithm Comparison", side = 3, line = -1, outer = TRUE)


###########################
pred_roc <- as.numeric(pred)
pROC_obj <- roc(data_test$mort_30, pred_roc,
                smoothed = TRUE,
                # arguments for ci
                ci=TRUE, ci.alpha=0.9, stratified=FALSE,
                # arguments for plot
                plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=FALSE,
                print.auc=TRUE, show.thres=TRUE)

sens.ci <- ci.se(pROC_obj)
plot(sens.ci, type="shape", col="lightblue")
## Warning in plot.ci.se(sens.ci, type = "shape", col = "lightblue"): Low
## definition shape.
plot(sens.ci, type="bars")

library("ROCR")
predicted <- prediction(as.numeric(pred), as.numeric(data_test$mort_30))
perf <- performance(predicted,"tpr","fpr")
plot(perf,colorize=FALSE)

library("PRROC")
PRROC_obj <- roc.curve(scores.class0 = as.numeric(pred), weights.class0=as.numeric(data_test$mort_30),
                       curve=TRUE)
plot(PRROC_obj)

library("precrec")
precrec_obj <- evalmod(scores = as.numeric(pred), labels = as.numeric(data_test$mort_30))
autoplot(precrec_obj)
