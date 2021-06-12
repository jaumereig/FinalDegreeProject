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
###### READMISSION_hh_30 ##### entrada hdom
##############################

#Load data
dataset <- read.csv( "~/Escritorio/Internship/HDOM_09_18/Generated_Files/dataset_imputed.csv")
head(dataset)
#attach(dataset)
#remove variables to predict
#colnames(dataset)
predicted_v <- dataset[,c ("mort_hh_30", "readm_hh_30", "readm_30", "mort_30")]
#dataset<-dataset[ , !(names(dataset) %in% c("mort_hh_30", 'readm_hh_30', "readm_30", "mort_30"))]
dataset<-dataset[ , !(names(dataset) %in% c("mort_hh_30", 'readm_30', "Episodi", "mort_30"))]
results <- data.frame(Sensitivity = rep(NA,10),Specificity = rep(NA,10),
                      Score = rep(NA, 10),AUC = rep(NA, 10))
#Remove information: treatments during and after, outcomes after, stays, EAP(same info as in ABS_new), UCIAS_30 (47)
#dataset<-dataset[,-c('DAY_HCP','dias_hdm','dias_tot','EQUI_HD','EQUI_ALT','atb_ev','furo_ev','cort_ev',
# 'trat_ev','HEPARINA','curas','ESPIRO','GASO','ing_hd','MORT_DUR','mort_30',
# 'SEGU_ALT','SEGUI_CA','EAP','UCIAS_30','ucias_hd')]
#remove_variables<-dataset[, -c("days_HH", "days_tot", "days_hospital", "equip_HH", "equip_dis", "atb_ev", 
#                               "furo_ev", "cort_ev", "treat_ev", "heparin", "cures", "spirometry", "gasometry",
#                               "inh_day", "followup_dis", "followup_ca", "emerg_HH", "adm_HH")]
dataset<-dataset[,-c(5,6,7,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,37,38,39,40,41,42,44,45)]

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
  h<-holdout(dataset$readm_hh_30,ratio=.70,mode="stratified")
  data_train <- dataset[h$tr,] # train
  data_test  <- dataset[h$ts,] # test
  
  print(table(data_train$readm_hh_30))
  print(table(data_test$readm_hh_30))
  
  ##Random forest, random under-sampling
  set.seed(1234) 
  ctrl <- caret::trainControl(method = "repeatedcv", number = 4, repeats=10, 
                              savePredictions = TRUE,classProbs = TRUE, sampling="down")
  
  #mod_rf <- caret::train(readm_hh_30~., data=data_train, method="rf", trControl = ctrl, metric="Kappa")
  mod_rf <- caret::train(readm_hh_30~., data=data_train, method="rf",ntree=500, trControl = ctrl,metric="Kappa")
  
  #mod_rf <- caret::train(readm_30~., data=data_train, method="rf", trControl = ctrl, preProc=c("center","scale"),metric="Kappa")
  #varImp(mod_rf)
  
  
  # Test the new model on new and unseen Data for reproducibility
  pred = predict(mod_rf, newdata=data_test)
  data_test$readm_hh_30<-as.factor(data_test$readm_hh_30)
  cm_under<-confusionMatrix(pred,data_test$readm_hh_30,positive="si")
  #draw_confusion_matrix(cm_under)
  test_roc <- function(model, data) {
    roc(data$readm_hh_30, predict(model, data, type = "prob")[, "si"])
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
mod_readm_hh_hdom <- mod_rf
pred_readm_hh_hdom <- pred
#HDOM + FUNCTIONAL
mod_readm_hh_hdom_funct <- mod_rf
pred_readm_hh_hdom_funct <- pred
#HDOM + POPULATION
mod_readm_hh_hdom_pop <- mod_rf
pred_readm_hh_hdom_pop <- pred
#ALL
mod_readm_hh <- mod_rf
pred_readm_hh <- pred

plot_readm_hh <- varImp(mod_readm_hh)
plot_readm_hh <- plot_readm_hh[["importance"]]

df.plot_readm_hh <- plot_readm_hh %>%
  arrange(desc(Overall)) %>%
  slice(1:20)

names <- c( "GMA", "RDW", "Cr", "Leu", "Lym", "p_state", "charlson", "Hb", "m_state", "BMI", "age", "Glu", "K", 
            "n_diag", "adm_pre", "Na", "barthel", "emerg_pre", "year_cat", "Cstrat_risc_baix")
df.plot_readm_hh <- cbind(names, df.plot_readm_hh)
space <- c(0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4, 0.4)
#par(mfrow=c(1,2))
### HORIZONTAL PLOT
df.plot_readm_hh <- df.plot_readm_hh[seq(dim(df.plot_readm_hh)[1],1),] #rotate for horizontal plot
par(mar = c(4.5, 4.5, 2.5, 1.5)) #bottom,left, top, right
bp <- barplot(height = df.plot_readm_hh$Overall, horiz = TRUE, las = 1, space=space,
              main = "RM1 - 20 Most Important Variables", xlab = "Percentage")
rounded <- round(df.plot_readm_hh$Overall, 2)
text(x = df.plot_readm_hh$Overall, y = bp, label = rounded, pos = 2, cex = 1, col = "black")
yax <- axis(2, at=bp, labels=df.plot_readm_hh$names, tick = FALSE, las=2, 
            line=-1, cex.axis = 0.8)

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

###### ROC Plot
# 4 DATA SOURCES
par(mfrow=c(2,2))
roc.readm_hh_30 <- roc(data_test$readm_hh_30, predict(mod_readm_hh, data_test, type = "prob")[, "si"])
lines(roc.readm_hh_30, col = "red")
roc.hdom_readm_hh <- roc(data_test$readm_hh_30, predict(mod_readm_hh_hdom, data_test, type = "prob")[, "si"])
plot(roc.hdom_readm_hh, col = "blue")
roc.funct_readm_hh <- roc(data_test$readm_hh_30, predict(mod_readm_hh_hdom_funct, data_test, type = "prob")[, "si"])
lines(roc.funct_readm_hh, col = "magenta")
roc.pop_readm_hh <- roc(data_test$readm_hh_30, predict(mod_readm_hh_hdom_pop, data_test, type = "prob")[, "si"])
lines(roc.pop_readm_hh, col = "goldenrod4")
title(main = "RM1", line = 2.5)
legend(0.25, 0.75, legend = c("Clinical", "Clinical + Biological","Clinical+Population","All data"),
       col = c("blue", "black", "green", "red"), lty = 1:1, cex = 2, box.lty = 0)

# 3 ALGORITHMS
roc.rf_readm_hh_DT <- roc(data_test$readm_hh_30, predict(mod_readm_hh_DT, data_test, type = "prob")[, "si"])
roc.mod_readm_hh_LR <- roc(data_test$readm_hh_30, predict(mod_readm_hh_LR, data_test, type = "prob")[, "si"])
plot(roc.readm_hh_30, col = "red")
lines(roc.rf_readm_hh_DT, col = "blue")
lines(roc.mod_readm_hh_LR, col = "goldenrod4")
