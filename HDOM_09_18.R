library("readxl")
library("tidyverse")
library("missForest")
library("naniar")
library("sjmisc")
library("foreign")
library("labelled")
library("rockchalk")

# LOAD HDOM DATABASE
HDOM_db <- read_xlsx("~/Escritorio/Internship/HDOM_09_18/Base de dades HDOM.xlsx")
head(HDOM_db)
# 1191 obs 72 variables
HDOM_old = read.spss("~/Escritorio/Internship/HDOM_09_18/HDOM 2006_15 Anonima_finalMC.sav", to.data.frame=TRUE,use.value.labels = FALSE)
HDOM_old<-remove_attributes(HDOM_old,"value.labels")


##Eliminate Episodes with multiple entries
HDOM_db<-HDOM_db[-c(which(HDOM_db$Episodi=="1005869044" & HDOM_db$`SERVEI que el proposa`=="1"),which(HDOM_db$Episodi=="1005955629" & HDOM_db$`SERVEI que el proposa`=="1"),which(HDOM_db$Episodi=="1005971068" & HDOM_db$ID=="859")),]

#LOAD SAP DATABASE
SAP_db <- read_xlsx("~/Escritorio/Internship/HDOM_09_18/Base de dades SAP.xlsx", sheet = "Lab")
head(SAP_db)
colnames(SAP_db)[3] <- "Episodi"
# 495183 obs  20  variables
SAP_old=read.delim("~/Escritorio/Internship/HDOM_09_18//Analitics_data_Atomian.txt", header = TRUE, sep = "\t", dec = ".")


#LOAD GMA DATABASE
GMA_expense_db <- read.csv2("~/Escritorio/Internship/HDOM_09_18/GMA i despesa HDOM.csv",dec=".")
head(GMA_expense_db)
colnames(GMA_expense_db)[2] <- "Episodi"
# 10407 obs 8 variables
GMA_old<-read_xlsx("~/Escritorio/Internship/HDOM_09_18/GMA.xlsx")

#load diagnosis ICD10 GEMs
db_path<-"~/Escritorio/Internship/HDOM_09_18/2018_I10gem.txt"
diag_ICD10<-read.table(db_path,header=FALSE)
diag_ICD10<-diag_ICD10[,-3]
colnames(diag_ICD10)<-c("icd10","icd9")

#load diagnosis ICD9 GEMs
db_path<-"~/Escritorio/Internship/HDOM_09_18/2018_I9gem.txt"
diag_ICD9<-read.table(db_path,header=FALSE)
diag_ICD9<-diag_ICD9[,-3]
colnames(diag_ICD9)<-c("icd9","icd10")

####################
## EXTRACT LAB DATA
####################


lab_list<-c("LAB1300","LAB1308","LAB1314","LAB1323","LAB2422","LAB2467","LAB2507","LAB2508")
dataset<-filter(SAP_db, Prestación %in% lab_list)
dataset$Valor<-as.numeric(dataset$Valor)
dataset<-dataset[!is.na(dataset$Valor),]
# 74355 obs 20 variables

##Remove duplicates
dataset <- dataset[!duplicated(dataset),]


##Ordenar per Episodi, data i hora i quedar-nos amb la primera analitica de l'episodi
dataset<-dataset[order(dataset$Episodi,dataset$Fecha,dataset$Hora),]
dataset<-dataset[!duplicated(dataset[,c("Episodi","Prestación")]),]
# 48078 obs 20 variables


#Keep only the desired columns
cols_wanted <- c("Episodi","Valor","Prestación")
dataset <- dataset[, names(dataset) %in% cols_wanted]
# 48078 obs 3 variables

##Rows to columns
dataset<-spread(dataset, Prestación, Valor)
#6121 obs 9 variables

SAP_old<-SAP_old[,append("ID",lab_list)]
names(SAP_old)[1]<-"Episodi"
dataset<-rbind(dataset,SAP_old)
#8072 obs 9 variables

##Rename columns
names(dataset)[names(dataset) == "LAB1300"] <- "Leu"
dataset$Leu<-as.numeric(dataset$Leu)
names(dataset)[names(dataset) == "LAB1308"] <- "Lym"
dataset$Lym<-as.numeric(dataset$Lym)
names(dataset)[names(dataset) == "LAB1314"] <- "Hb"
dataset$Hb<-as.numeric(dataset$Hb)
names(dataset)[names(dataset) == "LAB1323"] <- "RDW"
dataset$RDW<-as.numeric(dataset$RDW)
names(dataset)[names(dataset) == "LAB2422"] <- "Glu"
dataset$Glu<-as.numeric(dataset$Glu)
names(dataset)[names(dataset) == "LAB2467"] <- "Cr"
dataset$Cr<-as.numeric(dataset$Cr)
names(dataset)[names(dataset) == "LAB2507"] <- "Na"
dataset$Na<-as.numeric(dataset$Na)
names(dataset)[names(dataset) == "LAB2508"] <- "K"
dataset$K<-as.numeric(dataset$K)


####################
# EXTRACT GMA DATA
###################

dataset2<-GMA_expense_db

## Eliminate NAs
dataset2<-dataset2[!is.na (dataset2$GMA),]
GMA_old<-GMA_old[!is.na (GMA_old$Peso),]

dataset2<-dataset2[,c("Episodi","GMA","GMA_Pes")]
GMA_old<-GMA_old[,c("ID","GMA","Peso")]
names(GMA_old)<-c("Episodi","GMA","GMA_Pes")

dataset2<-rbind(dataset2,GMA_old)

#Rediscretitzaci? de categories de GMA amb poca prevaleNºa a ALTRES
GMA_new<-seq(0,0,length.out=nrow(dataset2))
GMA_new<-factor(GMA_new)
dataset2<-cbind(dataset2,GMA_new)
levels(dataset2$GMA_new)<-c("Altres","311", "312", "313", "314", "315", "321", "322", "323", "324", "325", "331","332", "333", "334", "335", "401", "402", "403", "404", "405")
for (i in 1:nrow(dataset2)){
  if(dataset2$GMA[i]=="1" | dataset2$GMA[i]=="101" | dataset2$GMA[i]=="102" | dataset2$GMA[i]=="103" | dataset2$GMA[i]=="104" | dataset2$GMA[i]=="105" | dataset2$GMA[i]=="201" | dataset2$GMA[i]=="202" | dataset2$GMA[i]=="203" | dataset2$GMA[i]=="204" | dataset2$GMA[i]=="205"){
    dataset2$GMA_new[i]<-"Altres"
  } else{
    dataset2$GMA_new[i]<-dataset2$GMA[i]
  }
}
dataset2$GMA_new<-as.factor(dataset2$GMA_new)

dataset2<-dataset2[,c("Episodi","GMA_new","GMA_Pes")]
##Rename variables
names(dataset2)[names(dataset2) == "GMA_new"] <- "GMA_cat"
names(dataset2)[names(dataset2) == "GMA_Pes"] <- "GMA"
# names(dataset2)[names(dataset2) == "nING"] <- "adm_pre"
# names(dataset2)[names(dataset2) == "DespesaPrevia"] <- "health_expenses"



##Ordenar per Episodi, i numero de ingressos, en ordre decreixent ens quedem l'ultima entrada
# dataset2<-dataset2[order(dataset2$Episodi,dataset2$n_admissions,decreasing = c(FALSE,TRUE)),]
dataset2<-dataset2[!duplicated(dataset2[,"Episodi"]),]
#13082 obs 3 variables


####################################
#### MERGE DATSETS
###################################

#CHECK if there are duplicated data entries



table(duplicated(dataset$Episodi))
table(duplicated(dataset2$Episodi))
table(duplicated(HDOM_db$Episodi))

HDOM_db <- HDOM_db[, ! names(HDOM_db) %in% c("NHC","cip","ID",">1 comorb crònica\r\n","Data adm hospit","Data ingrés HDOM","Data alta HDOM"), drop = F]
colnames<-colnames(HDOM_db)


HDOM_old<-HDOM_old[,c("ID_PAC","SEX","AGE","abs","p_entr","SERVICE","hh_ed","DAY_HCP",
"dias_hdm","dias_tot","ano_cat","diag_cat","icd9_ing","N_diag","charlson",
"tabaquis","imc","imc_cat","camina","barthel","barth_cat","sf_36","estad_fi","estad_me",
"EQUI_PRE","TECN_DOM","EQUI_HD","EQUI_ALT","dif_medi","past_dia","iny_dia","inh_diar",
"atb_ev","furo_ev","cort_ev","trat_ev","HEPARINA","curas","ESPIRO","GASO","ANALI_IN",
"nom_pato","sig_alar","ucias_an","U_REL_PR","ing_ano","I_REL_PR","ucias_hd","ing_hd",
"MORT_DUR","UCIAS_30","UCIAS_60","UCIAS_90","ING_30","ING_60","ING_90","mort_alt",
"VIS_NURS","VIS_DOC","VIS_HCB","CALL","PROFESIO","SEGU_PRE","SEGU_ALT","SEGUI_CA")]
colnames(HDOM_old)<-colnames

table(duplicated(HDOM_old$Episodi))

HDOM_db<-rbind(HDOM_db,HDOM_old)

dataset_merged<-merge(HDOM_db,dataset, by="Episodi")
dataset_merged<-merge(dataset_merged,dataset2, by="Episodi")

dataset<-dataset_merged
#2522 obs 75 variables
rm(dataset_merged, dataset2, GMA_expense_db, hdom, HDOM_db,SAP_db, GMA_old, HDOM_old, SAP_old)


######################################
## CLEAN DATASET
#####################################

dataset2<-dataset
dataset<-dataset2

#Create variables ox_pre and vent_pre
ox_pre<-seq(0,0,length.out=nrow(dataset))
vent_pre<-seq(0,0,length.out=nrow(dataset))
dataset<-cbind(dataset,ox_pre,vent_pre)


## Decode the categorical variables
for (i in 1:nrow(dataset)) {
  
  if (!is.na(dataset$Gènere[i])){
    if (dataset$Gènere[i]== 0) {
      dataset$Gènere[i]<- "male"
    } else if (dataset$Gènere[i]== 1) {
      dataset$Gènere[i]<- "female"
    }
  }    
  
###Tots els pacients venen de urg?ncies
  if (!is.na(dataset$'Porta entrada'[i])){
    if (dataset$'Porta entrada'[i]==1){
      dataset$'Porta entrada'[i]<-"Urgencias"
    } else if (dataset$'Porta entrada'[i]==2){
      dataset$'Porta entrada'[i]<-"Hospital de dia"
    } else if (dataset$'Porta entrada'[i]==3){
      dataset$'Porta entrada'[i]<-"Ingreso electivo"
    } else if (dataset$'Porta entrada'[i]==4){
      dataset$'Porta entrada'[i]<-"Fragiles"
    } else if (dataset$'Porta entrada'[i]==5){
      dataset$'Porta entrada'[i]<-"Otros"
    } else {
      dataset$'Porta entrada'[i]<-NA
    }
   
  }
  
  if (!is.na(dataset$'SERVEI que el proposa'[i])){
    if(dataset$'SERVEI que el proposa'[i]==1){
      dataset$'SERVEI que el proposa'[i]<-"URGENCIAS"
    } else if(dataset$'SERVEI que el proposa'[i]==2){
      dataset$'SERVEI que el proposa'[i]<-"CIRUGIA CARDIOVASCULAR"
    } else if(dataset$'SERVEI que el proposa'[i]==4){
      dataset$'SERVEI que el proposa'[i]<-"COT"
    } else if(dataset$'SERVEI que el proposa'[i]==5){
      dataset$'SERVEI que el proposa'[i]<-"NEUMOLOGIA"
    } else if(dataset$'SERVEI que el proposa'[i]==6){
      dataset$'SERVEI que el proposa'[i]<-"CARDIO"
    } else if(dataset$'SERVEI que el proposa'[i]==8){
      dataset$'SERVEI que el proposa'[i]<-"MDI"
    } else if(dataset$'SERVEI que el proposa'[i]==12){
      dataset$'SERVEI que el proposa'[i]<-"CIR. GENERAL"
    } else if(dataset$'SERVEI que el proposa'[i]==16){
      dataset$'SERVEI que el proposa'[i]<-"RESIDENCIA"
    } else if(dataset$'SERVEI que el proposa'[i]==17 || dataset$'SERVEI que el proposa'[i]==33 || dataset$'SERVEI que el proposa'[i]==37 || dataset$'SERVEI que el proposa'[i]==38){
      dataset$'SERVEI que el proposa'[i]<-"OTROS"
    } else if(dataset$'SERVEI que el proposa'[i]==18){
      dataset$'SERVEI que el proposa'[i]<-"UAI"
    } else if(dataset$'SERVEI que el proposa'[i]==32){
      dataset$'SERVEI que el proposa'[i]<-"ONCO"
    } else if(dataset$'SERVEI que el proposa'[i]==34 || dataset$'SERVEI que el proposa'[i]==44){
      dataset$'SERVEI que el proposa'[i]<-"URO/NEFRO"
    } else if(dataset$'SERVEI que el proposa'[i]==35){
      dataset$'SERVEI que el proposa'[i]<-"CTR"
    } else if(dataset$'SERVEI que el proposa'[i]==39){
      dataset$'SERVEI que el proposa'[i]<-"HEMATO"
    } else if(dataset$'SERVEI que el proposa'[i]==40){
        dataset$'SERVEI que el proposa'[i]<-"HEPATO"
    } else if(dataset$'SERVEI que el proposa'[i]==41){
      dataset$'SERVEI que el proposa'[i]<-"INFECCIONES"
    } else if(dataset$'SERVEI que el proposa'[i]==42){
        dataset$'SERVEI que el proposa'[i]<-"CIR. NEURO"
    } else if(dataset$'SERVEI que el proposa'[i]==43){
        dataset$'SERVEI que el proposa'[i]<-"OBSTETRICIA"
    } else if(dataset$'SERVEI que el proposa'[i]==45){
        dataset$'SERVEI que el proposa'[i]<-"MAS"
    } else if(dataset$'SERVEI que el proposa'[i]==46){
        dataset$'SERVEI que el proposa'[i]<-"PSIQUIATRIA"
    } else if(dataset$'SERVEI que el proposa'[i]==47){
        dataset$'SERVEI que el proposa'[i]<-"BARNACLINIC"
    } else if(dataset$'SERVEI que el proposa'[i]==48){
        dataset$'SERVEI que el proposa'[i]<-"CIR. PLASTICA"
    } else if(dataset$'SERVEI que el proposa'[i]==49){
        dataset$'SERVEI que el proposa'[i]<-"ALTRES HOSPITALS"
    } else if(dataset$'SERVEI que el proposa'[i]==999){
      dataset$'SERVEI que el proposa'[i]<-NA
    } else {
      dataset$'SERVEI que el proposa'[i]<-"OTROS"
    }
  }
  
  if (!is.na(dataset$'any alta hdom'[i])){
    dataset$'any alta hdom'[i]<-dataset$'any alta hdom'[i]+2005
  }
  
  
  if (!is.na(dataset$'N comorbilidades'[i])){
    if(dataset$'N comorbilidades'[i]==999){
      dataset$'N comorbilidades'[i]<-NA
    }
  }
  
  if (!is.na(dataset$`Índex CHARLSON`[i])){
    if(dataset$`Índex CHARLSON`[i]==999){
      dataset$`Índex CHARLSON`[i]<-NA
    }
  }
  
  if (!is.na(dataset$Tabaquisme[i])){
    if(dataset$Tabaquisme[i]==0){
      dataset$Tabaquisme[i]<- "none smoking"
    } else if (dataset$Tabaquisme[i]== 1) {
      dataset$Tabaquisme[i]<- "active smoker"
    }else if (dataset$Tabaquisme[i]== 2) {
      dataset$Tabaquisme[i]<- "ex-smoker"
    }else{
      dataset$Tabaquisme[i]<-NA
    }
  }
  
  if (!is.na(dataset$IMC[i])){
    if(dataset$IMC[i]==999){
      dataset$IMC[i]<-NA
    }
  }
  
  if (!is.na(dataset$Camina[i])){
    if (dataset$Camina[i]=="0"){
      dataset$Camina[i]<-"no"
    }else if (dataset$Camina[i]=="1"){
      dataset$Camina[i]<-"si"
    }else if(dataset$Camina[i]==999){
      dataset$Camina[i]<-NA
    }
  }
  
  if (!is.na(dataset$`Índex Barthel`[i])){
    if(dataset$`Índex Barthel`[i]==999){
      dataset$`Índex Barthel`[i]<-NA
    }
  }
  
  if (!is.na(dataset$`Procedeix SF-36`[i])){
    if (dataset$`Procedeix SF-36`[i]=="0"){
      dataset$`Procedeix SF-36`[i]<-"no"
    }else if (dataset$`Procedeix SF-36`[i]=="1"){
      dataset$`Procedeix SF-36`[i]<-"si"
    }else if(dataset$`Procedeix SF-36`[i]==999){
      dataset$`Procedeix SF-36`[i]<-NA
    }
  }
  
  if (!is.na(dataset$`Procedeix SF-36`[i])){
    if(dataset$`Procedeix SF-36`[i]=="no"){
      dataset$'Estat mental'[i]<--1
      dataset$'Estat físic'[i]<--1
    }
  }
  if (!is.na(dataset$'Estat mental'[i])){
    if(dataset$'Estat mental'[i]==999){
      dataset$'Estat mental'[i]<-NA
    }    
  }
  if (!is.na(dataset$'Estat físic'[i])){
    if(dataset$'Estat físic'[i]==999){
      dataset$'Estat físic'[i]<-NA
    }    
  }
  
  


  if (!is.na(dataset$'Equips terapia respiratoria'[i])){
    if (dataset$'Equips terapia respiratoria'[i]==0){
      dataset$'Equips terapia respiratoria'[i]<-"ninguno"
    }else if(dataset$'Equips terapia respiratoria'[i]==1){
      dataset$'Equips terapia respiratoria'[i]<-"ocd"
    }else if(dataset$'Equips terapia respiratoria'[i]==2){
      dataset$'Equips terapia respiratoria'[i]<-"nebus"
    }else if(dataset$'Equips terapia respiratoria'[i]==3){
      dataset$'Equips terapia respiratoria'[i]<-"ocd + nebus"
    }else if(dataset$'Equips terapia respiratoria'[i]==4){
      dataset$'Equips terapia respiratoria'[i]<-"ocd/nebus + otros"
    }else if(dataset$'Equips terapia respiratoria'[i]==5){
      dataset$'Equips terapia respiratoria'[i]<-"cpap"
    }else if(dataset$'Equips terapia respiratoria'[i]==6){
      dataset$'Equips terapia respiratoria'[i]<-"bipap" 
    }else if(dataset$'Equips terapia respiratoria'[i]==7){
      dataset$'Equips terapia respiratoria'[i]<-"cpap + ocd"
    }else if(dataset$'Equips terapia respiratoria'[i]==8){
      dataset$'Equips terapia respiratoria'[i]<-"otros"
    }else if(dataset$'Equips terapia respiratoria'[i]==9){
      dataset$'Equips terapia respiratoria'[i]<-"bipap + ocd"
    }else if(dataset$'Equips terapia respiratoria'[i]==999){
      dataset$'Equips terapia respiratoria'[i]<-NA
    }
  }
  
  if (!is.na(dataset$'Nº Tècniques o cures'[i])){
    if(dataset$'Nº Tècniques o cures'[i]==999){
      dataset$'Nº Tècniques o cures'[i]<-NA
    }
  }
  
  #####################REVISAR SI CAL REDISCRETITZAR #####
  if (!is.na(dataset$'Equips terapia respiratoria'[i])){
    if(dataset$'Equips terapia respiratoria'[i]=="bipap" ||dataset$'Equips terapia respiratoria'[i]=="bipap + ocd"){
      dataset$vent_pre[i]<-"bipap"
    }else if(dataset$'Equips terapia respiratoria'[i]=="cpap" ||dataset$'Equips terapia respiratoria'[i]=="cpap + ocd"){
      dataset$vent_pre[i]<-"cpap"
    }else {
      dataset$vent_pre[i]<-"ninguno"
    }
  }
  
  if (!is.na(dataset$'Equips terapia respiratoria'[i])){
    if(dataset$'Equips terapia respiratoria'[i]=="ocd"||dataset$'Equips terapia respiratoria'[i]=="ocd + nebus"||dataset$'Equips terapia respiratoria'[i]=="ocd/nebus + otros"||dataset$'Equips terapia respiratoria'[i]=="cpap + ocd"||dataset$'Equips terapia respiratoria'[i]=="bipap + ocd"){
      dataset$ox_pre[i]<-"si"
    }else {
      dataset$ox_pre[i]<-"no"
    }
  }
  
  #####################REVISAR SI CAL REDISCRETITZAR #####  
  if (!is.na(dataset$`Es mantenen equips a l'alta`[i])){
    if (dataset$`Es mantenen equips a l'alta`[i]==0){
      dataset$`Es mantenen equips a l'alta`[i]<-"nada"
    }else if(dataset$`Es mantenen equips a l'alta`[i]==1){
      dataset$`Es mantenen equips a l'alta`[i]<-"ocd"
    }else if(dataset$`Es mantenen equips a l'alta`[i]==2){
      dataset$`Es mantenen equips a l'alta`[i]<-"nebus"
    }else if(dataset$`Es mantenen equips a l'alta`[i]==3){
      dataset$`Es mantenen equips a l'alta`[i]<-"ocd + nebus"
    }else if(dataset$`Es mantenen equips a l'alta`[i]==4){
      dataset$`Es mantenen equips a l'alta`[i]<-"ocd+ nebus + otros"
    }else if(dataset$`Es mantenen equips a l'alta`[i]==5){
      dataset$`Es mantenen equips a l'alta`[i]<-"otros"
    }else if(dataset$`Es mantenen equips a l'alta`[i]==999){
      dataset$`Es mantenen equips a l'alta`[i]<-NA
    }
  }
  
  if (!is.na(dataset$'Equips durant HD'[i])){
    if (dataset$'Equips durant HD'[i]==0){
      dataset$'Equips durant HD'[i]<-"nada"
    }else if(dataset$'Equips durant HD'[i]==1){
      dataset$'Equips durant HD'[i]<-"ocd"
    }else if(dataset$'Equips durant HD'[i]==2){
      dataset$'Equips durant HD'[i]<-"nebus"
    }else if(dataset$'Equips durant HD'[i]==3){
      dataset$'Equips durant HD'[i]<-"ocd + nebus"
    }else if(dataset$'Equips durant HD'[i]==4){
      dataset$'Equips durant HD'[i]<-"ocd+ nebus + otros"
    }else if(dataset$'Equips durant HD'[i]==5){
        dataset$'Equips durant HD'[i]<-"aspirador"
    }else if(dataset$'Equips durant HD'[i]==6){
        dataset$'Equips durant HD'[i]<-"medela"
    }else if(dataset$'Equips durant HD'[i]==7){
        dataset$'Equips durant HD'[i]<-"cura vac"
    }else if(dataset$'Equips durant HD'[i]==999){
      dataset$'Equips durant HD'[i]<-NA
    }else{
      dataset$'Equips durant HD'[i]<-"otros"
    }
  }
  
  if (!is.na(dataset$'Dificultat medicació'[i])){
    if (dataset$'Dificultat medicació'[i]=="0"){
      dataset$'Dificultat medicació'[i]<-"ninguna dificultad"
    }else if (dataset$'Dificultat medicació'[i]=="1"){
      dataset$'Dificultat medicació'[i]<-"presenta dificultad"
    }else if(dataset$'Dificultat medicació'[i]==999){
      dataset$'Dificultat medicació'[i]<-NA
    }
  }
  
  if (!is.na(dataset$'Nº Pastilles'[i])){
    if(dataset$'Nº Pastilles'[i]==999){
      dataset$'Nº Pastilles'[i]<-NA
    }
  }
  
  if (!is.na(dataset$'Nº Injeccions'[i])){
    if(dataset$'Nº Injeccions'[i]==999){
      dataset$'Nº Injeccions'[i]<-NA
    }
  }
  
  if (!is.na(dataset$'Nº Inhalacions'[i])){
    if(dataset$'Nº Inhalacions'[i]==999){
      dataset$'Nº Inhalacions'[i]<-NA
    }
  }
  
  
  #####REVISAR atb_ev == 4 & 5  
  if (!is.na(dataset$'ATB EV'[i])){
    if (dataset$'ATB EV'[i]=="0"){
      dataset$'ATB EV'[i]<-"no"
    }else if (dataset$'ATB EV'[i]=="1"){
      dataset$'ATB EV'[i]<-"si"
    }else if (dataset$'ATB EV'[i]=="2"){
      dataset$'ATB EV'[i]<-"Antibi?tico en bomba de perfusion"
    }else if (dataset$'ATB EV'[i]=="3"){
      dataset$'ATB EV'[i]<-"Otros tratamientos endovenosos"
    }else if(dataset$'ATB EV'[i]==999){
      dataset$'ATB EV'[i]<-NA
    } else {
      dataset$'ATB EV'[i]<-NA
    }
  }
  
  if (!is.na(dataset$'FURO EV'[i])){
    if (dataset$'FURO EV'[i]=="0"){
      dataset$'FURO EV'[i]<-"no"
    }else if (dataset$'FURO EV'[i]=="1"){
      dataset$'FURO EV'[i]<-"si"
    }else if(dataset$'FURO EV'[i]==999){
      dataset$'FURO EV'[i]<-NA
    }
  }
  
  if (!is.na(dataset$'CORTIS EV'[i])){
    if (dataset$'CORTIS EV'[i]=="0"){
      dataset$'CORTIS EV'[i]<-"no"
    }else if (dataset$'CORTIS EV'[i]=="1"){
      dataset$'CORTIS EV'[i]<-"si"
    }else if(dataset$'CORTIS EV'[i]==999){
      dataset$'CORTIS EV'[i]<-NA
    }
  }
  
  if (!is.na(dataset$'algun tto ev'[i])){
    if (dataset$'algun tto ev'[i]=="0"){
      dataset$'algun tto ev'[i]<-"no"
    }else if (dataset$'algun tto ev'[i]=="1"){
      dataset$'algun tto ev'[i]<-"si"
    }else if(dataset$'algun tto ev'[i]==999){
      dataset$'algun tto ev'[i]<-NA
    }
  }
  
  if (!is.na(dataset$'HBPM sc'[i])){
    if (dataset$'HBPM sc'[i]=="0"){
      dataset$'HBPM sc'[i]<-"no"
    }else if (dataset$'HBPM sc'[i]=="1"){
      dataset$'HBPM sc'[i]<-"si"
    }else if(dataset$'HBPM sc'[i]==999){
      dataset$'HBPM sc'[i]<-NA
    }
  }
  
  if (!is.na(dataset$cures[i])){
    if (dataset$cures[i]=="0"){
      dataset$cures[i]<-"no"
    }else if (dataset$cures[i]=="1"){
      dataset$cures[i]<-"si"
    }else if(dataset$cures[i]==999){
      dataset$cures[i]<-NA
    }
  }
  
  if (!is.na(dataset$'Es realitza EF'[i])){
    if (dataset$'Es realitza EF'[i]=="0"){
      dataset$'Es realitza EF'[i]<-"no"
    }else if (dataset$'Es realitza EF'[i]=="1"){
      dataset$'Es realitza EF'[i]<-"si"
    }else if(dataset$'Es realitza EF'[i]==999){
      dataset$'Es realitza EF'[i]<-NA
    }
  }
  
  if (!is.na(dataset$'Es realitza GSA'[i])){
    if (dataset$'Es realitza GSA'[i]=="0"){
      dataset$'Es realitza GSA'[i]<-"no"
    }else if (dataset$'Es realitza GSA'[i]=="1"){
      dataset$'Es realitza GSA'[i]<-"si"
    }else if(dataset$'Es realitza GSA'[i]==999){
      dataset$'Es realitza GSA'[i]<-NA
    }
  }
  
  if (!is.na(dataset$'Coneix nom patologia crònica'[i])){
    if (dataset$'Coneix nom patologia crònica'[i]=="0"){
      dataset$'Coneix nom patologia crònica'[i]<-"no"
    }else if (dataset$'Coneix nom patologia crònica'[i]=="1"){
      dataset$'Coneix nom patologia crònica'[i]<-"si"
    }else if(dataset$'Coneix nom patologia crònica'[i]==999){
      dataset$'Coneix nom patologia crònica'[i]<-NA
    }
  }
  
  if (!is.na(dataset$'Signes alarma'[i])){
    if (dataset$'Signes alarma'[i]=="0"){
      dataset$'Signes alarma'[i]<-"no"
    }else if (dataset$'Signes alarma'[i]=="1"){
      dataset$'Signes alarma'[i]<-"si"
    }else if(dataset$'Signes alarma'[i]==999){
      dataset$'Signes alarma'[i]<-NA
    } else {
      dataset$'Signes alarma'[i]<-NA
    }
  } 
  
  if (!is.na(dataset$'Nº Consultes urgencies últim any'[i])){
    if(dataset$'Nº Consultes urgencies últim any'[i]==999){
      dataset$'Nº Consultes urgencies últim any'[i]<-NA
    }
  }
  
  
  if (!is.na(dataset$'Urgencies durant HDOM'[i])){
    if (dataset$'Urgencies durant HDOM'[i]=="0"){
      dataset$'Urgencies durant HDOM'[i]<-"no"
    }else if (dataset$'Urgencies durant HDOM'[i]=="1"){
      dataset$'Urgencies durant HDOM'[i]<-"si"
    }else if(dataset$'Urgencies durant HDOM'[i]==999){
      dataset$'Urgencies durant HDOM'[i]<-NA
    }
  } 
  
  if (!is.na(dataset$'Reingrés durant HDOM'[i])){
    if (dataset$'Reingrés durant HDOM'[i]=="0"){
      dataset$'Reingrés durant HDOM'[i]<-"no"
    }else if(dataset$'Reingrés durant HDOM'[i]==999){
      dataset$'Reingrés durant HDOM'[i]<-NA
    }else {
      dataset$'Reingrés durant HDOM'[i]<-"si"
    }
  } 
  
  if (!is.na(dataset$Mortalitat[i])){
    if (dataset$Mortalitat[i]=="0"){
      dataset$Mortalitat[i]<-"no"
    }else if (dataset$Mortalitat[i]=="1"){
      dataset$Mortalitat[i]<-"si"
    }else if(dataset$Mortalitat[i]==999){
      dataset$Mortalitat[i]<-NA
    }
  } 
  dataset<-dataset[which(!is.na(dataset$Mortalitat)),]
  
  if (!is.na(dataset$'Reingressos als 30 dies'[i])){
    if (dataset$'Reingressos als 30 dies'[i]=="0"){
      dataset$'Reingressos als 30 dies'[i]<-"no"
    }else if (dataset$'Reingressos als 30 dies'[i]=="1"){
      dataset$'Reingressos als 30 dies'[i]<-"si"
    }else if(dataset$'Reingressos als 30 dies'[i]==999){
      dataset$'Reingressos als 30 dies'[i]<-NA
    }
  } 
  dataset<-dataset[which(!is.na(dataset$`Reingressos als 30 dies`)),]
  
  if (!is.na(dataset$'Mortalitat als 30 dies'[i])){
    if (dataset$'Mortalitat als 30 dies'[i]=="0" | dataset$'Mortalitat als 30 dies'[i]=="2" | dataset$'Mortalitat als 30 dies'[i]=="3"){
      dataset$'Mortalitat als 30 dies'[i]<-"no"
    }else if (dataset$'Mortalitat als 30 dies'[i]=="1"){
      dataset$'Mortalitat als 30 dies'[i]<-"si"
    }else if(dataset$'Mortalitat als 30 dies'[i]==999){
      dataset$'Mortalitat als 30 dies'[i]<-NA
    }
  } 
  dataset<-dataset[which(!is.na(dataset$`Mortalitat als 30 dies`)),]
  
  if (!is.na(dataset$'Visita infermeria'[i])){
    if(dataset$'Visita infermeria'[i]==999){
      dataset$'Visita infermeria'[i]<-NA
    }
  }
  
  if (!is.na(dataset$'Visita mèdica'[i])){
    if(dataset$'Visita mèdica'[i]==999){
      dataset$'Visita mèdica'[i]<-NA
    }
  }
  
  if (!is.na(dataset$'Visita HCP'[i])){
    if(dataset$'Visita HCP'[i]==999){
      dataset$'Visita HCP'[i]<-NA
    }
  }
  
  if (!is.na(dataset$`Trucada  professional`[i])){
    if(dataset$`Trucada  professional`[i]==999){
      dataset$`Trucada  professional`[i]<-NA
    }
  } 

  #####################REVISAR SI CAL REDISCRETITZAR #####    
  if (!is.na(dataset$'Seguimient previ'[i])){
    if (dataset$'Seguimient previ'[i]=="0"){
      dataset$'Seguimient previ'[i]<-"nadie"
    } else if (dataset$'Seguimient previ'[i]=="1"){
      dataset$'Seguimient previ'[i]<-"ATDOM"
    } else if (dataset$'Seguimient previ'[i]=="2"){
      dataset$'Seguimient previ'[i]<-"PADES"
    } else if (dataset$'Seguimient previ'[i]=="3"){
      dataset$'Seguimient previ'[i]<-"FRAGILES"
    } else if (dataset$'Seguimient previ'[i]=="4"){
      dataset$'Seguimient previ'[i]<-"CCEE"
    } else if (dataset$'Seguimient previ'[i]=="5"){
      dataset$'Seguimient previ'[i]<-"RAE"
    } else if (dataset$'Seguimient previ'[i]=="6"){
      dataset$'Seguimient previ'[i]<-"RH"
    } else if (dataset$'Seguimient previ'[i]=="7"){
      dataset$'Seguimient previ'[i]<-"AISBE grup 3"
    } else if (dataset$'Seguimient previ'[i]=="8"){
      dataset$'Seguimient previ'[i]<-"AISBE grup 1-2"
    } else if (dataset$'Seguimient previ'[i]=="9"){
      dataset$'Seguimient previ'[i]<-"Residencia"
    } else if (dataset$'Seguimient previ'[i]=="10"){
      dataset$'Seguimient previ'[i]<-"ALTRES HOSPITALS"
    } else if (dataset$'Seguimient previ'[i]=="11"){
      dataset$'Seguimient previ'[i]<-"MUTUA"
    } else if (dataset$'Seguimient previ'[i]=="999"){
      dataset$'Seguimient previ'[i]<-NA
    } else {
      dataset$'Seguimient previ'[i]<-NA
    }
  }

  #####################REVISAR SI CAL REDISCRETITZAR #####    
  if (!is.na(dataset$`Seguimient a l'alta`[i])){
    if (dataset$`Seguimient a l'alta`[i]==0){
      dataset$`Seguimient a l'alta`[i]<-"nadie"
    } else if (dataset$`Seguimient a l'alta`[i]==1){
      dataset$`Seguimient a l'alta`[i]<-"ATDOM"
    } else if (dataset$`Seguimient a l'alta`[i]==2){
      dataset$`Seguimient a l'alta`[i]<-"PADES"
    } else if (dataset$`Seguimient a l'alta`[i]==3){
      dataset$`Seguimient a l'alta`[i]<-"FRAGILES"
    } else if (dataset$`Seguimient a l'alta`[i]==4){
      dataset$`Seguimient a l'alta`[i]<-"FRAGILES 1 mes"
    } else if (dataset$`Seguimient a l'alta`[i]==5){
      dataset$`Seguimient a l'alta`[i]<-"CCEE"
    } else if (dataset$`Seguimient a l'alta`[i]==6){
      dataset$`Seguimient a l'alta`[i]<-"MUTUAM"
    } else if (dataset$`Seguimient a l'alta`[i]==7){
      dataset$`Seguimient a l'alta`[i]<-"AISBE grup 3"
    } else if (dataset$`Seguimient a l'alta`[i]==8){
      dataset$`Seguimient a l'alta`[i]<-"AISBE grup 1-2"
    } else if (dataset$`Seguimient a l'alta`[i]==9){
      dataset$`Seguimient a l'alta`[i]<-"Residencia"
    } else if (dataset$`Seguimient a l'alta`[i]==10){
      dataset$`Seguimient a l'alta`[i]<-"INGRESO HOSP."
    } else if (dataset$`Seguimient a l'alta`[i]==11){
      dataset$`Seguimient a l'alta`[i]<-"ATENCIO PRIMARIA"
    } else if (dataset$`Seguimient a l'alta`[i]==12){
      dataset$`Seguimient a l'alta`[i]<-"EXITUS"
    } else if (dataset$`Seguimient a l'alta`[i]==13){
      dataset$`Seguimient a l'alta`[i]<-"ALTRES HOSPITALS"
    } else if (dataset$`Seguimient a l'alta`[i]==999){
      dataset$`Seguimient a l'alta`[i]<-NA
    } 
  }
  
  if (!is.na(dataset$'Seguiment alta categories'[i])){
    if (dataset$'Seguiment alta categories'[i]==0){
      dataset$'Seguiment alta categories'[i]<-"nadie"
    } else if (dataset$'Seguiment alta categories'[i]==1){
      dataset$'Seguiment alta categories'[i]<-"ATENCIO PRIMARIA"
    } else if (dataset$'Seguiment alta categories'[i]==2){
      dataset$'Seguiment alta categories'[i]<-"PADES"
    } else if (dataset$'Seguiment alta categories'[i]==3){
      dataset$'Seguiment alta categories'[i]<-"FRAGILS"
    } else if (dataset$'Seguiment alta categories'[i]==4){
      dataset$'Seguiment alta categories'[i]<-"OTROS"
    } else if (dataset$'Seguiment alta categories'[i]==5){
      dataset$'Seguiment alta categories'[i]<-"REINGRESOS"
    } else if (dataset$'Seguiment alta categories'[i]==999){
      dataset$'Seguiment alta categories'[i]<-NA
    }
  }
}




#Rename all variables 
#Factorize categorical variables in clinical data
colnames(dataset)[colnames(dataset)=="Gènere"]<-"sex"
dataset$sex<-as.factor(dataset$sex)
colnames(dataset)[colnames(dataset)=="Edat"]<-"age"
colnames(dataset)[colnames(dataset)=="Porta entrada"]<-"entr_g"
dataset$entr_g<-as.factor(dataset$entr_g)
colnames(dataset)[colnames(dataset)=="SERVEI que el proposa"]<-"service"
dataset$service<-as.factor(dataset$service)
colnames(dataset)[colnames(dataset)=="Dies ingrés hospital"]<-"days_hospital"
colnames(dataset)[colnames(dataset)=="Dies ingrés HDOM"]<-"days_HH"
colnames(dataset)[colnames(dataset)=="Dies totals"]<-"days_tot"
colnames(dataset)[colnames(dataset)=="any alta hdom"]<-"year_cat"
dataset$year_cat<-as.factor(dataset$year_cat)
#colnames(dataset)[colnames(dataset)==""]<-"diag_cat"
#dataset$diag_cat<-as.factor(dataset$diag_cat)
colnames(dataset)[colnames(dataset)=="N comorbilidades"]<-"N_diag"
colnames(dataset)[colnames(dataset)=="Índex CHARLSON"]<-"charlson"
colnames(dataset)[colnames(dataset)=="Tabaquisme"]<-"smoking"
dataset$smoking<-as.factor(dataset$smoking)
colnames(dataset)[colnames(dataset)=="IMC"]<-"BMI"
colnames(dataset)[colnames(dataset)=="Camina"]<-"walk"
dataset$walk<-as.factor(dataset$walk)
colnames(dataset)[colnames(dataset)=="Índex Barthel"]<-"barthel"
colnames(dataset)[colnames(dataset)=="Procedeix SF-36"]<-"sf_36"
dataset$sf_36<-as.factor(dataset$sf_36)
colnames(dataset)[colnames(dataset)=="Estat físic"]<-"p_state"
colnames(dataset)[colnames(dataset)=="Estat mental"]<-"m_state"
colnames(dataset)[colnames(dataset)=="Equips terapia respiratoria"]<-"equip_pre"
dataset$equip_pre<-as.factor(dataset$equip_pre)
dataset$ox_pre<-as.factor(dataset$ox_pre)
dataset$vent_pre<-as.factor(dataset$vent_pre)
colnames(dataset)[colnames(dataset)=="Nº Tècniques o cures"]<-"Ntecn_home"
colnames(dataset)[colnames(dataset)=="Equips durant HD"]<-"equip_HH"
dataset$equip_HH<-as.factor(dataset$equip_HH)
colnames(dataset)[colnames(dataset)=="Es mantenen equips a l'alta"]<-"equip_dis"
dataset$equip_dis<-as.factor(dataset$equip_dis)
colnames(dataset)[colnames(dataset)=="Dificultat medicació"]<-"diff_med"
dataset$diff_med<-as.factor(dataset$diff_med)
colnames(dataset)[colnames(dataset)=="Nº Pastilles"]<-"pills_day"
colnames(dataset)[colnames(dataset)=="Nº Injeccions"]<-"inj_day"
colnames(dataset)[colnames(dataset)=="Nº Inhalacions"]<-"inh_day"
colnames(dataset)[colnames(dataset)=="ATB EV"]<-"atb_ev"
dataset$atb_ev<-as.factor(dataset$atb_ev)
colnames(dataset)[colnames(dataset)=="FURO EV"]<-"furo_ev"
dataset$furo_ev<-as.factor(dataset$furo_ev)
colnames(dataset)[colnames(dataset)=="CORTIS EV"]<-"cort_ev"
dataset$cort_ev<-as.factor(dataset$cort_ev)
colnames(dataset)[colnames(dataset)=="algun tto ev"]<-"treat_ev"
dataset$treat_ev<-as.factor(dataset$treat_ev)
colnames(dataset)[colnames(dataset)=="HBPM sc"]<-"heparin"
dataset$heparin<-as.factor(dataset$heparin)
dataset$cures<-as.factor(dataset$cures)
colnames(dataset)[colnames(dataset)=="Es realitza EF"]<-"spirometry"
dataset$spirometry<-as.factor(dataset$spirometry)
colnames(dataset)[colnames(dataset)=="Es realitza GSA"]<-"gasometry"
dataset$gasometry<-as.factor(dataset$gasometry)
colnames(dataset)[colnames(dataset)=="Coneix nom patologia crònica"]<-"name_patho"
dataset$name_patho<-as.factor(dataset$name_patho)
colnames(dataset)[colnames(dataset)=="Signes alarma"]<-"alar_sig"
dataset$alar_sig<-as.factor(dataset$alar_sig)
colnames(dataset)[colnames(dataset)=="Nº Consultes urgencies últim any"]<-"emerg_pre"
colnames(dataset)[colnames(dataset)=="Nº Ingressos últim any"]<-"adm_pre"
colnames(dataset)[colnames(dataset)=="Urgencies durant HDOM"]<-"emerg_HH"
dataset$emerg_HH<-as.factor(dataset$emerg_HH)
colnames(dataset)[colnames(dataset)=="Reingrés durant HDOM"]<-"adm_HH"
dataset$adm_HH<-as.factor(dataset$adm_HH)
colnames(dataset)[colnames(dataset)=="Mortalitat"]<-"MORT_DUR"
dataset$MORT_DUR<-as.factor(dataset$MORT_DUR)
colnames(dataset)[colnames(dataset)=="Reingressos als 30 dies"]<-"readm_30"
dataset$readm_30<-as.factor(dataset$readm_30)
colnames(dataset)[colnames(dataset)=="Mortalitat als 30 dies"]<-"mort_30"
dataset$mort_30<-as.factor(dataset$mort_30)
colnames(dataset)[colnames(dataset)=="Visita infermeria"]<-"vis_nurs"
colnames(dataset)[colnames(dataset)=="Visita mèdica"]<-"vis_doc"
colnames(dataset)[colnames(dataset)=="Visita HCP"]<-"vis_hosp"
colnames(dataset)[colnames(dataset)=="Trucada  professional"]<-"call"
colnames(dataset)[colnames(dataset)=="Seguimient previ"]<-"followup_pre"
dataset$followup_pre<-as.factor(dataset$followup_pre)
colnames(dataset)[colnames(dataset)=="Seguimient a l'alta"]<-"followup_dis"
dataset$followup_dis<-as.factor(dataset$followup_dis)
colnames(dataset)[colnames(dataset)=="Seguiment alta categories"]<-"followup_ca"
dataset$followup_ca<-as.factor(dataset$followup_ca)

#Removal of variables with no interest (60 and 90 days after discharge, data from satisfaction survey)
cols.dont.want<-c("Ubicació del pacient","Diagnòstic principal al alta","IMC CAT","Barthel CAT","sf_36", "Es realitza analítica o cultiu","Ultima visita Causa relacionada","últim ingrés causa relacionada","Visites Ucias  als 30 dies post-alta","Visites Ucias 60d post-alta","Visites Ucias 90d post-alta","Reingressos als 60 dies","Reingressos als 90 dies","profesional AI que va realitzar les visites")        
dataset <- dataset[, ! names(dataset) %in% cols.dont.want, drop = F]



# Si no tiene equipos durante, al alta no procede
levels(dataset$equip_dis)<-c(levels(dataset$equip_dis),"NP")
for (i in 1:nrow(dataset)){
  if(!is.na(dataset$equip_HH[i])){
    if(dataset$equip_HH[i]=="nada"){
      dataset$equip_dis[i]<-"NP"
    }
  }
}

#Si tiene equipos previos, al alta se dejan
for (i in 1:nrow(dataset)){
  if(is.na(dataset$equip_dis[i]) & !is.na(dataset$equip_pre[i])){
    if(dataset$equip_pre[i]!="ninguno"){
      dataset$equip_dis[i]<-dataset$equip_pre[i]
    }
  }
}

#### REVISAR SI CAL REDISCRETITZAR VARIABLES!!!!!
table(dataset$service)
table(dataset$equip_pre)
table(dataset$equip_HH)
table(dataset$equip_dis)
table(dataset$followup_ca)
table(dataset$followup_dis)
table(dataset$followup_pre)
table(dataset$ABS)


###################################################
### DETERMINAR DISEASE GROUPS segons ICD9 o ICD10
###################################################
disease_ICD_trunc<-seq(0,0,length.out=nrow(dataset))
dataset<-cbind(dataset,disease_ICD_trunc)
disease_group<-seq(0,0,length.out=nrow(dataset))
disease_group<-factor(disease_group)
dataset<-cbind(dataset,disease_group)

dataset$`ICD-9 al alta`<-gsub(" ","",dataset$`ICD-9 al alta`)

levels(dataset$disease_group)<-c("infectious and parasitic diseases", "neoplasms","endocrine, nutritional and metabolic diseases, and immunity disorders",
                                 "diseases of the blood and blood-forming organs","mental disorders","diseases of the nervous system and sense organs",
                                 "diseases of the circulatory system","diseases of the respiratory system",
                                 "diseases of the digestive system","diseases of the genitourinary system",
                                 "complications of pregnancy, childbirth, and the puerperium","diseases of the skin and subcutaneous tissue",
                                 "diseases of the musculoskeletal system and connective tissue","congenital anomalies",
                                 "certain conditions originating in the perinatal period","symptoms, signs, and ill-defined conditions",
                                 "injury and poisoning","factors influencing health status and contact with health service","external causes of injury and poisoning")

for (i in 1:nrow(dataset)){
  icd=as.character(dataset$`ICD-9 al alta`[i])
  if (str_contains(dataset$`ICD-9 al alta`[i],".")){
    icd<-as.character(gsub("\\.","",dataset$`ICD-9 al alta`[i]))
  }
  if (icd %in% diag_ICD10$icd10){
    dataset$disease_ICD_trunc[i]<-substring(as.character(dataset$`ICD-9 al alta`[i]),1,3)
    if (!is.na(dataset$disease_ICD_trunc[i])){
      if (startsWith(dataset$disease_ICD_trunc[i],"A") | startsWith(dataset$disease_ICD_trunc[i],"B")){
        dataset$disease_group[i]<-"infectious and parasitic diseases"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"C")){
        dataset$disease_group[i]<-"neoplasms"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"D")){
        if (substring(as.character(dataset$disease_ICD_trunc[i]),2,3)<50){
          dataset$disease_group[i]<-"neoplasms"
        } else if (substring(as.character(dataset$disease_ICD_trunc[i]),2,3)<80){
          dataset$disease_group[i]<-"diseases of the blood and blood-forming organs"
        } else if (substring(as.character(dataset$disease_ICD_trunc[i]),2,3)<90){
          dataset$disease_group[i]<-"endocrine, nutritional and metabolic diseases, and immunity disorders"
        }
      } else if (startsWith(dataset$disease_ICD_trunc[i],"E")){
        dataset$disease_group[i]<-"endocrine, nutritional and metabolic diseases, and immunity disorders"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"F")){
        dataset$disease_group[i]<-"mental disorders"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"G") | startsWith(dataset$disease_ICD_trunc[i],"H")){
        dataset$disease_group[i]<-"diseases of the nervous system and sense organs"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"I")){
        dataset$disease_group[i]<-"diseases of the circulatory system"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"J")){
        dataset$disease_group[i]<-"diseases of the respiratory system"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"K")){
        dataset$disease_group[i]<-"diseases of the digestive system"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"L")){
        dataset$disease_group[i]<-"diseases of the skin and subcutaneous tissue"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"M")){
        dataset$disease_group[i]<-"diseases of the musculoskeletal system and connective tissue"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"N")){
        dataset$disease_group[i]<-"diseases of the genitourinary system"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"O")){
        dataset$disease_group[i]<-"complications of pregnancy, childbirth, and the puerperium"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"P")){
        dataset$disease_group[i]<-"certain conditions originating in the perinatal period"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"Q")){
        dataset$disease_group[i]<-"congenital anomalies"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"R")){
        dataset$disease_group[i]<-"symptoms, signs, and ill-defined conditions"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"S") | startsWith(dataset$disease_ICD_trunc[i],"T")){
        dataset$disease_group[i]<-"injury and poisoning"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"V") | startsWith(dataset$disease_ICD_trunc[i],"W") | startsWith(dataset$disease_ICD_trunc[i],"X") | startsWith(dataset$disease_ICD_trunc[i],"Y")){
        dataset$disease_group[i]<-"external causes of injury and poisoning"
      } else if (startsWith(dataset$disease_ICD_trunc[i],"Z")){
        dataset$disease_group[i]<-"factors influencing health status and contact with health service"
      }
    }
    
  } else {
    dataset$disease_ICD_trunc[i]<-substring(as.character(dataset$`ICD-9 al alta`[i]),1,3)
    if (!is.na(dataset$disease_ICD_trunc[i])){
      if (startsWith(dataset$disease_ICD_trunc[i],"V")){
        dataset$disease_group[i]<- "factors influencing health status and contact with health service"   
      } else if (startsWith(dataset$disease_ICD_trunc[i],"E")){
        dataset$disease_group[i]<-"external causes of injury and poisoning"
      }
      if (grepl("^[[:digit:]]+",dataset$disease_ICD_trunc[i])){
        if (as.integer(dataset$disease_ICD_trunc[i])<140){
          dataset$disease_group[i]<-"infectious and parasitic diseases"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=140 && as.integer(dataset$disease_ICD_trunc[i])<240){
          dataset$disease_group[i]<-"neoplasms"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=240 && as.integer(dataset$disease_ICD_trunc[i])<280){
          dataset$disease_group[i]<-"endocrine, nutritional and metabolic diseases, and immunity disorders"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=280 && as.integer(dataset$disease_ICD_trunc[i])<290){
          dataset$disease_group[i]<-"diseases of the blood and blood-forming organs"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=290 && as.integer(dataset$disease_ICD_trunc[i])<320){
          dataset$disease_group[i]<-"mental disorders"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=320 && as.integer(dataset$disease_ICD_trunc[i])<390){
          dataset$disease_group[i]<-"diseases of the nervous system and sense organs"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=390 && as.integer(dataset$disease_ICD_trunc[i])<460){
          dataset$disease_group[i]<-"diseases of the circulatory system"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=460 && as.integer(dataset$disease_ICD_trunc[i])<520){
          dataset$disease_group[i]<-"diseases of the respiratory system"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=520 && as.integer(dataset$disease_ICD_trunc[i])<580){
          dataset$disease_group[i]<-"diseases of the digestive system"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=580 && as.integer(dataset$disease_ICD_trunc[i])<630){
          dataset$disease_group[i]<-"diseases of the genitourinary system"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=630 && as.integer(dataset$disease_ICD_trunc[i])<680){
          dataset$disease_group[i]<-"complications of pregnancy, childbirth, and the puerperium"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=680 && as.integer(dataset$disease_ICD_trunc[i])<710){
          dataset$disease_group[i]<-"diseases of the skin and subcutaneous tissue"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=710 && as.integer(dataset$disease_ICD_trunc[i])<740){
          dataset$disease_group[i]<-"diseases of the musculoskeletal system and connective tissue"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=740 && as.integer(dataset$disease_ICD_trunc[i])<760){
          dataset$disease_group[i]<-"congenital anomalies"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=760 && as.integer(dataset$disease_ICD_trunc[i])<780){
          dataset$disease_group[i]<-"certain conditions originating in the perinatal period"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=780 && as.integer(dataset$disease_ICD_trunc[i])<800){
          dataset$disease_group[i]<-"symptoms, signs, and ill-defined conditions"
        } else if (as.integer(dataset$disease_ICD_trunc[i])>=800 && as.integer(dataset$disease_ICD_trunc[i])<=999){
          dataset$disease_group[i]<-"injury and poisoning"
        }else {
          dataset$disease_group[i]<-NA
        }
      }
    }
  } 
}

#### REVISAR SI CAL REDISCRETITZAR VARIABLES!!!!!
table(dataset$disease_group)

cols.dont.want<-c("disease_ICD_trunc")
dataset <- dataset[, ! names(dataset) %in% cols.dont.want, drop = F]


###################################################
### MODIFICAR ABS
###################################################

for (i in 1:nrow(dataset)){
  dataset$ABS[i]<-substr(dataset$ABS[i],1,2)
}


table(dataset$ABS)



## Modify info in ABS
ABS_new<-seq(0,0,length.out=nrow(dataset))
EAP<-seq(0,0,length.out=nrow(dataset))
AGA<-seq(0,0,length.out=nrow(dataset))
Hospital_ref<-seq(0,0,length.out=nrow(dataset))
proveidor<-seq(0,0,length.out=nrow(dataset))

ABS_new<-factor(ABS_new)
EAP<-factor(EAP)
AGA<-factor(AGA)
Hospital_ref<-factor(Hospital_ref)
proveidor<-factor(proveidor)

dataset<-cbind(dataset,ABS_new)
dataset<-cbind(dataset,EAP)
dataset<-cbind(dataset,AGA)
dataset<-cbind(dataset,Hospital_ref)
dataset<-cbind(dataset,proveidor)

levels(dataset$ABS_new)<-c("2A","2B","2C","2D","2E","3A","3B","3C","3D","3E","3G","3H","4A-4B","4C","5A","5B","5C","5D","6C","6D","Otros")
levels(dataset$EAP)<-c("Sant Antoni","Via Roma","Borrell","Universitat","Casanova","Les Hortes","Poble Sec","Carles Ribas + FOC","Bordeta-Magoria","Sants","Numancia","La Marina","Montnegre","Les Corts","Marc Aureli","Sant Elies","Sarria","Sarria-Les planes","Lesseps","Sant Gervasi-Vallcarca","Otros")
levels(dataset$AGA)<-c("Barcelona Esquerra","Barcelona Dreta","Barcelona Nord","Otros")
levels(dataset$Hospital_ref)<-c("Sagrat Cor","H.Clinic","H.Plato","Otros")
levels(dataset$proveidor)<-c("ICS","CAPSE","EBA","Pere Virgili","Otros")

for (i in 1:nrow(dataset)){
  if(dataset$ABS[i]=="2A"){
    dataset$ABS_new[i]<-"2A"
    dataset$EAP[i]<-"Sant Antoni"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"Sagrat Cor"
    dataset$proveidor[i]<-"ICS"
  } else if (dataset$ABS[i]=="2B"){
    dataset$ABS_new[i]<-"2B"
    dataset$EAP[i]<-"Via Roma"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"Sagrat Cor"
    dataset$proveidor[i]<-"ICS"
  } else if (dataset$ABS[i]=="2C"){
    dataset$ABS_new[i]<-"2C"
    dataset$EAP[i]<-"Borrell"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"H.Clinic"
    dataset$proveidor[i]<-"CAPSE"
  } else if (dataset$ABS[i]=="2D"){
    dataset$ABS_new[i]<-"2D"
    dataset$EAP[i]<-"Universitat"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"Sagrat Cor"
    dataset$proveidor[i]<-"ICS"
  } else if (dataset$ABS[i]=="2E"){
    dataset$ABS_new[i]<-"2E"
    dataset$EAP[i]<-"Casanova"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"H.Clinic"
    dataset$proveidor[i]<-"CAPSE"
  } else if (dataset$ABS[i]=="3A"){
    dataset$ABS_new[i]<-"3A"
    dataset$EAP[i]<-"Les Hortes"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"H.Clinic"
    dataset$proveidor[i]<-"EBA"
  } else if (dataset$ABS[i]=="3B"){
    dataset$ABS_new[i]<-"3B"
    dataset$EAP[i]<-"Poble Sec"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"Sagrat Cor"
    dataset$proveidor[i]<-"ICS"
  } else if (dataset$ABS[i]=="3C"){
    dataset$ABS_new[i]<-"3C"
    dataset$EAP[i]<-"Carles Ribas + FOC"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"H.Clinic"
    dataset$proveidor[i]<-"ICS"
  } else if (dataset$ABS[i]=="3D"){
    dataset$ABS_new[i]<-"3D"
    dataset$EAP[i]<-"Bordeta-Magoria"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"H.Clinic"
    dataset$proveidor[i]<-"ICS"
  } else if (dataset$ABS[i]=="3E"){
    dataset$ABS_new[i]<-"3E"
    dataset$EAP[i]<-"Sants"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"H.Clinic"
    dataset$proveidor[i]<-"ICS"
  } else if (dataset$ABS[i]=="3G"){
    dataset$ABS_new[i]<-"3G"
    dataset$EAP[i]<-"Numancia"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"H.Clinic"
    dataset$proveidor[i]<-"ICS"
  } else if (dataset$ABS[i]=="3H"){
    dataset$ABS_new[i]<-"3H"
    dataset$EAP[i]<-"La Marina"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"H.Clinic"
    dataset$proveidor[i]<-"ICS"
  } else if (dataset$ABS[i]=="4A" || dataset$ABS[i]=="4B"){
    dataset$ABS_new[i]<-"4A-4B"
    dataset$EAP[i]<-"Montnegre"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"H.Clinic"
    dataset$proveidor[i]<-"ICS"
  } else if (dataset$ABS[i]=="4C"){
    dataset$ABS_new[i]<-"4C"
    dataset$EAP[i]<-"Les Corts"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"H.Clinic"
    dataset$proveidor[i]<-"CAPSE"
  } else if (dataset$ABS[i]=="5A"){
    dataset$ABS_new[i]<-"5A"
    dataset$EAP[i]<-"Marc Aureli"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"H.Plato"
    dataset$proveidor[i]<-"ICS"
  } else if (dataset$ABS[i]=="5B"){
    dataset$ABS_new[i]<-"5B"
    dataset$EAP[i]<-"Sant Elies"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"H.Plato"
    dataset$proveidor[i]<-"ICS"
  } else if (dataset$ABS[i]=="5C"){
    dataset$ABS_new[i]<-"5C"
    dataset$EAP[i]<-"Sarria"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"H.Plato"
    dataset$proveidor[i]<-"ICS"
  } else if (dataset$ABS[i]=="5D"){
    dataset$ABS_new[i]<-"5D"
    dataset$EAP[i]<-"Sarria-Les planes"
    dataset$AGA[i]<-"Barcelona Esquerra"
    dataset$Hospital_ref[i]<-"H.Plato"
    dataset$proveidor[i]<-"EBA"
  } else if (dataset$ABS[i]=="6C"){
    dataset$ABS_new[i]<-"6C"
    dataset$EAP[i]<-"Lesseps"
    dataset$AGA[i]<-"Barcelona Dreta"
    dataset$Hospital_ref[i]<-"H.Plato"
    dataset$proveidor[i]<-"Pere Virgili"
  } else if (dataset$ABS[i]=="6D"){
    dataset$ABS_new[i]<-"6D"
    dataset$EAP[i]<-"Sant Gervasi-Vallcarca"
    dataset$AGA[i]<-"Barcelona Nord"
    dataset$Hospital_ref[i]<-"H.Plato"
    dataset$proveidor[i]<-"EBA"
  }else{
    dataset$ABS_new[i]<-"Otros"
    dataset$EAP[i]<-"Otros"
    dataset$AGA[i]<-"Otros"
    dataset$Hospital_ref[i]<-"Otros"
    dataset$proveidor[i]<-"Otros"
  }
}

###################################################
### DETERMINAR GMA poblacional
###################################################

#estratificacio risc poblacional (cutoff at 9.360, 22.098, 40.387)
Pstrat<-seq(0,0,length.out=nrow(dataset))
Pstrat<-factor(Pstrat)
dataset<-cbind(dataset,Pstrat)
levels(dataset$Pstrat)<-c("Risc molt alt","Risc alt","Risc moderat","Risc baix")
for (i in 1:nrow(dataset)){
  if(dataset$GMA[i]>40.387){
    dataset$Pstrat[i]<-"Risc molt alt"
  } else if (dataset$GMA[i]>22.098){
    dataset$Pstrat[i]<-"Risc alt"
  } else if (dataset$GMA[i]>9.360){
    dataset$Pstrat[i]<-"Risc moderat"
  } else{
    dataset$Pstrat[i]<-"Risc baix"
  }
}
dataset$Pstrat<-as.factor(dataset$Pstrat)


#estratificacio risc cohort (cutoff at 0.5, 0.8, 0.95)
Cstrat<-seq(0,0,length.out=nrow(dataset))
Cstrat<-factor(Cstrat)
dataset<-cbind(dataset,Cstrat)
levels(dataset$Cstrat)<-c("Risc molt alt","Risc alt","Risc moderat","Risc baix")
for (i in 1:nrow(dataset)){
  if(dataset$GMA[i]>quantile(dataset$GMA,.95)){
    dataset$Cstrat[i]<-"Risc molt alt"
  } else if (dataset$GMA[i]>quantile(dataset$GMA,.8)){
    dataset$Cstrat[i]<-"Risc alt"
  } else if (dataset$GMA[i]>quantile(dataset$GMA,.5)){
    dataset$Cstrat[i]<-"Risc moderat"
  } else{
    dataset$Cstrat[i]<-"Risc baix"
  }
}
dataset$Cstrat<-as.factor(dataset$Cstrat)

###################################################
### DETERMINAR Mortalitat i readmissio durant HDOM + 30d
###################################################


#set variable mortality during HH and 30 post discharge
mort_hh_30<-seq(0,0,length.out=nrow(dataset))
mort_hh_30<-factor(mort_hh_30)
dataset<-cbind(dataset,mort_hh_30)
levels(dataset$mort_hh_30)<-c("si","no")

for (i in 1:nrow(dataset)){
  if (dataset$mort_30[i] == "si" | dataset$MORT_DUR[i] == "si"){
    dataset$mort_hh_30[i] <- "si"
  } else {
    dataset$mort_hh_30[i] <- "no"
  }
}

#set variable readmision during HH and 30 post discharge
readm_hh_30<-seq(0,0,length.out=nrow(dataset))
readm_hh_30<-factor(readm_hh_30)
dataset<-cbind(dataset,readm_hh_30)
levels(dataset$readm_hh_30)<-c("si","no")

for (i in 1:nrow(dataset)){
  if (dataset$adm_HH [i] == "si" | dataset$readm_30[i] == "si"){
    dataset$readm_hh_30[i] <- "si"
  } else {
    dataset$readm_hh_30[i] <- "no"
  }
}


cols.dont.want<-c("ABS","ICD-9 al alta","MORT_DUR","EAP")        
dataset <- dataset[, ! names(dataset) %in% cols.dont.want, drop = F]
names(dataset)[names(dataset) == "ABS_new"] <- "ABS"
names(dataset)[names(dataset) == "Hospital_ref"] <- "Ref_hospital"
names(dataset)[names(dataset) == "proveidor"] <- "provider"


summary(dataset)

#################################################################################################################################
# Rediscretització de variables
#################################################################################################################################


dataset$entr_g<-combineLevels(dataset$entr_g,levs = c("Fragiles", "Hospital de dia", "Otros"), newLabel = c("Hospital de dia / otros"))
dataset$service<-combineLevels(dataset$service,levs = c("CARDIO", "CIR. GENERAL", "HEPATO","MAS","OTROS"), newLabel = c("OTROS"))
dataset$service<-combineLevels(dataset$service,levs = c("INFECCIONES","URO/NEFRO"), newLabel = c("INFECCIONES / URO / NEFRO"))
dataset$year_cat<-combineLevels(dataset$year_cat,levs = c("2009","2010"), newLabel = c("2010"))
dataset$equip_pre<-combineLevels(dataset$equip_pre, levs = c("bipap", "bipap + ocd"), newLabel = c("bipap / bipap + ocd"))
dataset$equip_pre<-combineLevels(dataset$equip_pre, levs = c("cpap", "cpap + ocd"), newLabel = c("cpap / cpap + ocd"))
dataset$equip_pre<-combineLevels(dataset$equip_pre, levs = c("ocd/nebus + otros", "otros"), newLabel = c("ocd/nebus + otros / otros"))
dataset$equip_HH<-combineLevels(dataset$equip_HH, levs = c("ocd + nebus", "ocd+ nebus + otros"), newLabel = c("ocd + nebus / ocd + nebus + otros"))
dataset$equip_dis<-combineLevels(dataset$equip_dis, levs = c("ocd + nebus", "ocd+ nebus + otros","otros"), newLabel = c("ocd + nebus / ocd + nebus + otros / otros"))
dataset$atb_ev<-combineLevels(dataset$atb_ev, levs = c("Antibi?tico en bomba de perfusion", "Otros tratamientos endovenosos"), newLabel = c("Otros tratamientos endovenosos"))
dataset$followup_pre<-combineLevels(dataset$followup_pre, levs = c("AISBE grup 3", "MUTUA", "ALTRES HOSPITALS","Residencia","RH"), newLabel = c("OTROS"))
dataset$followup_dis<-combineLevels(dataset$followup_dis, levs = c("MUTUAM", "Residencia"), newLabel = c("OTROS"))
dataset$ox_pre<-combineLevels(dataset$ox_pre, levs = c("0"), newLabel = c("no"))
dataset$vent_pre<-combineLevels(dataset$vent_pre, levs = c("0"), newLabel = c("ninguno"))
dataset$disease_group<-combineLevels(dataset$disease_group, levs = c("endocrine, nutritional and metabolic diseases, and immunity disorders",
                                                                       "diseases of the musculoskeletal system and connective tissue",
                                                                       "diseases of the nervous system and sense organs",
                                                                       "complications of pregnancy, childbirth, and the puerperium",
                                                                       "congenital anomalies",
                                                                       "mental disorders",
                                                                       "certain conditions originating in the perinatal period",
                                                                       "external causes of injury and poisoning"), newLabel = c("other"))
dataset$ABS<-combineLevels(dataset$ABS, levs = c("6C","6D","Otros"), newLabel = c("Otros"))
dataset$AGA<-combineLevels(dataset$AGA, levs = c("Barcelona Dreta","Barcelona Nord","Otros"), newLabel = c("Otros"))
dataset$provider<-combineLevels(dataset$provider, levs = c("Pere Virgili","Otros"), newLabel = c("Otros"))
dataset$GMA_cat <- combineLevels(dataset$GMA_cat, levs = c("311", "312","313","Altres"), newLabel = c("Altres"))

summary(dataset)

write.csv(dataset,file="~/Escritorio/Internship/HDOM_09_18/Generated_Files/dataset.csv",row.names = FALSE)

###########################################################################################################################################
#### imputar NAs
############################################################################################################################################


dataset<-read.csv2("~/Escritorio/Internship/HDOM_09_18/Generated_Files/dataset.csv",sep=",",dec=".")

#missing values
n_var_miss(dataset) #43 variables with missing values
# gg_miss_which(dataset) #shows which ones contain NAs
miss_var_summary(dataset) #percentage of missing values in each column
pct_miss(dataset) #total % missing

#vis_miss(dataset, sort_miss = TRUE) 
#gg_miss_var(dataset)

#remove variables to predict
predicted_v <- dataset[,c ("mort_hh_30", 'readm_hh_30', "readm_30", "mort_30")]
dataset<-dataset[ , !(names(dataset) %in% c("mort_hh_30", 'readm_hh_30', "readm_30", "mort_30"))]

#input values
options(stringsAsFactors = TRUE)

set.seed(44)
dataset.imp<-missForest(dataset,maxiter = 1, maxnodes = 1)

dataset.imp$ximp$m_state<-round(dataset.imp$ximp$p_state)
dataset.imp$ximp$m_state<-round(dataset.imp$ximp$m_state)
dataset.imp$ximp$Ntecn_home<-round(dataset.imp$ximp$Ntecn_home)
dataset.imp$ximp$pills_day<-round(dataset.imp$ximp$pills_day)
dataset.imp$ximp$inj_day<-round(dataset.imp$ximp$inj_day)
dataset.imp$ximp$inh_day<-round(dataset.imp$ximp$inh_day)
dataset.imp$ximp$barthel<-round(dataset.imp$ximp$barthel)
dataset.imp$ximp$emerg_pre<-round(dataset.imp$ximp$emerg_pre)
dataset.imp$ximp$adm_pre<-round(dataset.imp$ximp$adm_pre)
dataset.imp$ximp$vis_doc<-round(dataset.imp$ximp$vis_doc)
dataset.imp$ximp$vis_hosp<-round(dataset.imp$ximp$vis_hosp)
dataset.imp$ximp$call<-round(dataset.imp$ximp$call)

#save data
dataset<-dataset.imp$ximp
dataset<-cbind(dataset,predicted_v)

write.csv(dataset,file="~/Escritorio/Internship/HDOM_09_18/Generated_Files/dataset_imputed.csv",row.names = FALSE)

