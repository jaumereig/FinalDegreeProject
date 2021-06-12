library(data.table)
# Load data
dataset <- read.csv( "~/Escritorio/Internship/HDOM_09_18/Generated_Files/dataset_imputed.csv")
# AGE - Student T-test
t.test( age ~ mort_hh_30, data= dataset, alternative="two.sided")
t.test( age ~ readm_hh_30, data= dataset, alternative="two.sided")
# SEX -- Fisher test
dataset$sex <- as.factor(dataset$sex)
fisher.sex.mort <- data.frame(sex = rep(NA, nlevels(dataset$sex)),
                          pvalue = rep(NA, nlevels(dataset$sex)))
for (i in 1:nlevels(dataset$sex)){
  fisher.sex.mort$sex[i]<-levels(dataset$sex)[i]
  fisher.sex.mort$pvalue[i]<-fisher.test(data.table(c(sum(dataset$sex==levels(dataset$sex)[i]), 
                                                  sum(dataset$sex!=levels(dataset$sex)[i])),
                                                c(sum(dataset$sex[dataset$mort_hh_30=="male"]==levels(dataset$sex)[i]),
                                                  sum(dataset$sex[dataset$mort_hh_30=="male"]!=levels(dataset$sex)[i]))))$p.value
  
}
dataset$sex <- as.factor(dataset$sex)
fisher.sex.readm <- data.frame(sex = rep(NA, nlevels(dataset$sex)),
                         pvalue = rep(NA, nlevels(dataset$sex)))
for (i in 1:nlevels(dataset$sex)){
  fisher.sex.readm$sex[i]<-levels(dataset$sex)[i]
  fisher.sex.readm$pvalue[i]<-fisher.test(data.table(c(sum(dataset$sex==levels(dataset$sex)[i]), 
                                                 sum(dataset$sex!=levels(dataset$sex)[i])),
                                               c(sum(dataset$sex[dataset$readm_hh_30=="male"]==levels(dataset$sex)[i]),
                                                 sum(dataset$sex[dataset$readm_hh_30=="male"]!=levels(dataset$sex)[i]))))$p.value
  
}

# GMA
t.test(GMA ~ mort_hh_30, data= dataset, alternative="two.sided")
t.test(GMA ~ readm_hh_30, data= dataset, alternative="two.sided")
# CHARLSON
t.test(charlson ~ mort_hh_30, data= dataset, alternative="two.sided")
t.test(charlson ~ readm_hh_30, data= dataset, alternative="two.sided")
# DISEASE -- Fisher test
dataset$disease_group <- as.factor(dataset$disease_group)
fisher.mort <- data.frame(disease = rep(NA, nlevels(dataset$disease_group)),
                     pvalue = rep(NA, nlevels(dataset$disease_group)))
for (i in 1:nlevels(dataset$disease_group)){
  fisher.mort$disease[i]<-levels(dataset$disease_group)[i]
  fisher.mort$pvalue[i]<-fisher.test(data.table(c(sum(dataset$disease_group==levels(dataset$disease_group)[i]), 
                                             sum(dataset$disease_group!=levels(dataset$disease_group)[i])),
                                           c(sum(dataset$disease_group[dataset$mort_hh_30=="si"]==levels(dataset$disease_group)[i]),
                                             sum(dataset$disease_group[dataset$mort_hh_30=="si"]!=levels(dataset$disease_group)[i]))))$p.value
  
}

fisher.readm <- data.frame(disease = rep(NA, nlevels(dataset$disease_group)),
                     pvalue = rep(NA, nlevels(dataset$disease_group)))
for (i in 1:nlevels(dataset$disease_group)){
  fisher.readm$disease[i]<-levels(dataset$disease_group)[i]
  fisher.readm$pvalue[i]<-fisher.test(data.table(c(sum(dataset$disease_group==levels(dataset$disease_group)[i]), 
                                             sum(dataset$disease_group!=levels(dataset$disease_group)[i])),
                                           c(sum(dataset$disease_group[dataset$readm_hh_30=="si"]==levels(dataset$disease_group)[i]),
                                             sum(dataset$disease_group[dataset$readm_hh_30=="si"]!=levels(dataset$disease_group)[i]))))$p.value
  
}
