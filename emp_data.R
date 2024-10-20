### creating selection.df

setwd("C:/Users/Kasutaja/Desktop/Cognitive Science/Ncategory learning/uncetainty/CODE")
source("fx_manifestAna.R")

# read in codes of al participants with valid data; can be read in from either the oSpan_pruned- or dinoNimi_pruned-folder
setwd("C:/Users/Kasutaja/Desktop/Cognitive Science/Ncategory learning/uncetainty/CODE/data_ospan")
listedFilesOSpan <- list.files(pattern="*.CSV|*.txt")
validCodes <- toupper(c(as.matrix(sapply(listedFilesOSpan, code_fx))))

## creating a data.frame with all oSpan scores and corresponding codes
setwd("C:/Users/Kasutaja/Desktop/Cognitive Science/Ncategory learning/uncetainty/CODE/data_ospan")
for(i in 1:length(listedFilesOSpan)){
  if(i==1){oSpan_Scores_df <- NULL}
  iData <- fxOspan(listedFilesOSpan[i])
  oSpan_Scores_df <- rbind(oSpan_Scores_df, iData)
}

solve_accuracies <- oSpan_Scores_df$solve_accuracy
recall_accuracy <- oSpan_Scores_df$nCorrect

# excluding outliers, i.e., participants whose solve-accuracy is below the average accuracy minus two times the SD
outliers <- which ( solve_accuracies <= mean(solve_accuracies) - 2*sd(solve_accuracies) )

WMC.group <- with(oSpan_Scores_df, cut(nCorrect, breaks=quantile(nCorrect), include.lowest=TRUE))
WMC.group <- as.numeric(WMC.group)
oSpan_Scores_df <- cbind(oSpan_Scores_df, WMC.group)

accu.outlier <- rep(TRUE,nrow(oSpan_Scores_df))
accu.outlier[outliers] <- FALSE
oSpan_Scores_df <- cbind(oSpan_Scores_df, accu.outlier)


### analyzing dinoNimi
setwd("C:/Users/Kasutaja/Desktop/Cognitive Science/Ncategory learning/uncetainty/CODE/data_dino")
listedFilesDinoNimi <- list.files(pattern="*.CSV|*.txt")
codes_dinoNimi <- toupper(c(as.matrix(sapply(listedFilesDinoNimi, code_fx))))

### self-guided selection

# adding up all participants' ...
for(file_x in 1:length(listedFilesDinoNimi)){
  if(file_x==1){
    selection.df <- NULL
    sample.sizes <- NULL
    n.ent <- 0
    n.quartile <- rep(0,4)
  }
  file_x_selectionResults <- fx_dinoNimi_selection(listedFilesDinoNimi[file_x], oSpan_Scores_df= oSpan_Scores_df)
  if(is.element("Dorygnathusathus",file_x_selectionResults$name.subordinate)){
    file_x_selectionResults[which(file_x_selectionResults$name.subordinate=="Dorygnathusathus"),"name.subordinate"] <- "Dorygnathus"
  }
  
  code.x <- c(as.matrix(file_x_selectionResults$code[1]))
  if(oSpan_Scores_df$accu.outlier[which(oSpan_Scores_df$code==code.x)]){
    nPhases <- length(unique(file_x_selectionResults$catPhases))
    if(nPhases==7){
      n.ent <- n.ent+1
      gx <- file_x_selectionResults$WMC.group[1]
      n.quartile[gx] <- n.quartile[gx]+1
      selection.df <- rbind(selection.df, file_x_selectionResults)
    }
    
  }
  print(c("Creating 'selection.df'",file_x/length(listedFilesDinoNimi)))
}
selection.df[,c("oSpan.score","timeStamps","N_SearchPaths","N_Categories","nCurCat","nCompCat",
                "nCompCats_unique","H_norm","practice_index","matchNomatch")] <- apply(selection.df[,c("oSpan.score","timeStamps","N_SearchPaths","N_Categories","nCurCat","nCompCat",
                                                                                                       "nCompCats_unique","H_norm","practice_index","matchNomatch")],2,as.numeric)
selection.df[,c("code","Level_of_learning","N","dudi","catPhases","WMC.group")] <- apply(selection.df[,c("code","Level_of_learning","N","dudi","catPhases","WMC.group")],2,as.factor)

unique_codes <- unique(selection.df$code)
nEpisodes_perPhase_fx <- function(code_x){
  code_x_data <- selection.df[selection.df$code==code_x,]
  return(as.vector(table(code_x_data$catPhases)))
}
nEpisodes_perPhaseAndCode <- t(sapply(unique_codes,nEpisodes_perPhase_fx))
nEpisodes_perPhase_M <- colMeans(nEpisodes_perPhaseAndCode)
nEpisodes_perPhase_SD <- apply(nEpisodes_perPhaseAndCode,2,sd)

setwd("C:/Users/Kasutaja/Desktop/Cognitive Science/Ncategory learning/uncetainty/CODE")
source("fx_manifestAna.R")

# read in codes of al participants with valid data; can be read in from either the oSpan_pruned- or dinoNimi_pruned-folder
setwd("C:/Users/Kasutaja/Desktop/Cognitive Science/Ncategory learning/uncetainty/CODE/data_ospan")
listedFilesOSpan <- list.files(pattern="*.CSV|*.txt")
validCodes <- toupper(c(as.matrix(sapply(listedFilesOSpan, code_fx))))

## creating a data.frame with all oSpan scores and corresponding codes
setwd("C:/Users/Kasutaja/Desktop/Cognitive Science/Ncategory learning/uncetainty/CODE/data_ospan")
for(i in 1:length(listedFilesOSpan)){
  if(i==1){oSpan_Scores_df <- NULL}
  iData <- fxOspan(listedFilesOSpan[i])
  oSpan_Scores_df <- rbind(oSpan_Scores_df, iData)
}

solve_accuracies <- oSpan_Scores_df$solve_accuracy
recall_accuracy <- oSpan_Scores_df$nCorrect

# excluding outliers, i.e., participants whose solve-accuracy is below the average accuracy minus two times the SD
outliers <- which ( solve_accuracies <= mean(solve_accuracies) - 2*sd(solve_accuracies) )

WMC.group <- with(oSpan_Scores_df, cut(nCorrect, breaks=quantile(nCorrect), include.lowest=TRUE))
WMC.group <- as.numeric(WMC.group)
oSpan_Scores_df <- cbind(oSpan_Scores_df, WMC.group)

accu.outlier <- rep(TRUE,nrow(oSpan_Scores_df))
accu.outlier[outliers] <- FALSE
oSpan_Scores_df <- cbind(oSpan_Scores_df, accu.outlier)

### dinoNimi data
allSubNames <- c("Pisanosaurus","Rinchenia","Edmontosaurus","Parasaurolophus",
                 "Dorygnathus","Scaphognathus","Istiodactylus","Pteranodon")

setwd("C:/Users/Kasutaja/Desktop/Cognitive Science/Ncategory learning/uncetainty/CODE/data_dino")
listedFilesDinoNimi <- list.files(pattern="*.CSV|*.txt")
codes_dinoNimi <- toupper(c(as.matrix(sapply(listedFilesDinoNimi, code_fx))))

for(px in 1:length(listedFilesDinoNimi)){
  if(px==1){taxo_of_participant <- NULL}
  taxo_of_participant <- rbind(taxo_of_participant,taxoFx(listedFilesDinoNimi[px]))
}

#### Study behavior
## PA Probability of selecting a path depending on the path's frequency
for(file_x in 1:length(listedFilesDinoNimi)){
  file <- listedFilesDinoNimi[file_x]
  data <- read.table(file,sep=",",fill=T,head = T)
  code <- c(as.matrix(data[3,"responses"]))
  code <- unlist(strsplit(code,":"))[2]
  code <- toupper(gsub("\\}|\\}|\"","",code))
  code.taxo_type <- as.vector(taxo_of_participant[taxo_of_participant$code==code,"taxo.type"])
  if(code.taxo_type=="taxo.dino"){allStimLabels <- names(path_structure_dino)}else{
    allStimLabels <- names(path_structure_ptero)
  }
  
  if(oSpan_Scores_df[which(oSpan_Scores_df$code==code),"accu.outlier"]){
    file_x_StudyData <- fx_dinoNimi_selection(listedFilesDinoNimi[file_x], oSpan_Scores_df= oSpan_Scores_df)
    if(is.element("Dorygnathusathus",file_x_StudyData$name.subordinate)){
      file_x_StudyData[which(file_x_StudyData$name.subordinate=="Dorygnathusathus"),"name.subordinate"] <- "Dorygnathus"
    }
    nPhases <- length(unique(as.vector(file_x_StudyData$catPhases)))
    
    if(nPhases==7){
      code.Ospan <- file_x_StudyData$oSpan[1]
      code.Group <- file_x_StudyData$WMC.group[1]
      
      if(file_x==1){PA_df <- NULL} # PA Preferential Attachment
      
      code_x_uniqueStims <- unique(file_x_StudyData$name.subordinate)
      freq_vec <- rep(0,length(code_x_uniqueStims))
      names(freq_vec) <- code_x_uniqueStims
      for(i in 2:nrow(file_x_StudyData)){
        if(i==2){PA_df_codex <- NULL}
        past_paths <- as.vector(file_x_StudyData[1:(i-1),"name.subordinate"])
        freq_vec[match(names(table(past_paths)),names(freq_vec))] <- table(past_paths)
        prop_vec <- round(prop.table(freq_vec),2)
        i_path_subName <- as.vector(file_x_StudyData[i,"name.subordinate"])
        path_selected <- rep(0,length(freq_vec))
        path_selected[which(names(freq_vec)==i_path_subName)] <- 1
        i_phase <- as.vector(file_x_StudyData$catPhases[i])
        i_df <- data.frame(code,ospan=code.Ospan,group=code.Group,phase=i_phase,i,path_name=names(freq_vec),freq_vec,prop_vec,path_selected)
        PA_df_codex <- rbind(PA_df_codex,i_df)
      }
      PA_df <- rbind(PA_df,PA_df_codex)
    }
  }
  
  print(c("Gathering PA emp",paste("Subject",file_x/length(listedFilesDinoNimi))))
}

PA_df <- within(PA_df,{
  code <- as.factor(code)
  group <- as.factor(group)
  phase <- as.factor(phase)
  path_name <- as.factor(path_name)
})

PA_df_noP1 <- PA_df[-which(PA_df$prop_vec==1),]
PA_df_noP1$propCat <- as.numeric(cut(PA_df_noP1$prop_vec,seq(0,1,0.1),include.lowest = T))
# only few data points in the tenth decile 
P_Revisit_by_Freq <- aggregate(path_selected~propCat,PA_df_noP1,function(i){c(mean(i),sd(i))})
PA_M_emp <-  P_Revisit_by_Freq[,2][1:9,1]
PA_SD_emp <- P_Revisit_by_Freq[,2][1:9,2]
PA_SEM_emp <- PA_SD_emp/sqrt(table(PA_df_noP1$propCat)[1:9])

PA_byGroup <- aggregate(path_selected~propCat*group,PA_df_noP1,function(i){c(mean(i),sd(i))})
PA_q1_M_emp <- PA_byGroup[PA_byGroup$group==1,3][1:9,1]
PA_q1_SD_emp <- PA_byGroup[PA_byGroup$group==1,3][1:9,2]
# only 9 instead of 10 data points
PA_q1_SEM_emp <- PA_q1_SD_emp/sqrt(table(PA_df_noP1$propCat,PA_df_noP1$group)[1:9,1])

PA_q4_M_emp <- PA_byGroup[PA_byGroup$group==4,3][1:9,1]
PA_q4_SD_emp <- PA_byGroup[PA_byGroup$group==4,3][1:9,2]
# only 9 instead of 10 data points
PA_q4_SEM_emp <- PA_q4_SD_emp/sqrt(table(PA_df_noP1$propCat,PA_df_noP1$group)[1:9,4])

PA_df_noP1_2 <- PA_df_noP1[-which(PA_df_noP1$ospan<30),]
summary(glm(path_selected~ospan+prop_vec, family=binomial, data=PA_df_noP1_2))
summary(glm(path_selected~ospan*prop_vec, family=binomial, data=PA_df_noP1_2))

## ER Exploration Rate
for(file_x in 1:length(listedFilesDinoNimi)){
  if(file_x==1){
    ExpRate.df <- NULL
  }
  file <- listedFilesDinoNimi[file_x]
  data <- read.table(file,sep=",",fill=T,head = T)
  code <- c(as.matrix(data[3,"responses"]))
  code <- unlist(strsplit(code,":"))[2]
  code <- toupper(gsub("\\}|\\}|\"","",code))
  code.taxo_type <- as.vector(taxo_of_participant[taxo_of_participant$code==code,"taxo.type"])
  if(code.taxo_type=="taxo.dino"){allStimLabels <- names(path_structure_dino)}else{
    allStimLabels <- names(path_structure_ptero)
  }
  if(oSpan_Scores_df[which(oSpan_Scores_df$code==code),"accu.outlier"]){
    file_x_StudyData <- fx_dinoNimi_selection(listedFilesDinoNimi[file_x], oSpan_Scores_df= oSpan_Scores_df)
    code.Ospan <- file_x_StudyData$oSpan.score[1]
    code.Group <- file_x_StudyData$WMC.group[1]
    nPhases <- length(unique(file_x_StudyData$catPhases))
    if(nPhases==7){
      if(is.element("Dorygnathusathus",file_x_StudyData$name.subordinate)){
        file_x_StudyData[which(file_x_StudyData$name.subordinate=="Dorygnathusathus"),"name.subordinate"] <- "Dorygnathus"
      }
      for(i in 1:nrow(file_x_StudyData)){
        i_phase <- file_x_StudyData[i,"catPhases"]
        nPaths_unique <- length(unique(file_x_StudyData[1:i,"name.subordinate"]))
        if(i==1){newCat <- 1}else{
          if(is.element(file_x_StudyData[i,"name.subordinate"],file_x_StudyData[1:(i-1),"name.subordinate"])){
            newCat <- 0
          }else{
            newCat <- 1
          }
        }
       
        i_df <- data.frame(code,ospan=code.Ospan,group=code.Group,phase=i_phase,i=i,nPaths=nPaths_unique,newCat=newCat)
        ExpRate.df <- rbind(ExpRate.df,i_df)
      }
    }
  }
  print(c("Gathering ER emp",paste("Subject",file_x/length(listedFilesDinoNimi))))
}
ExpRate.df <- within(ExpRate.df,{
  code <- as.factor(code)
  group <- as.factor(group)
  #phase <- as.factor(phase)
  i <- as.factor(i)
})
## Probability choosing new category
ER_aggro <- aggregate(newCat~phase,ExpRate.df,function(i){c(mean(i),sd(i))})
ER_M_emp <-  ER_aggro[,2][,1]
ER_SD_emp <- ER_aggro[,2][,2]
ER_SEM_emp <- ER_SD_emp/sqrt(table(ExpRate.df$phase))

ER_aggroGroup <- aggregate(newCat~phase*group,ExpRate.df,function(i){c(mean(i),sd(i))})
ER_q1_M_emp <- ER_aggroGroup[ER_aggroGroup$group==1,3][,1]
ER_q1_SD_emp <- ER_aggroGroup[ER_aggroGroup$group==1,3][,2]
ER_q1_SEM_emp <- ER_q1_SD_emp/sqrt(table(ExpRate.df$phase,ExpRate.df$group)[,1])

ER_q4_M_emp <- ER_aggroGroup[ER_aggroGroup$group==4,3][,1]
ER_q4_SD_emp <- ER_aggroGroup[ER_aggroGroup$group==4,3][,2]
ER_q4_SEM_emp <- ER_q4_SD_emp/sqrt(table(ExpRate.df$phase,ExpRate.df$group)[,4])

ExpRate.df2 <- ExpRate.df[-which(ExpRate.df$ospan<30),]
summary(glm(newCat~ospan+phase, family=binomial, data=ExpRate.df2))
summary(glm(newCat~ospan*phase, family=binomial, data=ExpRate.df2))

#### Learning Performance
## PE Effect of practice
for(file_x in 1:length(listedFilesDinoNimi)){
  if(file_x==1){
    P_E.df <- NULL
  }
  file <- listedFilesDinoNimi[file_x]
  data <- read.table(file,sep=",",fill=T,head = T)
  code <- c(as.matrix(data[3,"responses"]))
  code <- unlist(strsplit(code,":"))[2]
  code <- toupper(gsub("\\}|\\}|\"","",code))
  code.taxo_type <- as.vector(taxo_of_participant[taxo_of_participant$code==code,"taxo.type"])
  if(code.taxo_type=="taxo.dino"){allStimLabels <- names(path_structure_dino)}else{
    allStimLabels <- names(path_structure_ptero)
  }
  if(oSpan_Scores_df[which(oSpan_Scores_df$code==code),"accu.outlier"]){
    file_x_StudyData <- fx_dinoNimi_selection(listedFilesDinoNimi[file_x], oSpan_Scores_df= oSpan_Scores_df)
    code.Ospan <- file_x_StudyData$oSpan.score[1]
    code.Group <- file_x_StudyData$WMC.group[1]
    nPhases <- length(unique(file_x_StudyData$catPhases))
    if(nPhases==7){
      if(is.element("Dorygnathusathus",file_x_StudyData$name.subordinate)){
        file_x_StudyData[which(file_x_StudyData$name.subordinate=="Dorygnathusathus"),"name.subordinate"] <- "Dorygnathus"
      }
      code_x.dinos <- as.vector(unique(file_x_StudyData$name.subordinate))
      for(dx in 1:length(code_x.dinos)){
        #if(dx==1){code_df <- NULL}
        dinox <- code_x.dinos[dx]
        dinox_data <- file_x_StudyData[file_x_StudyData$name.subordinate==dinox,]
        dinox_df <- data.frame(code=code,ospan=code.Ospan,group=code.Group,dino=dinox,phase=dinox_data$catPhases,
                               i=c(1:nrow(dinox_data)),wrong_choice=1-dinox_data$matchNomatch)
        #code_df <- rbind(code_df,dinox_df)
        P_E.df <- rbind(P_E.df,dinox_df)
      }
    }
  }
  print(c("Gathering PE emp",paste("Subject",file_x/length(listedFilesDinoNimi))))
}
P_E.df <- within(P_E.df,{
  code <- as.factor(code)
  group <- as.factor(group)
  dino <- as.factor(dino)
  #phase <- as.factor(phase)
})

PE_aggr <- aggregate(wrong_choice~phase,P_E.df,function(i){c(mean(i),sd(i))})
PE_M_emp <- PE_aggr$wrong_choice[,1]
PE_SD_emp <- PE_aggr$wrong_choice[,1]
PE_SEM_emp <- PE_SD_emp/sqrt(table(P_E.df$phase))

PE_aggrGroup <- aggregate(wrong_choice~phase*group,P_E.df,function(i){c(mean(i),sd(i))})
PE_q1_M_emp <- PE_aggrGroup[PE_aggrGroup$group==1,3][,1]
PE_q1_SD_emp <- PE_aggrGroup[PE_aggrGroup$group==1,3][,2]
PE_q1_SEM_emp <- PE_q1_SD_emp/sqrt(table(P_E.df$phase,P_E.df$group)[,1])

PE_q4_M_emp <- PE_aggrGroup[PE_aggrGroup$group==4,3][,1]
PE_q4_SD_emp <- PE_aggrGroup[PE_aggrGroup$group==4,3][,2]
PE_q4_SEM_emp <- PE_q4_SD_emp/sqrt(table(P_E.df$phase,P_E.df$group)[,4])

P_E.df2 <- P_E.df[-which(P_E.df$ospan<30),]
summary(glm(wrong_choice~ospan+phase, family=binomial, data=P_E.df2))

## Graph for an impact card (CEITER project)
P_E.df_q1q4 <- P_E.df2[(P_E.df2$group==1)|(P_E.df2$group==4),]
P_E_means_q1q4 <- aggregate(wrong_choice~phase*group,P_E.df_q1q4,mean)
means_q1 <- P_E_means_q1q4[P_E_means_q1q4$group==1,"wrong_choice"]
means_q4 <- P_E_means_q1q4[P_E_means_q1q4$group==4,"wrong_choice"]
P_E_sd_q1q4 <- aggregate(wrong_choice~phase*group,P_E.df_q1q4,sd)
sd_q1 <- P_E_sd_q1q4[P_E_sd_q1q4$group==1,"wrong_choice"]
n_q1 <- length(unique(P_E.df2[(P_E.df2$group==1),"code"]))
sem_q1 <- sd_q1/sqrt(n_q1)
sd_q4 <- P_E_sd_q1q4[P_E_sd_q1q4$group==4,"wrong_choice"]
n_q4 <- length(unique(P_E.df2[(P_E.df2$group==4),"code"]))
sem_q4 <- sd_q4/sqrt(n_q4)
plot(means_q1,ylim = c(0,1),xlab = "Study Phase",ylab = "Error Probability (labeling exemplars)",main = "Figure. Learning Curve and CLA")
points(means_q4,pch=17)
arrowFx <- function(M,SEM,x,colx){arrows(x, M+SEM,x,M-SEM,lwd=.5,code=3,angle=90,length=.05,col=colx)}
arrowFx(M=means_q1,SEM=sem_q1,x=1:7,col=1)
arrowFx(M=means_q4,SEM=sem_q4,x=1:7,col=1)
abline(h=1-(1/6),lty=3)
legend(4.5,0.8,c("Low Span","High Span"),box.lwd = 0,pch = c(1,17))
sim_data <- c(0.7133743,0.5957092,0.4492906,0.3549231,0.2843801,0.2494442,0.1981390)
points(sim_data,type = "l",lty=2)

## RI Effect of NIE (Number Intervening Episodes)
for(file_x in 1:length(listedFilesDinoNimi)){
  if(file_x==1){
    RI.df <- NULL
  }
  file <- listedFilesDinoNimi[file_x]
  data <- read.table(file,sep=",",fill=T,head = T)
  code <- c(as.matrix(data[3,"responses"]));code <- unlist(strsplit(code,":"))[2];code <- toupper(gsub("\\}|\\}|\"","",code))
  code.taxo_type <- as.vector(taxo_of_participant[taxo_of_participant$code==code,"taxo.type"])
  if(code.taxo_type=="taxo.dino"){allStimLabels <- names(path_structure_dino)}else{
    allStimLabels <- names(path_structure_ptero)
  }
  if(oSpan_Scores_df[which(oSpan_Scores_df$code==code),"accu.outlier"]){
    file_x_StudyData <- fx_dinoNimi_selection(listedFilesDinoNimi[file_x], oSpan_Scores_df= oSpan_Scores_df)
    code.Ospan <- file_x_StudyData$oSpan.score[1]
    code.Group <- file_x_StudyData$WMC.group[1]
    nPhases <- length(unique(file_x_StudyData$catPhases))
    if(nPhases==7){
      if(is.element("Dorygnathusathus",file_x_StudyData$name.subordinate)){
        file_x_StudyData[which(file_x_StudyData$name.subordinate=="Dorygnathusathus"),"name.subordinate"] <- "Dorygnathus"
      }
      code_x.dinos <- as.vector(unique(file_x_StudyData$name.subordinate))
      for(dx in 1:length(code_x.dinos)){
        dinox <- code_x.dinos[dx]
        dinox_data <- file_x_StudyData[file_x_StudyData$name.subordinate==dinox,]
        if(nrow(dinox_data)>1){
          rel_rows <- 2:nrow(dinox_data)
          dinox_NIE <- dinox_data$lag.i[rel_rows]
          dinox_df <- data.frame(code=code,ospan=code.Ospan,group=code.Group,dino=dinox,phase=dinox_data$catPhases[rel_rows],
                                 NIE=dinox_NIE,wrong_choice=1-dinox_data$matchNomatch[rel_rows])
          RI.df <- rbind(RI.df,dinox_df)
        }
      }
    }
  }
  print(c("Gathering RI emp",paste("Subject",file_x/length(listedFilesDinoNimi))))
}
RI.df <- within(RI.df,{
  code <- as.factor(code)
  group <- as.factor(group)
  dino <- as.factor(dino)
  #phase <- as.factor(phase)
})
NIE_aggr <- aggregate(wrong_choice~NIE,RI.df,function(i){c(mean(i),sd(i))})
nobs <- 4
RI_M_emp <- NIE_aggr$wrong_choice[1:nobs,1]
RI_SD_emp <- NIE_aggr$wrong_choice[1:nobs,2]
RI_SEM_emp <- NIE_aggr$wrong_choice[1:nobs,2]/sqrt(table(RI.df$NIE)[1:nobs])

RI_aggrGroup <- aggregate(wrong_choice~NIE*group,RI.df,function(i){c(mean(i),sd(i))})
RI_q1_M_emp <- RI_aggrGroup[RI_aggrGroup$group==1,3][1:10,1]
RI_q1_SD_emp <- RI_aggrGroup[RI_aggrGroup$group==1,3][1:10,2]
RI_q1_SEM_emp <- RI_q1_SD_emp/sqrt(table(RI.df$NIE,RI.df$group)[1:10,1])

RI_q4_M_emp <- RI_aggrGroup[RI_aggrGroup$group==4,3][1:10,1]
RI_q4_SD_emp <- RI_aggrGroup[RI_aggrGroup$group==4,3][1:10,2]
RI_q4_SEM_emp <- RI_q4_SD_emp/sqrt(table(RI.df$NIE,RI.df$group)[1:10,4])

RI.df2 <- RI.df[-which(RI.df$ospan<30),]
summary(glm(wrong_choice~ospan+NIE, family=binomial, data=RI.df2))
summary(glm(wrong_choice~ospan+NIE+phase, family=binomial, data=RI.df2))

summary(glm(wrong_choice~ospan*(NIE+phase), family=binomial, data=RI.df2))

