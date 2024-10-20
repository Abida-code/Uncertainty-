rm(list=ls())
clearhistory <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}
clearhistory()
### creating Prob_recall df
library(dplyr)
library(ggplot2)

# read in codes of al participants with valid data; can be read in from either the oSpan_pruned- or dinoNimi_pruned-folder
setwd("C:/Users/Kasutaja/Desktop/Cognitive Science/Ncategory learning/uncetainty/CODE/data_ospan")
listedFilesOSpan <- list.files(pattern="*.CSV|*.txt")
code_fx <- function(file){
  data <- read.table(file,sep=",",fill=T,head = T)
  code <- c(as.matrix(data[3,"responses"]))
  code <- unlist(strsplit(code,":"))[2]
  code <- toupper(gsub("\\}|\\}|\"","",code))
  return(code)
}
validCodes <- toupper(c(as.matrix(sapply(listedFilesOSpan, code_fx))))
fxOspan <- function(file){
  
  data <- read.table(file,sep=",",fill=T,head = T)
  
  code <- c(as.matrix(data[3,"responses"]))
  code <- unlist(strsplit(code,":"))[2]
  code <- toupper(gsub("\\}|\\}|\"","",code))
  
  summaryData <- data[which(data[,"summary"]=="summary_realTask"),]
  
  solve_accuracy <- mean(summaryData$solve_accuracy)
  
  nCorrect <- sum(summaryData[,"nLettersCorrect"])
  nPresented <- sum(summaryData[,"nLettersPresented"])
  propCorrect <- nCorrect/nPresented
  
  toBeReturned <- data.frame(code, solve_accuracy, nCorrect, nPresented, propCorrect)
  
  return (toBeReturned)
  
}

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
final_data <- vector('list', length(listedFilesDinoNimi))
# adding up all participants' ...
par(new=F)
for(file_x in 1:length(listedFilesDinoNimi)){
  if(file_x==1){
    selection.df <- NULL
    sample.sizes <- NULL
    n.ent <- 0
    n.quartile <- rep(0,4)
  }
  
  fx_dinoNimi_selection <- function(file, oSpan_Scores_df){
    
    #data <- read.table("118.CSV",sep=",",fill=T,head = T)
    #data <- read.table("429.CSV",sep=",",fill=T,head = T)
    #data <- read.table("418.CSV",sep=",",fill=T,head = T)
    data <- read.table(file,sep=",",fill=T,head = T)
    
    code <- c(as.matrix(data[3,"responses"]))
    code <- unlist(strsplit(code,":"))[2]
    code <- toupper(gsub("\\}|\\}|\"","",code))
    df_file_x <- data.frame(code)
    
    # Variable WMC
    oSpan.score <- oSpan_Scores_df$nCorrect[oSpan_Scores_df$code==code]
    WMC.group <- oSpan_Scores_df$WMC.group[oSpan_Scores_df$code==code]
    df_file_x <- cbind(df_file_x, oSpan.score, WMC.group)
    
    # Determining which exemplars were learned on category vs. item level
    displayed.buttons <- as.matrix(data[which(data$summary_judgment =="summary_judgment"),"presentedNamesLabels"])[1]
    displayed.buttons <- unlist(strsplit(displayed.buttons,","))
    if(is.element("Edmontosaurus", displayed.buttons)){taxo.type="taxo.dino"}else{taxo.type="taxo.ptero"}
    df_file_x <- cbind(df_file_x, taxo.type)
    
    data_categorization <- data[data$summary_categorization=="summary_categorization",]
    
    # Variable Time
    timeStamps <- data_categorization[,"time_elapsed"]
    viewingTimes <- data_categorization$rt
    catPhases <- data_categorization[,"genPhase"]
    df_file_x <- cbind(df_file_x, timeStamps, viewingTimes, catPhases)
    
    # Self-selected Stimuli
    displayedPics <- as.matrix(data_categorization[,"stimulus"])
    Stimuli <- c(as.matrix(sapply(displayedPics,function(x){gsub("img/|\\d|.png","",x)})))
    if(length(Stimuli =="Dorygnathusathus")>0){
      Stimuli[match("Dorygnathusathus", Stimuli)] <- "Dorygnathus"
    }
    N_SearchPaths <- sapply(c(1:length(Stimuli)), function(i,Stimuli){length(unique(Stimuli[1:i]))},Stimuli=Stimuli)
    requiredButtons <- c(as.matrix(data_categorization$requiredButton))
    N_Categories <- sapply(c(1:length(requiredButtons)), function(i, requiredButtons){length(unique(requiredButtons[1:i]))}, requiredButtons = requiredButtons)
    
    df_file_x <- cbind(df_file_x,Stimuli, N_SearchPaths, requiredButtons, N_Categories)
    
    # Variable Level of learning
    Level_of_learning <- rep("item_learning",length(Stimuli))
    if(taxo.type=="taxo.dino"){
      cat.episodes <- which(match(Stimuli,c("Pteranodon","Istiodactylus","Dorygnathus","Scaphognathus"))>0)
      if(length(cat.episodes)>0){
        Level_of_learning[cat.episodes] <- "category_learning"
      }
      
    }else{
      cat.episodes <- which(match(Stimuli,c("Parasaurolophus","Edmontosaurus","Rinchenia","Pisanosaurus"))>0)
      if(length(cat.episodes)>0){
        Level_of_learning[cat.episodes] <- "category_learning"
      }
    }
    df_file_x <- cbind(df_file_x, Level_of_learning)
    
    # Variables Practice & Exploration: ingredients
    
    nSelected_CurCat <- rep(NA,nrow(df_file_x))
    practiceAndExploreFx <- function(i, requiredButtons){
      CurCat <- requiredButtons[i]
      if(i==1){
        N <- 1
        dudi <- 0
        chi2 <- 0
        return(c(CurCat, 0, 0, 0, 0, 0, N,dudi, chi2))
        
      }else{
        categoriesUntilNow <- requiredButtons[1:i]
        freqs <- table(categoriesUntilNow)
        ps <- table(categoriesUntilNow)/sum(table(categoriesUntilNow))
        N <- length(ps)
        dudi <- sqrt(sum((ps-mean(ps))^2)/N)
        
        freqs_exp <- sum(freqs)/N
        chi2 <- sum((freqs-freqs_exp)^2/freqs_exp)
        
        previous.categories <- requiredButtons[1:(i-1)]
        previous.categories.tab <- table(previous.categories)
        
        if(is.element(CurCat, previous.categories)){
          
          #positions.curCat <- which(requiredButtons[1:i] ==CurCat)
          
          # NII = Number Intervening Items
          #NII.Fx <- function(pos.x, positions.curCat){ 
          #	if(pos.x==1){
          #			NII.x <- positions.curCat[pos.x]-1
          #		}else{
          #			NII.x <- (positions.curCat[pos.x]-positions.curCat[pos.x-1])-1
          #		}
          #	return(NII.x)
          #}
          #NII.vec <- sapply(c(1:length(positions.curCat)),NII.Fx, positions.curCat = positions.curCat)
          
          nCurCat <- previous.categories.tab[which(names(previous.categories.tab)==CurCat)]
          indices.CompCats <- which(names(previous.categories.tab)!=CurCat)
          nCompCats_unique <- length(indices.CompCats)
          if(length(indices.CompCats)>0){
            nCompCat <- sum(previous.categories.tab[indices.CompCats])
            #previous.p <- prop.table(previous.categories.tab[indices.CompCats])
            previous.p <- prop.table(previous.categories.tab)
            H <- -sum(previous.p *log2(previous.p))
            
            #denom <- log2(length(indices.CompCats))
            denom <- log2(length(previous.p))
            if(denom==0){
              H_norm <- 0
              practice_index <- 0
            }else{
              H_norm <- H/denom
              practice_index <- (nCurCat* (2-H_norm))/(nCurCat* (2-H_norm) +nCompCat*H_norm)
            }
          }else{
            nCompCat <- 0
            H_norm <- 0
            practice_index <- (nCurCat* (2-H_norm))/(nCurCat* (2-H_norm) +nCompCat*H_norm)
          }
        }else{
          nCurCat <- 0
          indices.CompCats <- which(names(previous.categories.tab)!=CurCat)
          nCompCats_unique <- length(indices.CompCats)
          if(length(indices.CompCats)>0){
            nCompCat <- sum(previous.categories.tab[indices.CompCats])
            #previous.p <- prop.table(previous.categories.tab[indices.CompCats])
            previous.p <- prop.table(previous.categories.tab)
            H <- -sum(previous.p *log2(previous.p))
            #denom <- log2(length(indices.CompCats))
            denom <- log2(length(previous.p))
            if(denom==0){
              H_norm <- 0
              practice_index <- 0
            }else{
              H_norm <- H/denom
              practice_index <- 0
            }
          }else{
            nCompCat <- 0
            H_norm <- 0
            practice_index <- 0
          }
        }
        
        return(c(CurCat, nCurCat, nCompCat, nCompCats_unique, H_norm, practice_index,N,dudi, chi2))
        
      }
      
    }
    practiceAndExplore.df <- data.frame(t(sapply(c(1:length(requiredButtons)),practiceAndExploreFx, requiredButtons=requiredButtons)))
    colnames(practiceAndExplore.df) <- c("CurCat", "nCurCat", "nCompCat", "nCompCats_unique", "H_norm", "practice_index","N","dudi","chisq")
    
    df_file_x <- cbind(df_file_x, practiceAndExplore.df[,c("nCurCat", "nCompCat", "nCompCats_unique", "H_norm", "practice_index","N","dudi","chisq")])
    
    chosenButtons <- data_categorization$chosenButton
    matchNomatch <- data_categorization$correctCategorization
    
    for(i in 2:nrow(df_file_x)){
      if(i==2){rep.df <- data.frame(rep.i=0,lag.i=NA)}
      
      if(is.element(df_file_x$Stimuli[i],df_file_x$Stimuli[1:(i-1)])){
        rep.i <- 1
        lag.i <- i-max(which(df_file_x$Stimuli[i]==df_file_x$Stimuli[1:(i-1)]))
      }else{
        rep.i <- 0
        lag.i <- NA
      }
      rep.df <- rbind(rep.df,data.frame(rep.i,lag.i))
    }
    
    df_file_x <- cbind(df_file_x, chosenButtons, matchNomatch, rep.df, i=1:nrow(df_file_x) )
    
    stimuli.code.x <- c(as.matrix(df_file_x $Stimuli))
    switchFx <- function(i, stimuli.code.x){
      if(length(unique(c(stimuli.code.x[i],stimuli.code.x[i-1])))==2){
        switch.i <- "switch"
      }else{
        switch.i <-"noSwitch"
      }
      return(switch.i)
    }
    switch.vec <- c(NA,sapply(c(2:length(stimuli.code.x)),switchFx, stimuli.code.x= stimuli.code.x))
    df_file_x <- cbind(df_file_x,switch.vec)
    
    ## Binary stimulus coding
    r.sumCat <- which(data$summary_categorization=="summary_categorization")
    f1 <- data[r.sumCat-4,"button_pressed"]
    f2 <- data[r.sumCat-3,"button_pressed"]
    f3 <- data[r.sumCat-2,"button_pressed"]
    name.subordinate <- data[r.sumCat-1,"stimulus"]
    name.subordinate <- c(as.matrix(sapply(name.subordinate,function(x){gsub("img/|\\d|.png","",x)})))
    if(length(name.subordinate =="Dorygnathusathus")>0){
      name.subordinate[match("Dorygnathusathus", name.subordinate)] <- "Dorygnathus"
    }
    taxo.subordinate <- c("Pisanosaurus","Rinchenia","Edmontosaurus","Parasaurolophus",
                          "Dorygnathus","Scaphognathus","Istiodactylus","Pteranodon")
    names(taxo.subordinate) <- c("Ornithoscelida","Ornithoscelida","Saurolophidae","Saurolophidae",
                                 "Rhamphorhynchinae","Rhamphorhynchinae","Pteranodontia","Pteranodontia")
    name.basicLevel <- names(taxo.subordinate)[match(name.subordinate,taxo.subordinate)]
    df_file_x <- cbind(df_file_x,f1,f2,f3,name.basicLevel,name.subordinate)
    
    return(df_file_x)
    
  }
  file_x_selectionResults <- fx_dinoNimi_selection(listedFilesDinoNimi[file_x], oSpan_Scores_df= oSpan_Scores_df)

  file_x_selectionResultss<-file_x_selectionResults[,c("code","catPhases","switch.vec", "matchNomatch","chosenButtons", "viewingTimes")]
  file_x_selectionResultss %>% group_by(code, catPhases) %>% mutate(prob_recall = mean(switch.vec == 'noSwitch')) -> result
  Prob_rec<-result[,c("code","catPhases","prob_recall", "viewingTimes", "switch.vec")]
  Prob_rec<- within(Prob_rec,{
    code <- as.factor(code)
    catPhases <- as.factor(catPhases)
  })

  
  Prob_rec<-Prob_rec[!duplicated(Prob_rec), ]
  final_data[[file_x]] <- Prob_rec
  #Prob_rec[is.na(Prob_rec)] <- 0 
  print(Prob_rec)
}
  
combinedata <-  bind_rows(final_data)
#Query1
 bind_rows(final_data) %>%
   ungroup %>%
   mutate(code= as.character(code)) %>%
   slice(1:105) %>%
   ggplot() + aes(catPhases,prob_recall, color = catPhases, group =code) +geom_point() -> plot1
 plot1
 
 
 #Query2
 bind_rows(final_data) %>%
   group_by(code, catPhases) %>%
   summarise(viewTiming = sum(viewingTimes, na.rm = TRUE), 
            switch = na.omit(prob_recall)[1]) -> part2
 
 plot2 <- ggplot(part2[1:70, ]) + aes(catPhases, viewTiming, fill =code) + geom_bar(stat = 'identity', position = 'dodge')

 
 #Query3 
 bind_rows(final_data) %>%
   group_by(catPhases) %>%
   summarise(viewTiming = mean(viewingTimes, na.rm = TRUE)) %>%
   ggplot() + aes(catPhases, viewTiming) + geom_col() -> plot3

 
 #Query4
 bind_rows(final_data) %>%
   group_by(code, catPhases) %>%
   summarise(difference = {
     if(any(switch.vec == 'noSwitch', na.rm = TRUE)) {
       inds <- which(switch.vec == 'noSwitch')
       sum(viewingTimes[inds] - viewingTimes[inds - 1]) 
   } else 0
     }) -> part4
   
  
 