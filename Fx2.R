

SubNames <- c("Pisanosaurus","Rinchenia","Edmontosaurus","Parasaurolophus",
                 "Dorygnathus","Scaphognathus","Istiodactylus","Pteranodon")

training_data_fx <- function(code_x,selection.df){
  #code_x <- "AK03K2"
  code_x_data <- selection.df[selection.df$code==code_x,]
  code_x_taxo_type <- code_x_data$taxo.type[1]
  code_x_ospan <- code_x_data$oSpan.score
  code_x_group <- code_x_data$WMC.group
  code_x_phases <- code_x_data$catPhases
  code_x_dinos <- as.vector(code_x_data$Stimuli)
  featurePattern <- code_x_data[,c("f1","f2","f3")]
  code_x_labelCorrect <- code_x_data$matchNomatch
  return(cbind(code=code_x,taxo_type=code_x_taxo_type,ospan=code_x_ospan,group=code_x_group,phase=code_x_phases,
               dino_name=code_x_dinos,featurePattern,labelCorrect=code_x_labelCorrect))
}
#training_data_fx("AK03K2",selection.df)

### This function/model wasn't used for the LAK paper, so I do not comment on it
BSB_learnOnly_fx <- function(para){
  #para <- c(149,56,3.189366e-01,2.681809e+00,8.175618e-03,-1.355133e+03,1.000000e+00,1.171126e-01,9.272892e-01)
  #names(para) <- c("nCells_Label","nCells_Item","within_catNoise","c_label","g","e","R","r","out_intf")
  
  unique_codes <- unique(selection.df$code)
  #stopit <- 0
  #sx <- 0
  #while(stopit==0){
    #sx <- sx+1
    for(agent in 1:length(unique_codes)){
    
    if(agent==1){
      start_time <- Sys.time()
      
      i_run <- 0 # current row in searchMat
      
      nEpisodes_perParti <- as.vector(rowSums(nEpisodes_perPhaseAndCode))
      nrows_sm <- sum(nEpisodes_perParti)
      var_toBeStored <- c("agent","ospan","group","episode","phase","path","nPath","nie","p_error_sim","choice_wrong_data")
      searchMat <- matrix(rep(NA,length(var_toBeStored)*nrows_sm),nrow=nrows_sm)
      colnames(searchMat) <- var_toBeStored
      
      second_button.fx <- function(first_button,button_similarity){
        btn_vec <- sign(rnorm(para["nCells_Item"]))
        mask <- runif(para["nCells_Item"])<button_similarity
        return((1-mask)*btn_vec+mask*first_button)
      }
      
      F_loco1 <- sign(rnorm(para["nCells_Item"]))
      F_loco <- list(F_loco1,second_button.fx(first_button=F_loco1,button_similarity=0))
      
      F_size1 <- sign(rnorm(para["nCells_Item"]))
      F_size <- list(F_size1,second_button.fx(first_button=F_size1,button_similarity=0))
      
      F_skull1 <- sign(rnorm(para["nCells_Item"]))
      F_skull <- list(F_skull1,second_button.fx(first_button=F_skull1,button_similarity=0))
      
      paths <- list(path_1 = c(1,1,1), path_2 = c(1,1,2), path_3 = c(1,2,1), path_4 = c(1,2,2),
                    path_5 = c(2,1,1), path_6 = c(2,1,2), path_7 = c(2,2,1), path_8 = c(2,2,2))
      
      nItems_per_cat <- 10
      item.fx <- function(path.x,F_loco,F_size,F_skull,withinCatNoise,nItems_per_cat){
        f_cat.x <- c(F_loco[[path.x[1]]],F_size[[path.x[2]]],F_skull[[path.x[3]]])
        items_of_cat.mat <- matrix(NA,nItems_per_cat*length(f_cat.x),nrow=nItems_per_cat,ncol=length(f_cat.x))
        for(x in 1:nItems_per_cat){
          item.x.noise <- sample(c(-1,1),length(f_cat.x),prob=c(withinCatNoise,1-withinCatNoise),replace=T)
          f_item.x <- f_cat.x*item.x.noise
          items_of_cat.mat[x,] <- f_item.x
        }
        return(items_of_cat.mat)
      }
      items <- lapply(paths,item.fx,F_loco=F_loco,F_size=F_size,F_skull=F_skull,
                      withinCatNoise=para["within_catNoise"],nItems_per_cat=nItems_per_cat)
      
      Labels <- t(replicate(6,sign(rnorm(para["nCells_Label"]))))
      
      taxo.mapping.dino <- data.frame(path_ID=c(1:8),path_label=c(1,2,3,4,5,5,6,6))
      taxo.mapping.ptero <- data.frame(path_ID=c(1:8),path_label=c(1,1,2,2,3,4,5,6))
      
    }
    
    code_of_agent <- unique_codes[agent]
    trainingData_agent <- training_data_fx(code_of_agent,selection.df)
    taxo_type <- as.vector(trainingData_agent$taxo_type)[1]
    if(taxo_type=="taxo.dino"){taxo.mapping <- taxo.mapping.dino}else{taxo.mapping <- taxo.mapping.ptero}
    ospan <- trainingData_agent$ospan[1]
    group <- as.numeric(as.vector(trainingData_agent$group[1]))
    
    
    for(i in 1:nrow(trainingData_agent)){
      
      i_run <- i_run+1
      i_phase <- as.numeric(as.vector(trainingData_agent$phase[i]))
      
      if(i==1){
        W <- matrix(0,nrow=para["nCells_Item"]*3,ncol=para["nCells_Label"])
        E1 <- rep(NA,6)
      }
      
      path_i <- trainingData_agent[i,c("f1","f2","f3")]+1
      path_i_subname <- as.vector(trainingData_agent[i,"dino_name"])
      choice_wrong_data <- 1-trainingData_agent$labelCorrect[i]
      
      if(i>1){
        path_i_prevEpisodes <- which(as.vector(trainingData_agent[1:(i-1),"dino_name"])==path_i_subname)
        nPath <- length(path_i_prevEpisodes)
        if(nPath>0){nie <- i-max(path_i_prevEpisodes)-1}else{nie <- NA}
      }else{
        nPath <- 0
        nie <- NA
      }
      
      path_i_ID <- sum(abs(1-path_i)*c(4,2,1))+1
      path_i_label <- taxo.mapping[path_i_ID,"path_label"]
      
      f_i <- items[[path_i_ID]][sample(c(1:nItems_per_cat),1),]
      
      ## labelling
      l_IN <- sign(as.vector(f_i%*%W))
      # output interference
      W <- W+rnorm(length(W),sd=para["out_intf"])
      #if(sum(l_IN)!=0){l_IN <- l_IN/sqrt(sum(l_IN^2))}
      fx.sim_labels <- function(l_IN,Labels){
        if(sum(l_IN)!=0){
          D.fx <- function(x,y=l_IN){return(sqrt(sum((x-y)^2)))}
          D <- apply(Labels,1,D.fx)
          sim_labels <- exp(-para["c_label"]*(D^2))
          if(length(which(sim_labels<0))>0){sim_labels[which(sim_labels<0)] <- 0}
        }else{
          sim_labels <- rep(1/nrow(Labels),nrow(Labels))
        }
        return(sim_labels)
      }
      sim_labels <- fx.sim_labels(l_IN,Labels)
      
      if(sum(sim_labels)==0){
        p_choices <- rep(1/nrow(Labels),nrow(Labels))
      }else{
        p_choices <- sim_labels/sum(sim_labels)
      }
      i_choice <- sample(c(1:6),1,prob=p_choices)
      p_error <- 1-p_choices[path_i_label]
      
      ## encoding
      l <- Labels[path_i_label,] # v = true label presented during feedback
      E <- -l%*%l_IN # E = novelty
      A <- 1 / ( 1 + exp(-(E-para["e"])*para["g"]))
      eta <- c(as.matrix(A))*para["R"]
      delta <- eta*f_i%o%l
      W <- W+delta
      # removal
      'if(i_choice!=path_i_label){
        eta_r <- as.vector(A)*para["r"]
        delta <- -eta_r*f_i%o%Labels[i_choice,]
        W <- W+delta
      }'
      
      'sim_competitors <- sim_labels[setdiff(c(1:6),path_i_label)]
      omega <- NA
      if(any(sim_competitors>sim_labels[path_i_label])){
        competitor <- which.max(sim_labels)
        if(is.na(E1[competitor])){
          omega <- 1
          E1[competitor] <- -l_IN%*%Labels[competitor,]-1e-3 # if large overlap between prediction (l_IN) and competitor, then much suppression needs to take place
          
        }else{
          E_current <- -l_IN%*%Labels[competitor,]-1e-3
          omega <- 1/(1+exp(-(E_current/E1[competitor])))
        }
        eta_r <- as.vector(omega)*para["r"]
        delta <- -eta_r*f_i%o%Labels[competitor,]
        W <- W+delta
      }'
      if(i_choice!=path_i_label){
        eta_r <- as.vector(A)*para["r"]
        delta <- -eta_r*f_i%o%Labels[i_choice,]
        W <- W+delta
      }
      
      searchMat[i_run,] <- c(agent=agent,ospan=ospan,group=group,episode=i,phase=i_phase,
                             path=path_i_ID,nPath=nPath,nie=nie,p_error=p_error,choice_wrong_data=choice_wrong_data)
      
    }
    
    
    #if(agent==length(unique_codes)){end_time <- Sys.time(); runTime <- end_time-start_time; print(runTime)}
    
    }
    #print(sx)
  #}
  return(searchMat)
}

### This function/model wasn't used for the LAK paper, so I do not comment on it
BSB_searchNLearn_fx <- function(para){
  #para <- c(149,56,3.189366e-01,2.681809e+00,8.175618e-03,-1.355133e+03,1.000000e+00,1.171126e-01,9.272892e-01)
  #names(para) <- c("nCells_Label","nCells_Item","within_catNoise","c_label","g","e","R","r","out_intf")
  
  unique_codes <- unique(selection.df$code)
  #stopit <- 0
  #sx <- 0
  #while(stopit==0){
  #sx <- sx+1
  for(agent in 1:length(unique_codes)){
    
    if(agent==1){
      start_time <- Sys.time()
      #PA_df_sim <- NULL
      #PA_df_obs <- NULL
      #PA_sim_table <- matrix(rep(0,2*length(seq(0,1,0.01))),ncol=length(seq(0,1,0.01)))
      PA_sim_table <- matrix(rep(0,2*10),ncol=10)
      #colnames(PA_sim_table) <- seq(0,1,0.01)
      colnames(PA_sim_table) <- c(1:10)
      PA_obs_table <- PA_sim_table
      
      i_run <- 0 # current row in searchMat
      
      nEpisodes_perParti <- as.vector(rowSums(nEpisodes_perPhaseAndCode))
      nrows_sm <- sum(nEpisodes_perParti)
      var_toBeStored <- c("agent","ospan","group","episode","phase","path_obs","new_path_obs",
                          "nPath_obs","nie_obs","p_error_sim","choice_wrong_data",
                          "path_sim","new_path_sim","nPath_sim","nie_sim")
      searchMat <- matrix(rep(NA,length(var_toBeStored)*nrows_sm),nrow=nrows_sm)
      colnames(searchMat) <- var_toBeStored
      
      second_button.fx <- function(first_button,button_similarity){
        btn_vec <- sign(rnorm(para["nCells_Item"]))
        mask <- runif(para["nCells_Item"])<button_similarity
        return((1-mask)*btn_vec+mask*first_button)
      }
      
      F_loco1 <- sign(rnorm(para["nCells_Item"]))
      F_loco <- list(F_loco1,second_button.fx(first_button=F_loco1,button_similarity=0))
      
      F_size1 <- sign(rnorm(para["nCells_Item"]))
      F_size <- list(F_size1,second_button.fx(first_button=F_size1,button_similarity=0))
      
      F_skull1 <- sign(rnorm(para["nCells_Item"]))
      F_skull <- list(F_skull1,second_button.fx(first_button=F_skull1,button_similarity=0))
      
      paths <- list(path_1 = c(1,1,1), path_2 = c(1,1,2), path_3 = c(1,2,1), path_4 = c(1,2,2),
                    path_5 = c(2,1,1), path_6 = c(2,1,2), path_7 = c(2,2,1), path_8 = c(2,2,2))
      
      nItems_per_cat <- 10
      item.fx <- function(path.x,F_loco,F_size,F_skull,withinCatNoise,nItems_per_cat){
        f_cat.x <- c(F_loco[[path.x[1]]],F_size[[path.x[2]]],F_skull[[path.x[3]]])
        items_of_cat.mat <- matrix(NA,nItems_per_cat*length(f_cat.x),nrow=nItems_per_cat,ncol=length(f_cat.x))
        for(x in 1:nItems_per_cat){
          item.x.noise <- sample(c(-1,1),length(f_cat.x),prob=c(withinCatNoise,1-withinCatNoise),replace=T)
          f_item.x <- f_cat.x*item.x.noise
          items_of_cat.mat[x,] <- f_item.x
        }
        return(items_of_cat.mat)
      }
      items <- lapply(paths,item.fx,F_loco=F_loco,F_size=F_size,F_skull=F_skull,
                      withinCatNoise=para["within_catNoise"],nItems_per_cat=nItems_per_cat)
      
      Labels <- t(replicate(6,sign(rnorm(para["nCells_Label"]))))
      
      taxo.mapping.dino <- data.frame(path_ID=c(1:8),path_label=c(1,2,3,4,5,5,6,6))
      taxo.mapping.ptero <- data.frame(path_ID=c(1:8),path_label=c(1,1,2,2,3,4,5,6))
      
    }
    
    code_of_agent <- unique_codes[agent]
    trainingData_agent <- training_data_fx(code_of_agent,selection.df)
    var_agent <- c("phase","path_sim","nPath_sim","nie_sim")
    searchMat_agent <- matrix(rep(NA,length(var_agent)*nrow(trainingData_agent)),nrow=nrow(trainingData_agent))
    colnames(searchMat_agent) <- var_agent
    
    PA_df_agent <- NULL
    PA_df_parti <- NULL
    
    taxo_type <- as.vector(trainingData_agent$taxo_type)[1]
    if(taxo_type=="taxo.dino"){taxo.mapping <- taxo.mapping.dino}else{taxo.mapping <- taxo.mapping.ptero}
    ospan <- trainingData_agent$ospan[1]
    group <- as.numeric(as.vector(trainingData_agent$group[1]))
    
    if(is.element("Dorygnathusathus",trainingData_agent$dino_name)){
      trainingData_agent[trainingData_agent$dino_name=="Dorygnathusathus","dino_name"] <- "Dorygnathus"
    }
    nPhases <- max(as.numeric(trainingData_agent$phase))
    if(nPhases==7){
      if(oSpan_Scores_df[which(oSpan_Scores_df$code==code_of_agent),"accu.outlier"]){
        for(i in 1:nrow(trainingData_agent)){
          
          path_i_fPast_obs <- rep(0,8); names(path_i_fPast_obs) <- SubNames
          path_i_fPast_sim <- rep(0,8); names(path_i_fPast_sim) <- 1:8
          
          i_run <- i_run+1
          i_phase <- as.numeric(as.vector(trainingData_agent$phase[i]))
          
          if(i==1){
            W <- matrix(0,nrow=para["nCells_Item"]*3,ncol=para["nCells_Label"])
            E_vec <- rep(0,6)
            path_i_sim <- rbinom(3,1,.5)+1; names(path_i_sim) <- c("f1","f2","f3")
          }else{
            p_labels_search <- exp(E_vec*para["PA"])/sum(exp(E_vec*para["PA"]))
            label_in_focus <- sample(c(1:6),1,prob=p_labels_search)
            l_search <- Labels[label_in_focus,]
            f_IN <- as.vector(sign(l_search%*%t(W)))
            
            fx.navigation <- function(f_IN,cell.range,dim_x){
              D.fx <- function(x,y=f_IN[cell.range]){return(sqrt(sum((x-y)^2)))}
              D <- unlist(lapply(dim_x,D.fx))
              sim_turns <- exp(-1*(D^2))
              if(sum(sim_turns)>0){
                p_turns <- sim_turns/sum(sim_turns)
                turn.sampled <- sample(c(1,2),1,prob = p_turns)
              }else{
                turn.sampled <- sample(c(1,2),1)
              }
              return(turn.sampled)
            }
            cell.range_1 <- 1:para["nCells_Item"]; 
            cell.range_2 <- (para["nCells_Item"]+1):(para["nCells_Item"]*2); 
            cell.range_3 <- (para["nCells_Item"]*2+1):(para["nCells_Item"]*3)
            path_i_turn1 <- c(fx.navigation(f_IN,cell.range=cell.range_1,F_loco))
            path_i_turn2 <- c(fx.navigation(f_IN,cell.range=cell.range_2,F_size))
            path_i_turn3 <- c(fx.navigation(f_IN,cell.range=cell.range_3,F_skull))
            
            path_i_sim <- c(f1=path_i_turn1,f2=path_i_turn2,f3=path_i_turn3)
          }
          
          path_i_sim_ID <- sum(abs(1-path_i_sim)*c(4,2,1))+1
          path_i_sim_label <- taxo.mapping[path_i_sim_ID,"path_label"]
          path_i_sim_subname <- SubNames[path_i_sim_ID]
          
          path_i <- trainingData_agent[i,c("f1","f2","f3")]+1
          path_i_ID <- sum(abs(1-path_i)*c(4,2,1))+1
          path_i_label <- taxo.mapping[path_i_ID,"path_label"]
          path_i_subname <- as.vector(trainingData_agent[i,"dino_name"])
          
          choice_wrong_data <- 1-trainingData_agent$labelCorrect[i]
          
          if(i>1){
            path_i_prevEpisodes <- which(as.vector(trainingData_agent[1:(i-1),"dino_name"])==path_i_subname)
            nPath <- length(path_i_prevEpisodes)
            
            fPast_obs <- round(prop.table(table(trainingData_agent[1:(i-1),"dino_name"])),2)
            if(is.element("Dorygnathusathus",names(fPast_obs))){fPast_obs <- fPast_obs[-which(names(fPast_obs)=="Dorygnathusathus")]}
            path_i_fPast_obs[match(names(fPast_obs),names(path_i_fPast_obs))] <- fPast_obs
            path_chosen_obs <- rep(0,8)
            path_chosen_obs[which(SubNames==path_i_subname)] <- 1
            PA_df_obs_i <- data.frame(names(path_i_fPast_obs),as.vector(path_i_fPast_obs),path_chosen_obs)
            colnames(PA_df_obs_i) <- c("subname","f_past","chosen")
            PA_df_parti <- rbind(PA_df_parti,PA_df_obs_i)
            
            if(nPath>0){
              nie <- i-max(path_i_prevEpisodes)-1
              new_path_obs <- 0
            }else{
              nie <- NA
              new_path_obs <- 1
            }
            
            path_i_sim_prevEpisodes <- which(searchMat_agent[1:(i-1),"path_sim"]==path_i_sim_ID)
            nPath_sim <- length(path_i_sim_prevEpisodes)
            if(nPath_sim>0){
              nie_sim <- i-max(path_i_sim_prevEpisodes)-1
              new_path_sim <- 0
            }else{
              nie_sim <- NA
              new_path_sim <- 1
            }
            fPast_sim <- round(prop.table(table(searchMat_agent[1:(i-1),"path_sim"])),2)
            path_i_fPast_sim[match(names(fPast_sim),names(path_i_fPast_sim))] <- fPast_sim
            path_chosen_sim <- rep(0,8)
            path_chosen_sim[path_i_sim_ID] <- 1
            PA_df_i <- data.frame(names(path_i_fPast_sim),as.vector(path_i_fPast_sim),path_chosen_sim)
            colnames(PA_df_i) <- c("path_ID","f_past","chosen")
            PA_df_agent <- rbind(PA_df_agent,PA_df_i)
          }else{
            nPath <- 0
            nie <- NA
            nPath_sim <- 0
            nie_sim <- NA
            new_path_obs <- 1
            new_path_sim <- 1
          }
          
          f_i <- items[[path_i_sim_ID]][sample(c(1:nItems_per_cat),1),]
          
          ## labelling
          l_IN <- sign(as.vector(f_i%*%W))
          # output interference
          W <- W+rnorm(length(W),sd=para["out_intf"])
          #if(sum(l_IN)!=0){l_IN <- l_IN/sqrt(sum(l_IN^2))}
          fx.sim_labels <- function(l_IN,Labels){
            if(sum(l_IN)!=0){
              D.fx <- function(x,y=l_IN){return(sqrt(sum((x-y)^2)))}
              D <- apply(Labels,1,D.fx)
              sim_labels <- exp(-para["c_label"]*(D^2))
              if(length(which(sim_labels<0))>0){sim_labels[which(sim_labels<0)] <- 0}
            }else{
              sim_labels <- rep(1/nrow(Labels),nrow(Labels))
            }
            return(sim_labels)
          }
          sim_labels <- fx.sim_labels(l_IN,Labels)
          
          if(sum(sim_labels)==0){
            p_choices <- rep(1/nrow(Labels),nrow(Labels))
          }else{
            p_choices <- sim_labels/sum(sim_labels)
          }
          i_choice <- sample(c(1:6),1,prob=p_choices)
          p_error <- 1-p_choices[path_i_sim_label]
          
          ## encoding
          l <- Labels[path_i_sim_label,] # v = true label presented during feedback
          E <- -l%*%l_IN # E = novelty
          E_vec[path_i_sim_label] <- -E
          A <- 1 / ( 1 + exp(-(E-para["e"])*para["g"]))
          eta <- c(as.matrix(A))*para["R"]
          delta <- eta*f_i%o%l
          W <- W+delta
          # removal
          
          if(i_choice!=path_i_label){
            eta_r <- as.vector(A)*para["r"]
            delta <- -eta_r*f_i%o%Labels[i_choice,]
            W <- W+delta
          }
          
          searchMat[i_run,] <- c(agent=agent,ospan=ospan,group=group,episode=i,phase=i_phase,
                                 path_obs=path_i_ID,new_path_obs=new_path_obs,nPath_obs=nPath,
                                 nie_obs=nie,p_error_sim=p_error,choice_wrong_data=choice_wrong_data,
                                 path_sim=path_i_sim_ID,new_path_sim=new_path_sim,nPath_sim=nPath_sim,nie_sim=nie_sim)
          searchMat_agent[i,] <- c(phase=i_phase,path_sim=path_i_sim_ID,nPath_sim=nPath_sim,nie_sim=nie_sim)
          
        }
        if(is.element(1,PA_df_agent$f_past)){PA_df_agent <- PA_df_agent[-which(PA_df_agent$f_past==1),]}
        #M_PA_agent <- aggregate(chosen~f_past,PA_df_agent,mean)
        #PA_df_sim <- rbind(PA_df_sim,M_PA_agent)
        if(length(unique(searchMat_agent[,"path_sim"]))!=1){
          PA_df_agent$propCat <- as.numeric(cut(PA_df_agent$f_past,seq(0,1,0.1),include.lowest = T))
          PA_agent_table <- table(PA_df_agent$chosen,PA_df_agent$propCat)
          PA_sim_table[,match(as.numeric(colnames(PA_agent_table)),as.numeric(colnames(PA_sim_table)))] <-
            PA_sim_table[,match(as.numeric(colnames(PA_agent_table)),as.numeric(colnames(PA_sim_table)))]+PA_agent_table
        }
        
        #print(PA_sim_table)
        if(is.element(1,PA_df_parti$f_past)){PA_df_parti <- PA_df_parti[-which(PA_df_parti$f_past==1),]}
        #M_PA_parti <- aggregate(chosen~f_past,PA_df_parti,mean)
        PA_df_parti$propCat <- as.numeric(cut(PA_df_parti$f_past,seq(0,1,0.1),include.lowest = T))
        PA_parti_table <- table(PA_df_parti$chosen,PA_df_parti$propCat)
        PA_obs_table[,match(as.numeric(colnames(PA_parti_table)),as.numeric(colnames(PA_obs_table)))] <-
          PA_obs_table[,match(as.numeric(colnames(PA_parti_table)),as.numeric(colnames(PA_obs_table)))]+PA_parti_table
        #PA_df_obs <- rbind(PA_df_obs,M_PA_parti)
        
      }
    }
    
    #if(agent==length(unique_codes)){end_time <- Sys.time(); runTime <- end_time-start_time; print(runTime)}
    
  }
  #print(sx)
  #}
  
  #PA_df_sim$propCat <- as.numeric(cut(PA_df_sim$f_past,seq(0,1,0.1),include.lowest = T))
  #PA_sim <- aggregate(chosen~propCat,PA_df_sim,mean)
  
  #PA_df_obs$propCat <- as.numeric(cut(PA_df_obs$f_past,seq(0,1,0.1),include.lowest = T))
  #PA_obs <- aggregate(chosen~propCat,PA_df_obs,mean)
  
  return(list(searchMat=searchMat,PA_obs_table=PA_obs_table,PA_sim_table=PA_sim_table))
}

BSB_sNl2_fx <- function(para,nSubjects){
  #para <- c(149,56,3.189366e-01,2.681809e+00,8.175618e-03,-1.355133e+03,1.000000e+00,1.171126e-01,9.272892e-01)
  #names(para) <- c("nCells_Label","nCells_Item","within_catNoise","c_label","g","e","R","r","out_intf")
  
  unique_codes <- unique(selection.df$code)
  code_of_agent <- sample(unique_codes,1)
  
  for(agent in 1:nSubjects){
    
    if(agent==1){
      start_time <- Sys.time() # To stop the time it takes to simulate one experiment
      PA_sim_table <- matrix(rep(0,2*10),ncol=10) # matrix to store the simulated data
      colnames(PA_sim_table) <- c(1:10)
      
      taxo.mapping <- data.frame(path_ID=c(1:8),path_label=c(1,2,3,4,5,5,6,6)) # mapping path number (1-8) on category label (1-6)
      
      i_run <- 0 # current row in searchMat, which is another object to store simulated data (see below)
      
      nEpisodes_perParti <- as.vector(rowSums(nEpisodes_perPhaseAndCode)) # the object 'nEpisodes_perPhaseAndCode' ...
      # ... represents the number of learning episodes/trials each participant has performed in the real experiment for ...
      # ... each of the seven study/search phases. Thus, by summarizing the episodes across the seven phases ('rowSums'),...
      # ... you get the total number of Episodes for every agent/pariticpant.
      
      #### with the following function and for-loop, you use the empirical means and SDs of the ...
      # ... students' real nEpisodes to draw, for every simulated agent/participant, ... 
      # ... simulated nEpisodes from a normal distribution ('rnorm()')
      fx_nEpisodesPerPhase <- function(phase_x){
        nEpisodes_phase_x <- 0
        while(nEpisodes_phase_x==0){
          n_phase <- round(rnorm(1,mean=nEpisodes_perPhase_M[phase_x],sd=nEpisodes_perPhase_SD[phase_x]))
          if(n_phase>0){nEpisodes_phase_x <- n_phase}
        }
        return(nEpisodes_phase_x)
      }
      mat_nEpisodesPerPhase <- matrix(rep(NA,nSubjects*7),ncol=7)
      for(sx in 1:nSubjects){
        nEpisodes_perPhase_agent <- sapply(c(1:7),fx_nEpisodesPerPhase)
        mat_nEpisodesPerPhase[sx,] <- nEpisodes_perPhase_agent
      }
      ####
      
      #### create another object to store simulated data ('searchMat') ############################################
      nrows_sm <- sum(rowSums(mat_nEpisodesPerPhase))
      var_toBeStored <- c("agent","episode","phase","path_sim","new_path_sim","nPath_sim","nie_sim","p_error_sim")
      searchMat <- matrix(rep(NA,length(var_toBeStored)*nrows_sm),nrow=nrows_sm)
      colnames(searchMat) <- var_toBeStored
      #############################################################################################################
      
      #### Generate/simulate learning material (taxonomy) ################################################################
      ## The function second_button.fx generates feature vectors/patterns for the eight buttons (to navigate the taxonomy)
      second_button.fx <- function(first_button,button_similarity){
        btn_vec <- sign(rnorm(para["nCells_Item"]))
        mask <- runif(para["nCells_Item"])<button_similarity
        return((1-mask)*btn_vec+mask*first_button)
      }
      ## Feature vectors/patterns for level 1-buttons
      F_loco1 <- sign(rnorm(para["nCells_Item"]))
      F_loco <- list(F_loco1,second_button.fx(first_button=F_loco1,button_similarity=0))
      ## Feature vectors/patterns for level 2-buttons
      F_size1 <- sign(rnorm(para["nCells_Item"]))
      F_size <- list(F_size1,second_button.fx(first_button=F_size1,button_similarity=0))
      ## Feature vectors/patterns for level 3-buttons
      F_skull1 <- sign(rnorm(para["nCells_Item"]))
      F_skull <- list(F_skull1,second_button.fx(first_button=F_skull1,button_similarity=0))
      ## e.g., path_1 = c(1,1,1), which means that the agent needs to click on the first button on every level to reach category 1
      paths <- list(path_1 = c(1,1,1), path_2 = c(1,1,2), path_3 = c(1,2,1), path_4 = c(1,2,2),
                    path_5 = c(2,1,1), path_6 = c(2,1,2), path_7 = c(2,2,1), path_8 = c(2,2,2))
      #####################################################################################################################
      
      #### Generate/simulate learning material (dino pictures) ############################
      ## The function item.fx generates feature vectors/patterns for the 8*10 dino pictures
      nItems_per_cat <- 10 # In the experiment, there were 10 pictures per category
      item.fx <- function(path.x,F_loco,F_size,F_skull,withinCatNoise,nItems_per_cat){
        f_cat.x <- c(F_loco[[path.x[1]]],F_size[[path.x[2]]],F_skull[[path.x[3]]])
        items_of_cat.mat <- matrix(NA,nItems_per_cat*length(f_cat.x),nrow=nItems_per_cat,ncol=length(f_cat.x))
        for(x in 1:nItems_per_cat){
          item.x.noise <- sample(c(-1,1),length(f_cat.x),prob=c(withinCatNoise,1-withinCatNoise),replace=T)
          f_item.x <- f_cat.x*item.x.noise
          items_of_cat.mat[x,] <- f_item.x
        }
        return(items_of_cat.mat)
      }
      items <- lapply(paths,item.fx,F_loco=F_loco,F_size=F_size,F_skull=F_skull,
                      withinCatNoise=para["within_catNoise"],nItems_per_cat=nItems_per_cat)
      ######################################################################################
      
      #### Generate/simulate learning material (labels) #############
      Labels <- t(replicate(6,sign(rnorm(para["nCells_Label"]))))
      ###############################################################
    }
    
    phase_of_episode_vec <- unlist(sapply(c(1:7),function(i){rep(i,mat_nEpisodesPerPhase[agent,i])})) # a vector with as many cells as ...
    # ... there are trials/episodes for the given agent, each cell representing the current search phase
    nEpisodes <- length(phase_of_episode_vec)
    
    ###### Further objects to store siulated data, this time for the individual agent ########
    var_agent <- c("phase","path_sim","nPath_sim","nie_sim")
    searchMat_agent <- matrix(rep(NA,length(var_agent)*nEpisodes),nrow=nEpisodes)
    colnames(searchMat_agent) <- var_agent
    
    PA_df_agent <- NULL
    ##########################################################################################
    
    ###### Looping through all nEpisodes: searching, labeling, learning through feedback
    for(i in 1:nEpisodes){
      
      i_run <- i_run+1
      i_phase <- phase_of_episode_vec[i]
      
      if(i==1){
        W <- matrix(0,nrow=para["nCells_Item"]*3,ncol=para["nCells_Label"]) # Generate item-to-label association matrix W, which is set to zero at the very beginning
        E_vec <- rep(0,6) # Generate novelty vector, with every category's novelty set to zero at the very beginning. Note that...
        # ... Novelty is used here to guide the agent's search decisions.
        path_i_sim <- rbinom(3,1,.5)+1; names(path_i_sim) <- c("f1","f2","f3") # if i=1, the first path (search decision) is randomly generated ('rbinom')
      }else{
        p_labels_search <- exp(E_vec*para["PA"])/sum(exp(E_vec*para["PA"])) # for every label/category, determine the probability of being the agent's search goal.
        label_in_focus <- sample(c(1:6),1,prob=p_labels_search) # Agent chooses the current search goal, i.e., label_in_focus
        l_search <- Labels[label_in_focus,] # retrieve the feature pattern of the label_in_focus
        f_IN <- as.vector(sign(l_search%*%t(W))) # the label_in_focus' feature pattern provides input to the item layer of W
        
        ## Function, which takes as input f_IN to select one of the two buttons on each of the three taxonomy levels
        fx.navigation <- function(f_IN,cell.range,dim_x){
          D.fx <- function(x,y=f_IN[cell.range]){return(sqrt(sum((x-y)^2)))}
          D <- unlist(lapply(dim_x,D.fx))
          sim_turns <- exp(-1*(D^2))
          if(sum(sim_turns)>0){
            p_turns <- sim_turns/sum(sim_turns)
            turn.sampled <- sample(c(1,2),1,prob = p_turns)
          }else{
            turn.sampled <- sample(c(1,2),1)
          }
          return(turn.sampled)
        }
        cell.range_1 <- 1:para["nCells_Item"]; 
        cell.range_2 <- (para["nCells_Item"]+1):(para["nCells_Item"]*2); 
        cell.range_3 <- (para["nCells_Item"]*2+1):(para["nCells_Item"]*3)
        path_i_turn1 <- c(fx.navigation(f_IN,cell.range=cell.range_1,F_loco)) # turn1 = either 1 (first button) or 2 (second button)
        path_i_turn2 <- c(fx.navigation(f_IN,cell.range=cell.range_2,F_size))
        path_i_turn3 <- c(fx.navigation(f_IN,cell.range=cell.range_3,F_skull))
        
        path_i_sim <- c(f1=path_i_turn1,f2=path_i_turn2,f3=path_i_turn3)
      }
      
      path_i_sim_ID <- sum(abs(1-path_i_sim)*c(4,2,1))+1
      path_i_sim_label <- taxo.mapping[path_i_sim_ID,"path_label"]
      path_i_sim_subname <- SubNames[path_i_sim_ID]
      
      if(i>1){
        
        path_i_sim_prevEpisodes <- which(searchMat_agent[1:(i-1),"path_sim"]==path_i_sim_ID) # path chosen in the previous episode/trial
        nPath_sim <- length(path_i_sim_prevEpisodes) # how often has path_i been chosen in previous trials (1 to i-1)
        if(nPath_sim>0){
          nie_sim <- i-max(path_i_sim_prevEpisodes)-1 # nie = number of intervening episodes (see LAK'20 paper)
          new_path_sim <- 0 # 0 means that path_i is not new, i.e., has been chosen before
        }else{
          nie_sim <- NA
          new_path_sim <- 1
        }
        path_i_fPast_sim <- rep(0,8); names(path_i_fPast_sim) <- 1:8 # vector that represents for every path the path's choice frequency in the past
        fPast_sim <- round(prop.table(table(searchMat_agent[1:(i-1),"path_sim"])),2)
        path_i_fPast_sim[match(names(fPast_sim),names(path_i_fPast_sim))] <- fPast_sim
        
        path_chosen_sim <- rep(0,8) # vector that represents which of the eight paths has been chosen (which is set to 1)
        path_chosen_sim[path_i_sim_ID] <- 1
        
        PA_df_i <- data.frame(names(path_i_fPast_sim),as.vector(path_i_fPast_sim),path_chosen_sim) # store outcome of the ith search phase
        colnames(PA_df_i) <- c("path_ID","f_past","chosen")
        PA_df_agent <- rbind(PA_df_agent,PA_df_i)
      }else{
        nPath_sim <- 0
        nie_sim <- NA
        new_path_sim <- 1
      }
      
      ## the main result of the previous code in this for-loop is the selected path: path_i. Based on this information, ...
      # ... the process of labelling can start. To this end, the item feature pattern f_i is retrieved evoked by the randomly ...
      # ... sampled picture beloning to the category associated with path_i.
      f_i <- items[[path_i_sim_ID]][sample(c(1:nItems_per_cat),1),]
      
      ## labelling
      l_IN <- sign(as.vector(f_i%*%W)) # probing W with f_i to generate an activation pattern on the label layer, i.e., l_IN
      # output interference
      W <- W+rnorm(length(W),sd=para["out_intf"])
      
      # the function fx.sim_labels calculates the similarity (sim_labels) between l_IN and the feature patterns on the six category labels
      fx.sim_labels <- function(l_IN,Labels){
        if(sum(l_IN)!=0){
          D.fx <- function(x,y=l_IN){return(sqrt(sum((x-y)^2)))}
          D <- apply(Labels,1,D.fx)
          sim_labels <- exp(-para["c_label"]*(D^2))
          if(length(which(sim_labels<0))>0){sim_labels[which(sim_labels<0)] <- 0}
        }else{
          sim_labels <- rep(1/nrow(Labels),nrow(Labels))
        }
        return(sim_labels)
      }
      sim_labels <- fx.sim_labels(l_IN,Labels)
      
      # based on sim_labels, determine the probability of choosing one of the six labels for the presented picture f_i
      if(sum(sim_labels)==0){
        p_choices <- rep(1/nrow(Labels),nrow(Labels))
      }else{
        p_choices <- sim_labels/sum(sim_labels)
      }
      i_choice <- sample(c(1:6),1,prob=p_choices) # the chosen label in trial/episode i
      p_error <- 1-p_choices[path_i_sim_label] # 1 minus the probability of choosing the label assigned to the currently selected path/picture
      
      ## encoding (adjusting the item-to-label association matrix)
      l <- Labels[path_i_sim_label,] # v = true label presented during feedback
      E <- -l%*%l_IN # E = novelty (here, the (negative) inner product between the correct label pattern l and the predicted label pattern l_IN is used to determine the experienced novelty)
      E_vec[path_i_sim_label] <- -E # storing the current novelty signal associated with the chosen path in the novelty vector
      ### determining the extent of change (delta) within W ###
      A <- 1 / ( 1 + exp(-(E-para["e"])*para["g"]))
      eta <- c(as.matrix(A))*1 # R set to 1
      delta <- eta*f_i%o%l
      W <- W+delta
      #########################################################
      
      #### removal of erroneous associations (in case of an incorrect response) ####
      if(i_choice!=path_i_sim_label){
        eta_r <- as.vector(A)*para["r"]
        delta <- -eta_r*f_i%o%Labels[i_choice,]
        W <- W+delta
      }
      ##############################################################################
      
      ### Storing relevant information of trial/episode i
      searchMat[i_run,] <- c(agent=agent,episode=i,phase=i_phase,
                             path_sim=path_i_sim_ID,new_path_sim=new_path_sim,nPath_sim=nPath_sim,nie_sim=nie_sim,
                             p_error_sim=p_error)
      searchMat_agent[i,] <- c(phase=i_phase,path_sim=path_i_sim_ID,nPath_sim=nPath_sim,nie_sim=nie_sim)
      
    }
    ### End of episode i and storage of relevant information in the agent's data frame
    if(is.element(1,PA_df_agent$f_past)){PA_df_agent <- PA_df_agent[-which(PA_df_agent$f_past==1),]}
    if(length(unique(searchMat_agent[,"path_sim"]))!=1){
      PA_df_agent$propCat <- as.numeric(cut(PA_df_agent$f_past,seq(0,1,0.1),include.lowest = T))
      PA_agent_table <- table(PA_df_agent$chosen,PA_df_agent$propCat)
      PA_sim_table[,match(as.numeric(colnames(PA_agent_table)),as.numeric(colnames(PA_sim_table)))] <-
        PA_sim_table[,match(as.numeric(colnames(PA_agent_table)),as.numeric(colnames(PA_sim_table)))]+PA_agent_table
    }
    
  }
  PA_sim_table <- PA_sim_table[,1:9]
  return(list(searchMat=searchMat,PA_sim_table=PA_sim_table))
}

BSB_lOnlyIndi_fx <- function(para,subject){
  
  #subject="AK03K2"
  subject_data <- selection.df[selection.df$code==subject,]
  subject_ospan <- subject_data$oSpan.score[1]
  subject_group <- as.numeric(subject_data$WMC.group[1])
  subject_taxo <- as.vector(subject_data$taxo.type[1])
  
  if(subject_taxo=="taxo.dino"){taxo.mapping <- data.frame(path_ID=c(1:8),path_label=c(1,2,3,4,5,5,6,6))}else{
    taxo.mapping <- data.frame(path_ID=c(1:8),path_label=c(1,1,2,2,3,4,5,6))}
  
  nEpisodes <- nrow(subject_data)
  phase_of_episode_vec <- as.numeric(subject_data$catPhases)
  
  var_toBeStored <- c("ospan","group","episode","phase","path",
                      "p_error_sim","choice_wrong_data")
  searchMat <- matrix(rep(NA,length(var_toBeStored)*nEpisodes),nrow=nEpisodes)
  colnames(searchMat) <- var_toBeStored
  
  for(i in 1:nEpisodes){
    if(i==1){
      second_button.fx <- function(first_button,button_similarity){
        btn_vec <- sign(rnorm(para["nCells_Item"]))
        mask <- runif(para["nCells_Item"])<button_similarity
        return((1-mask)*btn_vec+mask*first_button)
      }
      
      F_loco1 <- sign(rnorm(para["nCells_Item"]))
      F_loco <- list(F_loco1,second_button.fx(first_button=F_loco1,button_similarity=0))
      
      F_size1 <- sign(rnorm(para["nCells_Item"]))
      F_size <- list(F_size1,second_button.fx(first_button=F_size1,button_similarity=0))
      
      F_skull1 <- sign(rnorm(para["nCells_Item"]))
      F_skull <- list(F_skull1,second_button.fx(first_button=F_skull1,button_similarity=0))
      
      paths <- list(path_1 = c(1,1,1), path_2 = c(1,1,2), path_3 = c(1,2,1), path_4 = c(1,2,2),
                    path_5 = c(2,1,1), path_6 = c(2,1,2), path_7 = c(2,2,1), path_8 = c(2,2,2))
      
      nItems_per_cat <- 10
      item.fx <- function(path.x,F_loco,F_size,F_skull,withinCatNoise,nItems_per_cat){
        f_cat.x <- c(F_loco[[path.x[1]]],F_size[[path.x[2]]],F_skull[[path.x[3]]])
        items_of_cat.mat <- matrix(NA,nItems_per_cat*length(f_cat.x),nrow=nItems_per_cat,ncol=length(f_cat.x))
        for(x in 1:nItems_per_cat){
          item.x.noise <- sample(c(-1,1),length(f_cat.x),prob=c(withinCatNoise,1-withinCatNoise),replace=T)
          f_item.x <- f_cat.x*item.x.noise
          items_of_cat.mat[x,] <- f_item.x
        }
        return(items_of_cat.mat)
      }
      items <- lapply(paths,item.fx,F_loco=F_loco,F_size=F_size,F_skull=F_skull,
                      withinCatNoise=para["within_catNoise"],nItems_per_cat=nItems_per_cat)
      
      Labels <- t(replicate(6,sign(rnorm(para["nCells_Label"]))))
      
      W <- matrix(0,nrow=para["nCells_Item"]*3,ncol=para["nCells_Label"])
      
    }
    
    i_phase <- phase_of_episode_vec[i]
    i_choice_wrong <- 1-subject_data$matchNomatch[i]
    
    path_i <- subject_data[i,c("f1","f2","f3")]+1
    path_i_ID <- sum(abs(1-path_i)*c(4,2,1))+1
    path_i_label <- taxo.mapping[path_i_ID,"path_label"]
    
    f_i <- items[[path_i_ID]][sample(c(1:nItems_per_cat),1),]
    ## labelling
    l_IN <- sign(as.vector(f_i%*%W))
    # output interference
    W <- W+rnorm(length(W),sd=para["out_intf"])
    fx.sim_labels <- function(l_IN,Labels){
      if(sum(l_IN)!=0){
        D.fx <- function(x,y=l_IN){return(sqrt(sum((x-y)^2)))}
        D <- apply(Labels,1,D.fx)
        sim_labels <- exp(-para["c_label"]*(D^2))
        if(length(which(sim_labels<0))>0){sim_labels[which(sim_labels<0)] <- 0}
      }else{
        sim_labels <- rep(1/nrow(Labels),nrow(Labels))
      }
      return(sim_labels)
    }
    sim_labels <- fx.sim_labels(l_IN,Labels)
    if(sum(sim_labels)==0){
      p_choices <- rep(1/nrow(Labels),nrow(Labels))
    }else{
      p_choices <- sim_labels/sum(sim_labels)
    }
    p_error <- 1-p_choices[path_i_label]    
    
    ## encoding
    l <- Labels[path_i_label,] # v = true label presented during feedback
    E <- -l%*%l_IN # E = novelty
    A <- 1 / ( 1 + exp(-(E-para["e"])*para["g"]))
    eta <- c(as.matrix(A))*1 # R set to 1
    delta <- eta*f_i%o%l
    W <- W+delta
    # removal
    if(i_choice_wrong==1){
      eta_r <- as.vector(A)*para["r"]
      delta <- -eta_r*f_i%o%Labels[path_i_label,]
      W <- W+delta
    }
    
    searchMat[i,] <- c(subject_ospan,subject_group,i,i_phase,
                           path_i_ID,p_error,i_choice_wrong)
  }
  return(list(searchMat=searchMat))
}



