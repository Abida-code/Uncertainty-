rm(list=ls())
clearhistory <- function() {
  write("", file=".blank")
  loadhistory(".blank")
  unlink(".blank")
}
clearhistory()

setwd("C:/Users/Kasutaja/Desktop/Cognitive Science/Ncategory learning/uncetainty/CODE")
source("emp_data.R")
setwd("C:/Users/Kasutaja/Desktop/Cognitive Science/Ncategory learning/uncetainty/CODE")
source("Fx2.R")

theta <- c(nCells_Label=55, nCells_Item=22, within_catNoise=0.379990685244312, 
           c_label=2.45923677208422, g=0.00806616352756813, e=-966.910821446676, r=0.744528127506688,
           out_intf=1.82609086348269,PA=0.12364360248288)
sNL2_Run_Fx <- function(theta,opt=TRUE){
  
  file_fitness <- "fitness_sNL2_q1.txt"
  PE_M_emp <- PE_M_emp
  PE_SEM_emp <- PE_SEM_emp
  RI_M_emp <- RI_M_emp
  RI_SEM_emp <- RI_SEM_emp
  PA_M_emp <- PA_M_emp
  PA_SEM_emp <- PA_SEM_emp
  ER_M_emp <- ER_M_emp
  ER_SEM_emp <- ER_SEM_emp
  
  # Fitting aggregated data of entire sample: All parameters free
  nCells_Label=ceiling(theta[1])
  nCells_Item=ceiling(theta[2])
  within_catNoise=theta[3]
  c_label=theta[4]
  g=theta[5]
  e=theta[6]
  r=theta[7]
  out_intf=theta[8]
  PA=theta[9]
  
  '# Fitting aggregated group data
  nCells_Label=55
  nCells_Item=22
  within_catNoise=0.378971417306412
  c_label=theta[1]
  g=0.00542150718057834
  e=-1119.84653017379
  r=theta[2]
  out_intf=1.79119253099364
  PA=0.118887469297919'
  
  para <- c(nCells_Label,nCells_Item,within_catNoise,
            c_label,g,e,r,out_intf,PA)
  names(para) <- c("nCells_Label","nCells_Item","within_catNoise",
                   "c_label","g","e","r","out_intf","PA")
  
  for(rx in 1:1){
    if(rx==1){sim_data <- {}}
    set.seed(42)
    sim_data <- rbind(sim_data,BSB_sNl2_fx(para=para,nSubjects=110))
  }
  
  searchMat_sim <- sim_data[,"searchMat"]$searchMat
  
  PE_M_sim <- aggregate(p_error_sim~phase,searchMat_sim,mean)[,2]
  RMSD_PE <- sqrt(sum((PE_M_sim-PE_M_emp)^2)/length(PE_M_emp))
  
  n_data_RI <- 4 # above ten, variance gets large
  RI_M_sim <- aggregate(p_error_sim~nie_sim,searchMat_sim,mean)[1:n_data_RI,2]
  if(any(is.na(RI_M_sim))){RI_M_sim[is.na(RI_M_sim)] <- 1}
  RMSD_RI <- sqrt(sum((RI_M_sim-RI_M_emp[1:n_data_RI])^2)/n_data_RI)
  
  PA_sim_table_run <- sim_data[,"PA_sim_table"]$PA_sim_table
  PA_sim_denom <- colSums(PA_sim_table_run)
  if(is.element(0,PA_sim_denom)){
    PA_sim_denom[which(PA_sim_denom==0)] <- 1e+10
  }
  PA_M_sim <- PA_sim_table_run[2,]/PA_sim_denom
  RMSD_PA <- sqrt(sum((PA_M_sim-PA_M_emp)^2)/length(PA_M_emp))
  
  ER_M_sim <- aggregate(new_path_sim~phase,searchMat_sim,mean)[,2]
  RMSD_ER <- sqrt(sum((ER_M_sim-ER_M_emp)^2)/length(ER_M_emp))
  
  RMSD <- RMSD_PE+RMSD_RI+RMSD_PA+RMSD_ER
  
  plotfx <- function(){
    par(mfrow=c(2,2))
    plot(PE_M_emp,ylim=c(0,1),ylab="P(E)",xlab="Study Phase")
    arrowFx <- function(M,SEM,x,colx){arrows(x, M+SEM,x,M-SEM,lwd=.5,code=3,angle=90,length=.05,col=colx)}
    arrowFx(M=PE_M_emp,SEM=PE_SEM_emp,x=1:7,col=1)
    points(PE_M_sim,pch=15)
    abline(h=5/6)
    text(2,0.9,paste("RMSD",round(RMSD_PE*1000)/1000))
    
    plot(RI_M_emp[1:4],ylim=c(0,1),ylab="P(E)",xlab="NIE")
    arrowFx(M=RI_M_emp[1:n_data_RI],SEM=RI_SEM_emp[1:n_data_RI],x=1:n_data_RI,col=1)
    abline(h=5/6)
    points(RI_M_sim,pch=15,ylim=c(0,1))
    text(1.5,0.9,paste("RMSD",round(RMSD_RI*1000)/1000))
    
    plot(PA_M_emp,ylim=c(0,1),ylab="P(Selected)",xlab="Past Frequency",xaxt="n")
    arrowFx(M=PA_M_emp,SEM=PA_SEM_emp,x=1:9,col=1)
    axis(1,c(1:9),c(0,0.1,0.3,0.4,0.5,0.6,0.7,0.8,0.9))
    points(PA_M_sim,pch=15)
    text(3,.9,paste("RMSD",round(RMSD_PA*1000)/1000))
    
    plot(ER_M_emp,ylim=c(0,1),ylab="P(New Path)",xlab="Study Phase")
    arrowFx(M=ER_M_emp,SEM=ER_SEM_emp,x=1:7,col=1)
    points(ER_M_sim,pch=15)
    text(3,.9,paste("RMSD",round(RMSD_ER*1000)/1000))
    
  }
  
  if(opt==FALSE){
    plotfx()
    return(list(
      PE_sim=PE_M_sim,
      RI_sim=RI_M_sim,
      PA_sim=PA_M_sim,
      ER_sim=ER_M_sim
    ))
  }else{
    setwd("C:/Users/Kasutaja/Desktop/Cognitive Science/Ncategory learning/uncetainty/CODE")
    if(!is.element(file_fitness,list.files())){
      file <- file_fitness
      write.table(t(c(theta,RMSD)), file = file,col.names = F, row.names = FALSE,append=T) 
      plotfx()
    }else{
      setwd("C:/Users/Kasutaja/Desktop/Cognitive Science/Ncategory learning/uncetainty/CODE")
      file <- file_fitness
      fitness_values <- read.table(file)
      fitness_values <- fitness_values[,ncol(fitness_values)]
      if(min(fitness_values)>RMSD){
        write.table(t(c(theta,RMSD)), file = file,col.names = FALSE, row.names = FALSE,append=T) 
        plotfx()
      }
    }
    return(RMSD)
  }
}

### Making Figures for LAK paper
## Plotting aggregated data
sim_data <- sNL2_Run_Fx(theta=theta,opt=FALSE)
# PA Curve
pSize=1.2
par(mfrow=c(1,1))
pdf(width=8,height=8,pointsize=20,file="Diagram_a.pdf")
par(mar=c(4.5,4.5,1,1))
plot(PA_M_emp,ylim=c(0,1),ylab=expression('Choice Probability '*italic('P')*'(i)'),
     xlab=expression('Past Frequency of Concept '*italic('i')),
     cex=pSize,cex.lab=1.5,cex.axis=1.5,xaxt="n")
axis(1,c(1:9),round(seq(0,0.9,length=9),1),cex.axis=1.5)
arrowFx <- function(M,SEM,x,colx){arrows(x, M+SEM,x,M-SEM,lwd=1,code=3,angle=90,length=.05,col=colx)}
arrowFx(M=PA_M_emp,SEM=PA_SEM_emp,x=1:10,col=1)
legend(1,1,c("Data","Model"),pch=c(1,16),cex=1.5,box.lwd=0,pt.cex=pSize)
points(sim_data$PA_sim,pch=16,cex=pSize)
dev.off() 

# ER Curve
par(mfrow=c(1,1))
pdf(width=8,height=8,pointsize=20,file="Diagram_b.pdf")
par(mar=c(4.5,4.5,1,1))
pSize=1.2
plot(ER_M_emp,ylim=c(0,1),ylab=expression(italic('P')*'(new)'),
     xlab="Time (Study Phase)",cex=pSize,cex.lab=1.5,cex.axis=1.5)
arrowFx(M=ER_M_emp,SEM=ER_SEM_emp,x=1:7,col=1)
points(sim_data$ER_sim,pch=16,cex=pSize)
dev.off() 

# Learning Curve: PE as a function of Study Phase
par(mfrow=c(1,1))
pdf(width=8,height=8,pointsize=20,file="Diagram_c.pdf")
par(mar=c(4.5,4.5,1,1))
pSize=1.2
plot(PE_M_emp,ylim=c(0,1),ylab=expression('Error Probability '*italic('P')*'(E)'),
     xlab="Time (Study Phase)",cex=pSize,cex.lab=1.5,cex.axis=1.5)
arrowFx(M=PE_M_emp,SEM=PE_SEM_emp,x=1:7,col=1)
points(sim_data$PE_sim,pch=16,cex=pSize); abline(h=5/6,lty=2)
dev.off() 
# legend(5,0.8,c("Data","Model"),pch=c(1,16),cex=1.2,box.lwd=0)
# Interference Curve: PE as a function of NIC
par(mfrow=c(1,1))
pdf(width=8,height=8,pointsize=20,file="Diagram_d.pdf")
par(mar=c(4.5,4.5,1,1))
pSize=1.2
plot(RI_M_emp,ylim=c(0,1),ylab=expression('Error Probability '*italic('P')*'(E)'),
     xlab=expression(italic('NIC')* ('Number Intervening Cycles')),
     cex=pSize,cex.lab=1.5,xaxt="n",cex.axis=1.5)
axis(1,c(1:4),c(0:3),cex.axis=1.5)
arrowFx(M=RI_M_emp,SEM=RI_SEM_emp,x=1:7,col=1)
points(sim_data$RI_sim,pch=16,cex=pSize); abline(h=5/6,lty=2)
dev.off() 
#legend(5,0.8,c("Data","Model"),pch=c(1,16),cex=.8,box.lwd=0)


## Fitting group data (starting from average parameter estimates determined through GA)
'library(dfoptim)
start_para <- c(c_label=1.38772625902958, r=0.624301468857425) 
opt_out <- nmkb(par=start_para,
                lower = c(0,0), upper = c(4,2),
                fn=function(theta){sNL2_Run_Fx(theta)}, 
                control=list(trace=1))
opt_out$par'

# without bounds
start_para <- c(c_label=1.38772625902958, r=0.624301468857425)
opt_out <- optim(par=start_para,
                 fn=function(theta){sNL2_Run_Fx(theta)},
                 control=list(trace=1)
)
opt_out$convergence
opt_out$par



## GA
library(GA)
#nr_emp <- c(EP1,EP2)
GA2 <- ga(type = "real-valued", fitness = sNL2_Run_Fx,
          lower = c(2,1,0,0,0,-2000,0,0,0), upper = c(150,100,0.4,3,0.01,0,1,2,1),
          crossover  = gareal_blxCrossover, maxiter = 1000,
          names = c("nCells_Label","nCells_Item","within_catNoise",
                    "c_label","g","e","r","out_intf","PA"))
# pcrossover  by default set to 0.5
# popSize by default set to 10 times the number of decision variables
summary(GA2)

