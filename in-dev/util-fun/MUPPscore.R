#setwd () ## Change working directory 

library(LearnBayes)
N=2000  ## Change sample size
PAIRS=20 ##Change number of pairs

P=29  ## number of quadrature points (ONLY advanced users are allowed to change) 
PS=841  ##square the P (ONLY advanced users are allowed to change) 

Parameters=read.csv("Parameters.csv",header=T) #####

Response=read.csv("Response.csv",header=T) #####
NewResponse=Response[rep(1:nrow(Response),each=PS),] 

prior=t(t(seq(-3.5,3.5,.25)))
PriorSForOne=t(t(prior[rep(1:nrow(prior),each=P),]))
PriorS=PriorSForOne[rep(1:PS,N)] 

PriorT=prior[rep(1:P,N*P),]
PriorTForOne=prior[rep(1:P,P),] 

mu=c(0,0)
Sigma=matrix(c(1,0.2,0.2,1),2,2) ##### change sigma prior？

PriorST=matrix(c(PriorS,PriorT),,2)
ProbS=dnorm(PriorS, mu, Sigma, log=FALSE)
ProbT=dnorm(PriorT, mu, Sigma, log=FALSE)
ProbST=dmnorm(PriorST, mu, Sigma, log=FALSE)

OneItemResponse=matrix(,nrow=(PS*N),ncol=1)
PickedProb=matrix(,nrow=(PS*N),ncol=PAIRS)
ThetaNumerator=matrix(,nrow=N,ncol=1)
ThetaDenominator=matrix(,nrow=N,ncol=1)
Theta=matrix(,nrow=N,ncol=1)
StdErrorNumeratorAll=matrix(,nrow=(PS*N),ncol=1)
StdErrorNumerator=matrix(,nrow=N,ncol=1)
StdError=matrix(,nrow=N,ncol=1)
ProbSgreaterTIRF=matrix(,nrow=PS,ncol=1)

for (i in 1:PAIRS)
{
  AlphaS=Parameters[(2*i-1),1]
  BetaS=Parameters[(2*i-1),2]
  TauS=Parameters[(2*i-1),3]
  AlphaT=Parameters[(2*i),1]
  BetaT=Parameters[(2*i),2]
  TauT=Parameters[(2*i),3]
  
  GammaS=1+exp(AlphaS*(3*(PriorS-BetaS)))+exp(AlphaS*((PriorS-BetaS)-TauS))+exp(AlphaS*(2*(PriorS-BetaS)-TauS))
  GammaT=1+exp(AlphaT*(3*(PriorT-BetaT)))+exp(AlphaT*((PriorT-BetaT)-TauT))+exp(AlphaT*(2*(PriorT-BetaT)-TauT))
  PS1=(exp(AlphaS*((PriorS-BetaS)-TauS))+exp(AlphaS*(2*(PriorS-BetaS)-TauS)))/GammaS
  PS0=(1+exp(AlphaS*(3*(PriorS-BetaS))))/GammaS
  PT1=(exp(AlphaT*((PriorT-BetaT)-TauT))+exp(AlphaT*(2*(PriorT-BetaT)-TauT)))/GammaT
  PT0=(1+exp(AlphaT*(3*(PriorT-BetaT))))/GammaT
  ProbSgreaterT=(PS1*PT0)/(PS1*PT0+PS0*PT1)
  ProbSlessT=(PS0*PT1)/(PS1*PT0+PS0*PT1)
  
  GammaSIRF=1+exp(AlphaS*(3*(PriorSForOne-BetaS)))+exp(AlphaS*((PriorSForOne-BetaS)-TauS))+exp(AlphaS*(2*(PriorSForOne-BetaS)-TauS))
  GammaTIRF=1+exp(AlphaT*(3*(PriorTForOne-BetaT)))+exp(AlphaT*((PriorTForOne-BetaT)-TauT))+exp(AlphaT*(2*(PriorTForOne-BetaT)-TauT))
  PS1IRF=(exp(AlphaS*((PriorSForOne-BetaS)-TauS))+exp(AlphaS*(2*(PriorSForOne-BetaS)-TauS)))/GammaSIRF
  PS0IRF=(1+exp(AlphaS*(3*(PriorSForOne-BetaS))))/GammaSIRF
  PT1IRF=(exp(AlphaT*((PriorTForOne-BetaT)-TauT))+exp(AlphaT*(2*(PriorTForOne-BetaT)-TauT)))/GammaTIRF
  PT0IRF=(1+exp(AlphaT*(3*(PriorTForOne-BetaT))))/GammaTIRF
  ProbSgreaterTIRF=(PS1IRF*PT0IRF)/(PS1IRF*PT0IRF+PS0IRF*PT1IRF)
  Table=matrix(ProbSgreaterTIRF,ncol=P)
  colnames(Table)=c(seq(-3.5,3.5,.25))
  rownames(Table)=c(seq(-3.5,3.5,.25))
  #IRF_Table=paste("IRFTable_",i,".csv",sep="")
  #write.csv(Table,file=IRF_Table)
  
  OneItemResponse=NewResponse[,i]
  
  for (j in 1:(PS*N))
  {     
    if(OneItemResponse[j]==1){
      PickedProb[j,i]=ProbSgreaterT[j]}
    if(OneItemResponse[j]==0){
      PickedProb[j,i]=ProbSlessT[j]}
    else if (OneItemResponse[j]==9){
      PickedProb[j,i]=1}
  }
}

rowProds=function(X){apply(X,1,FUN="prod")}
LVfA=rowProds(PickedProb)
LVf=LVfA*ProbST

Af=ProbST

NumeratorRaw=t(t(PriorS*LVf*Af))

DenominatorRaw=t(t(LVf*Af))

for (k in 1:N)
{
  ThetaNumerator[k]=sum(NumeratorRaw[(PS*(k-1)+1):(PS*(k-1)+PS),])
  ThetaDenominator[k]=sum(DenominatorRaw[(PS*(k-1)+1):(PS*(k-1)+PS),])
  Theta[k]=ThetaNumerator[k]/ThetaDenominator[k]
}

Theta=t(t(Theta))
ThetaAll=Theta[rep(1:nrow(Theta),each=PS),]
StdErrorNumeratorAll=t(t(Af*LVf*((PriorS-ThetaAll)^2)))

for (m in 1:N)
{
  StdErrorNumerator[m]=sum(StdErrorNumeratorAll[(PS*(m-1)+1):(PS*(m-1)+PS),])
  ThetaDenominator[m]=sum(DenominatorRaw[(PS*(m-1)+1):(PS*(m-1)+PS),])
  StdError[m]=sqrt(StdErrorNumerator[m]/ThetaDenominator[m])
}

TraitScores=cbind(Theta,StdError)
#colnames(TraitScores)=c("Theta","StdError")
#write.csv(TraitScores,file="TraitScores.csv")

Reliability=1-(median(StdError^2)/(var(Theta)+median(StdError^2)))
#colnames(Reliability)=c("Reliability")
#write.csv(Reliability,file="ScaleReliability.csv")

