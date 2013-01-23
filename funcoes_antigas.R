#####################################################
### Ecovirtual - funções não utilizadas no pacote ###
#####################################################


###########################
### Island Biogeography ###
###########################

## species-area relationship
sppArea=function(c , z){
          x11()
          curve(expr = c*x^z , from=1, to=10^10, xlab="Area", ylim=c(1,500),
                ylab="Species number", font.lab=2, lwd=2, col=2, main=paste("c = ",c,"; z = ",z))
          curve(expr = c*x^z , from=1, to=10^10, xlab="Area", ylim=c(1,500),
                ylab="Species number", font.lab=2, lwd=2, col=2, main=paste("c = ",c,"; z = ",z), log="xy")
}

#par(mfrow=c(2,2))
#sppArea(c = 1.5 , z = .25)
#sppArea(c = 2.1 , z = .25)

## chuva de sementes
iRain=function(Nspp , chuva , abund , tempo){
          spp=paste("sp.",1:Nspp)
          ilha=numeric()
          riq=numeric()
          for(i in 1:tempo){
                    ilha=union(unique(sample(spp,chuva,replace=TRUE,prob=abund/sum(abund))),ilha)
                    riq[i]=length(ilha)
          }
          x11()
          plot(0:tempo,c(0,riq),type="l",lwd=2,bty='n',xlab="time",ylab="number of species",
               font.lab=2,col=2,ylim=c(0,Nspp),main=c("Island richness",paste("(Nspp=",Nspp," ; rain=",chuva,")")))
          abline(h=Nspp,lty=3)
          invisible(riq)
}

#iRain(Nspp=10, chuva=10, abund=c(10,10,10,10,10,10,10,10,10,10), tempo=10)


## colonização em relação a tamanho da ilha x spp
iCol=function(areas, Nspp, chuva.total, abund, tempo){
          n.ilhas=length(areas)
          spp=paste("sp.",1:Nspp)
          ilhas=paste("Island",1:n.ilhas)
          chuva=round(chuva.total*areas/sum(areas))
          riq=numeric()
          par(mfrow=c(ceiling(sqrt(n.ilhas)),ceiling(sqrt(n.ilhas))))
          for(i in 1:n.ilhas){
                    riq[i]=iRain(Nspp,chuva[i],abund,tempo)[tempo]
          }
          names(riq)=ilhas
          mod=lm(log10(riq)~log10(areas))
          x11()
          plot(areas,riq,log="xy",font.lab=2,pch=16,col=2,bty="l",
               main=paste("N Islands=",n.ilhas,"; N spp=",Nspp,"; time=",tempo),
               xlab="Island area",ylab="Number of species",ylim=c(1,Nspp))
          abline(mod,lty=2)
          cat("c=",mod[[1]][1],"z=",mod[[1]][2],"\n")
          invisible(riq)
}

#iCol(areas=c(10,20,40,80),Nspp=1000,chuva.total=100,abund=rep(10,1000),tempo=10)


## colonização e extincao tamanho da ilha x sp
iColExt=function(Nspp, chuva, abund, tempo, tx.ext){
          spp=paste("sp.",1:Nspp,sep="")
          ilha=numeric()
          riq=numeric()
          ilha=unique(sample(spp,chuva,replace=TRUE,prob=abund/sum(abund)))
          riq[1]=length(ilha)
          
          for(i in 2:tempo){
                    rr=sample(c("V","M"),length(ilha),replace=TRUE,prob=c((1-tx.ext),tx.ext))
                    roleta=data.frame(ilha,rr)
                    vivos=roleta[roleta$rr=="V",1]
                    ilha=union(sample(spp,chuva,replace=TRUE,prob=abund/sum(abund)),vivos)
                    riq[i]=length(unique(ilha))
          }
          x11()
          plot(0:tempo,c(0,riq),type="l",lwd=2,bty='n',xlab="time",ylab="number of species",
               font.lab=2,col=2,ylim=c(0,Nspp),
               main=c("Island Richness",paste("Nspp =",Nspp," ; rain =",chuva,"; tx.ext = ",tx.ext)))
          abline(h=Nspp,lty=3)
          invisible(riq)
}

#iColExt(Nspp=100, chuva=5, abund=rep(100,100), tempo=100, tx.ext=.1)


## biogeografia de ilha
MW=function(areas , dist , P , a=1, b=-.01, c=1, d=-.01){
          par(mfrow=c(1,2))
          E=a+b*areas
          I=c+d*dist
          S=numeric()
          for(i in 1:length(areas)){S[i]=I[i]*P/(I[i]+E[i])}
          Tn=numeric()
          for(i in 1:length(areas)){Tn[i]=I[i]*E[i]/(I[i]+E[i])}
          
          curve(I[1]-I[1]*x/P,0,P,bty="n",xaxt="n",yaxt="n",xlab="Species number",
                ylab="Rates",font.lab=2,lwd=2,ylim=c(0,1))
          curve((E[1]/P)*x,0,P,lwd=2,add=TRUE)	
          
          abline(v=0)
          abline(h=0)
          mtext("P",side=1,at=P,font=2)
          mtext("I1",side=2,at=I[1],font=2,las=1)
          mtext("E1",side=4,at=E[1],font=2,las=1)
          mtext("S1",side=1,at=S[1],font=2,col=2)
          mtext("T1",side=2,at=Tn[1],font=2,las=1,col=2)
          points(S[1],Tn[1],col=2,pch=16,cex=1.3)
          segments(S[1],Tn[1],S[1],0,lty=3,col=2)
          segments(S[1],Tn[1],0,Tn[1],lty=3,col=2)
          
          for(i in 2:length(areas)){
                    curve(I[i]-I[i]*x/P,0,P,lwd=2,ylim=c(0,1),add=TRUE,lty=i)
                    curve((E[i]/P)*x,0,P,lwd=2,add=TRUE,lty=i)
                    mtext(paste("I",i,sep=""),side=2,at=I[i],font=2,las=1)
                    mtext(paste("E",i,sep=""),side=4,at=E[i],font=2,las=1)
                    mtext(paste("S",i,sep=""),side=1,at=S[i],font=2,col=i+1)
                    mtext(paste("T",i,sep=""),side=2,at=Tn[i],font=2,las=1,col=i+1)
                    points(S[i],Tn[i],col=i+1,pch=16,cex=1.3)
                    segments(S[i],Tn[i],S[i],0,lty=3,col=i+1)
                    segments(S[i],Tn[i],0,Tn[i],lty=3,col=i+1)
          }
          ex=data.frame(areas=areas,spp=S,dist=dist)
          y=lm(S~areas)[[1]][1]
          z=lm(S~areas)[[1]][2]	
          plot(spp~areas,data=ex,log="xy",font.lab=2,pch=as.character(1:length(areas)),col=2,bty="l",
               main=c("Equilibrium",paste("c = ",round(y,2),"; z = ",round(z,2))),
               xlab="Island area",ylab="Species number",ylim=c(1,P))
          abline(lm(log10(spp)~log10(areas),data=ex),lty=3)
          invisible(ex)
          par(mfrow=c(1,2))
}


## Island Biog. Plus Rescue Effect and Internal Colonization  
MW.2.0=function(areas , dist , P , peso.A=.5 , a=1, b=-.01, c=1, d=-.01, e=0, f=.01, g=0, h=.01){
          E=((a+b*areas)*peso.A+(g+h*dist)*(1-peso.A))/(peso.A+(1-peso.A))
          I=((c+d*dist)*peso.A+(e+f*areas)*(1-peso.A))/(peso.A+(1-peso.A))
          S=numeric()
          for(i in 1:length(areas)){S[i]=I[i]*P/(I[i]+E[i])}
          Tn=numeric()
          for(i in 1:length(areas)){Tn[i]=I[i]*E[i]/(I[i]+E[i])}
          curve(I[1]-I[1]*x/P,0,P,bty="n", xaxt="n",yaxt="n",xlab="Species number",	 ylab="Rates",font.lab=2,lwd=2,ylim=c(0,1),main="Equilibrium")
          curve((E[1]/P)*x,0,P,lwd=2,add=TRUE)
          abline(v=0)
          abline(h=0)
          mtext("P",side=1,at=P,font=2)
          mtext("I1",side=2,at=I[1],font=2,las=1)
          mtext("E1",side=4,at=E[1],font=2,las=1)
          mtext("S1",side=1,at=S[1],font=2,col=2)
          mtext("T1",side=2,at=Tn[1],font=2,las=1,col=2)
          points(S[1],Tn[1],col=2,pch=16,cex=1.3)
          segments(S[1],Tn[1],S[1],0,lty=3,col=2)
          segments(S[1],Tn[1],0,Tn[1],lty=3,col=2)
          for(i in 2:length(areas)){
                    curve(I[i]-I[i]*x/P,0,P,lwd=2,ylim=c(0,1),add=TRUE,lty=i)
                    curve((E[i]/P)*x,0,P,lwd=2,add=TRUE,lty=i)
                    mtext(paste("I",i,sep=""),side=2,at=I[i],font=2,las=1)
                    mtext(paste("E",i,sep=""),side=4,at=E[i],font=2,las=1)
                    mtext(paste("S",i,sep=""),side=1,at=S[i],font=2,col=i+1)
                    mtext(paste("T",i,sep=""),side=2,at=Tn[i],font=2,las=1,col=i+1)
                    points(S[i],Tn[i],col=i+1,pch=16,cex=1.3)
                    segments(S[i],Tn[i],S[i],0,lty=3,col=i+1)
                    segments(S[i],Tn[i],0,Tn[i],lty=3,col=i+1)
          }
          ex=data.frame(areas=areas,spp=S,dist=dist)
          y=lm(S~areas)[[1]][1]
          z=lm(S~areas)[[1]][2]
          plot(spp~areas,data=ex,log="xy",font.lab=2,pch=16,col=2,bty="l",
               main=c("Species-area relationship",paste("c = ",round(y,2),"; z = ",round(z,2))),
               xlab="Island area",ylab="Species number",ylim=c(1,P))
          abline(lm(log10(spp)~log10(areas),data=ex),lty=3)
          invisible(ex)
}


###########################
### Population Dynamics ###
###########################

## Exponetial Growth
crescExp <- function(N0,lambda,r, tmax) 
{
          #r=log(lambda)
          resulta <- matrix(rep(NA,3*tmax),nrow=tmax)
          colnames(resulta)<-c("time","Nd","Nc")
          resulta[,1] <- seq(0,tmax-1)
          resulta[1,2:3] <- N0
          for (t in 2:tmax) 
          {
                    #print(t)
                    #print(r)
                    resulta[t,2] <- N0*(exp(r*(t-1))) 
                    resulta[t,3] <- N0*(lambda^(t-1)) 
          }
          x11()
          plot(resulta[,1],resulta[,2],type="l",lty=2, main= "Exponential Growth", xlab="time(t)", ylab="population size (N)", col="red")
          points(resulta[,1],resulta[,3])
          legend("topleft",c("discrete growth","continuous"),lty=c(2,NA_integer_),pch=c(NA_integer_, 1), col=c(2,1), bty="n")
          text(x=tmax*0.4, y= resulta[(tmax/2),2], paste("r=", r), col="blue")
          invisible(resulta)
}

#crescExp(N0=100,lambda=1.05,r=log(1.05), tmax=20)


### logistical growth
discrLog<-function(N0, rd, K, tmax)
{
          Nt=c(N0,numeric(tmax))
          for(t in 2: (tmax+1))
          {
                    Nt[t]=Nt[t-1] + (rd * Nt[t-1]* (1-(Nt[t-1]/K))) 
          }
          return(Nt)
}

#discrLog(N0=10, rd=0.05, K=80, tmax=100)


## Chaos
atrBif=function(N0, K, tmax, nrd,maxrd=3)
{
          rd.s=seq(1,maxrd,length=nrd)
          r1=sapply(rd.s, function(x){discrLog(N0=N0, rd=x, K=100,tmax=200)})
          r2=stack(as.data.frame(r1))
          names(r2)=c("N", "old.col")
          r2$rd=rep(rd.s,each=tmax+1)
          r2$time=rep(0:tmax, nrd)
          res.bif=subset(r2, time>0.5*tmax)
          plot(N~rd, data=res.bif, pch=".", cex=2)
}

#atrBif(N0=50,K=100,tmax=200,nrd=500, maxrd=3)


## Atractors
crescAtr<-function( N0, lambda,varl,rd,K, tmax)
{
          resulta=matrix(0, ncol=3, nrow=tmax)
          resulta[,1]=1:tmax
          colnames(resulta)=c("time", "exp.estocastic", "logist.discrete")
          resulta[1,c(2,3)]= N0
          for(t in 2: tmax)
          {
                    nt.exp=resulta[(t-1),2]
                    nt.log=resulta[(t-1),3]
                    resulta[t,3]=nt.log + (rd * nt.log* (1-(nt.log/K)))
                    lamb.est=rnorm(1,lambda,sd=sqrt(varl))
                    resulta[t,2]=nt.exp * lamb.est
                    if(resulta[t,2]<1)
                    {
                              resulta[t,2]=0
                    }
          }
          x11()
          op <- par(mfrow = c(2, 2)) # 2 x 2 pictures on one plot
          plot(resulta[,1],resulta[,2],main="Exponential growth", sub=paste("lamb= ", lambda, "  var= ",
                                                                            varl), type="l",lty=2,xlab="Time (t)", ylab="Population size (N)")
          #tmax <- dim(resulta)[1] 
          plot(resulta[1:(tmax-1),2],resulta[2:tmax,2],type="l",col="red",xlab="N[t]",ylab="N[t+1]")
          points(resulta[1:(tmax-1),2],resulta[2:tmax,2],pch=20)
          plot(resulta[,1],resulta[,3],type="l", main="Logistic Growth", sub=paste(" rd= ",  rd), xlab="Time (t)", ylab="Population size (N)")
          plot(resulta[1:(tmax-1),3],resulta[2:tmax,3],type="l",col="red",xlab="N[t]",ylab="N[t+1]")
          points(resulta[1:(tmax-1),3],resulta[2:tmax,3],pch=20)
          par(op)
          invisible(resulta)
}

#crescAtr(N0=610, lambda=1.1,varl=0.05,rd=2.99,K=600, tmax=100)


## survival
sobrevive=function(p.mort,N0)
{
          res=rep(0,N0)
          for(i in 1: N0)
          {
                    conta=0
                    while(sample(c("m","v"),prob=c(p.mort,1-p.mort),size=1)=="v")
                    {
                              conta=conta+1
                    }
                    res[i]=conta  
          }
          return(res)
}

#sobrevive(0.5,200)


##########################################
### Internal fuctions not used anymore ###
##########################################

## anima 
anima <-function(dados)
{
          x11()
          for(i in 1:dim(dados)[3])
          {
                    image(dados[,,i], main=("Metapopulation"),sub=paste("simulation no.= ", i), col=c("white","red"), bty="n",xaxt='n',yaxt='n')
                    grid(dim(dados)[1],dim(dados)[2])
                    Sys.sleep(.2)
          }
}

## grFreq
grFreq=function(E , I , P){
          S = I*P/(I+E) ; T = I*E/(I+E)
          curve(I-I*x/P,0,P,bty="n",xaxt="n",yaxt="n",xlab="Species number",
                ylab="Taxas",font.lab=2,lwd=2,ylim=c(0,1))
          curve((E/P)*x,0,P,lwd=2,add=T)
          abline(v=0)
          abline(h=0)
          mtext("P",side=1,at=P,font=2)
          mtext("I",side=2,at=I,font=2,las=1)
          mtext("E",side=4,at=E,font=2,las=1)
          mtext("S",side=1,at=S,font=2,col=2)
          mtext("T",side=2,at=T,font=2,las=1,col=2)
          points(S,T,col=2,pch=16,cex=1.3)
          segments(S,T,S,0,lty=3,col=2)
          segments(S,T,0,T,lty=3,col=2)
}


### Trade-off Multispecies Graphic
grToff=function(rq, fsp1,pe,add=FALSE,...)
{
          #          rq <- as.numeric(tclvalue(rqVar))
          #	fsp1 <- as.numeric(tclvalue(fsp1Var))
          #	pe <- as.numeric(tclvalue(peVar))
          rank=1:rq
          ci= pe/(1-fsp1)^(2*rank-1)
          px= fsp1*(1-fsp1)^(rank-1)
          if(add==FALSE)
          {
                    toff<-x11( width=5, height=5)
          }
          old<-par(mar=c(3,3,3,3))
          plot(ci~rank, col="red",ylim=c(0,max(ci)*1.2), type="b", ann=FALSE, axes=FALSE,)
          axis(4, cex.axis=0.8, col.axis='red', col='red')#, yaxp=c(0,3,3))
          par(new=TRUE)
          plot(px~rank, ylim=c(0,fsp1),type="b", bty="n",  ann=FALSE, cex.axis=0.8)#yaxt="n", xaxp=c(0,10,5))
          #axis(2, cex.axis=0.8)#, yaxp=c(0,0.2,4))
          mtext("Specie competitive rank", 1, 2, cex=0.9)
          mtext("Abundance", 2, 2, cex=0.9)
          mtext("Colonization rate", 4, 2, cex=0.9, col='red')
          mtext("Trade-off Species Rank ", 3, 0, cex=1.2)
          par(old)
}
#grToff(rq =  10 , fsp1 =  0.2 , pe =  0.1 ,add=FALSE)



###########################
### Community Functions ###
###########################

## Virtual Community with ambiental gradients
grCom=function(medias, desvios, minimo, maximo)
{
          nsp=length(medias)
          cor=rainbow(nsp)
          x11()
          curve(dnormTrunc(x, medias[1], desvios[1], maximo=maximo, minimo=minimo),from=minimo, to=maximo, ylim=c(0,1), ylab="Population Density", xlab="Gradient Value", main="Species Distribution", col=cor[1])
          for (i in 2:nsp)
          {
                    curve(dnormTrunc(x, medias[i], desvios[i], maximo=maximo, minimo=minimo),from=minimo, to=maximo,add=TRUE, col=cor[i], lty=2)
          } 
          text(medias+1, dnormTrunc(medias, medias, desvios,maximo=maximo,minimo=minimo)+0.5, labels=(paste("sp",(1:(nsp)),sep="_")), col=cor, cex=0.8)
}  

#grCom(medias=c(2,3,4,5,6,7,8), desvios=c(1,1,1,1,1,1,1), minimo=0, maximo=10)
#grCom(media=sample(seq(from=1.5, to=19.5,by=0.25), size=10), runif(10,0.5,2.5), minimo=0, maximo=20)
#grCom(medias=sample(2:19, size=10),desvios=sample(seq(from=0.5, to=2.5, by=0.1),10), minimo=1, maximo=20)


## Internal function for 'grCom'
dnormTrunc=function(x, minimo=-Inf, maximo=Inf, media=0, desvio=1)
{
          res=numeric(length(x))
          x.prov=dnorm(x,mean=media, sd=desvio)
          ampl.norm=pnorm(maximo,mean=media, sd=desvio)-pnorm(minimo,mean=media, sd=desvio)
          x.prov/ampl.norm
}


## Proportion of species at each sample
probS=function(medias, desvios, amostra, minimo, maximo)
{
          nsp=length(medias)
          namostra=length(amostra)
          resulta=matrix(NA, nrow=nsp, ncol=namostra)
          rownames(resulta)=paste("sp", 1:nsp, sep="_")
          colnames(resulta)=paste("plot", 1:namostra, sep="_")
          for(k in 1:namostra)
          {
                    for(i in 1:nsp)
                    {
                              resulta[i,k]= pnormTrunc(amostra[k]+1,minimo=minimo, maximo=maximo, media=medias[i], desvio=desvios[i])- pnormTrunc(amostra[k],minimo=minimo, maximo=maximo, media=medias[i], desvio=desvios[i] )
                    }
          }
          invisible(resulta)
}


## Internal Function used in 'probS'
pnormTrunc=function(x,minimo=-Inf, maximo=Inf, media=0, desvio=1)
{
          denom <- pnorm(maximo, mean=media, sd=desvio) - pnorm(minimo, mean=media, sd=desvio)
          qtmp <- pnorm(x, mean=media, sd=desvio) - pnorm(minimo, mean=media, sd=desvio)
          qtmp/denom
}


## Bray-curtis Distance index
distBC<-function(dados)
{
          nplot=dim(dados)[2]
          similar=matrix(NA,ncol=nplot,nrow=nplot)
          rownames(similar)<-paste("plot", c(1:nplot))
          colnames(similar)<-paste("plot", c(1:nplot))
          for(i in 1:(nplot-1))
          {
                    m=i+1
                    for(m in m:nplot)
                    {
                              bc.dist=sum(abs(dados[,i]-dados[,m]))/(sum (dados[,c(i,m)]))
                              similar[m,i]=bc.dist
                    }
          }
          invisible(round(similar,3))
}


## Polar-Ordination Function
ordenaPolar=function(dist)
{
          somadist1.cont=apply(dist, 1, sum, na.rm=TRUE) + apply(dist,2,sum, na.rm=TRUE)
          nomes.parc=names(somadist1.cont)
          parc.ax=nomes.parc[somadist1.cont==max(somadist1.cont)][1]
          dist.ax=rbind(dist[,parc.ax], dist[parc.ax,])
          dist.ax=apply(dist.ax,2,sum, na.rm=TRUE)
          max.ax=max(dist.ax)
          parc.bx=nomes.parc[dist.ax==max.ax]
          if(length(parc.bx)>1)
          {
                    somamax.bx=max(somadist1.cont[parc.bx])
                    parc.bx=nomes.parc[somadist1.cont==somamax.bx][1]
                    parc.bx
          }
          dist.bx=rbind(dist[,parc.bx], dist[parc.bx,])
          dist.bx=apply(dist.bx,2,sum, na.rm=TRUE)
          xi= (max.ax^2 + dist.ax^2 - dist.bx^2)/(2*max.ax)
          yi=sqrt((dist.ax)^2-xi^2)
          yi[parc.bx]=max(dist.ax)
          op.xy=data.frame(xi,yi)
          plot(op.xy, pch=19, col=rainbow(length(xi)), xlim=c(-0.1, 1), ylim=c(-0.1,1), main="Polar Ordination", sub="Bray-Curtis distance")
          text(op.xy-0.05, labels=rownames(op.xy))
          invisible(op.xy)
}


### Similarity Matrix Function
sim<-function(dados, indice="bc")
{
          nplot=dim(dados)[2]
          similar=matrix(1,ncol=nplot,nrow=nplot)
          rownames(similar)<-paste("plot", c(1:nplot))
          colnames(similar)<-paste("plot", c(1:nplot))
          for(i in 1:(nplot-1))
          {
                    m=i+1
                    for(m in m:nplot)
                    {
                              if(indice=="jacc")
                              {
                                        dados[dados>0]=1
                                        co.oc=sum(dados[,i]>0 & dados[,m]>0)
                                        total.sp=sum(dados[,i])+sum(dados[,m])-co.oc
                                        similar[i,m]=co.oc/total.sp 
                                        similar[m,i]=co.oc/total.sp
                              }
                              if(indice=="bc") 
                              {
                                        bc.sim=sum(apply(dados[,c(i,m)], 1, min))/(sum (dados[,c(i,m)]))
                                        similar[i,m]=bc.sim
                                        similar[m,i]=bc.sim
                              }
                    }
          }
          invisible(round(similar,3))
}



#############################
### Multispecies function ###
#############################

## Sucessional stages matrix
sucMatrix=function(mat_trans, init_prop, ln, cl, tmax)
{
          mat_trans=as.matrix(mat_trans)
          porc1=apply(mat_trans,2,sum)
          if(sum(porc1!=1)>0)
          {
                    stop("the transition for each fase should sum 1: there is no extintion of area under the model")
          }
          if(sum(init_prop)!=1 | length(init_prop) != dim(mat_trans)[2])
          {
                    stop("the initial proportion of ocupance should sum 1 and the number of stages should be iqual to transition matrix")
          }
          nfase=dim(mat_trans)[1]
          ncel=ln*cl
          fase_n=round(init_prop*ncel)
          cl_fase=colorRampPalette(c("gray","yellow", "orange","green"))
          #cores=c("#ffffff",colors(nfase-1))
          #cores=terrain.colors(nfase)
          arena=matrix(NA,nrow=ln,ncol=cl)
          #resulta=matrix(0,ncol=nfase, nrow=tmax)
          pais=array(0,dim=c(ln, cl, tmax))
          #image(0:ln,0:cl, matrix(0,nrow=ln,ncol=cl), col="white", xlab="", ylab="")
          #grid(ln,cl)
          n0=sample(rep(0:(nfase-1), fase_n))
          arena[1:ncel]<-n0
          #t.n0=table(n0)
          #resulta[1,as.numeric(names(t.n0))+1]<-t.n0
          pais[,,1]<-arena
          image(0:ln, 0:cl, arena, col=cl_fase(nfase) , breaks=c(-0.1,(1:nfase)-0.1), xlab="", ylab="", main="Sucessional Model")
          grid(ln,cl)
          for (tc in 2:tmax)
          {
                    for(nf in 0:(nfase-1))
                    {
                              nf_vf=pais[,,(tc-1)]==nf
                              contn=sum(nf_vf)
                              pais[,,tc][nf_vf]<-sample(0:(nfase-1),contn,replace=TRUE, prob=as.numeric(mat_trans[,(nf+1)]))
                    }
                    #	resulta[tc,as.numeric(names(t_n0))+1]<-t_n0
                    image(0:ln, 0:cl, pais[,,tc], col=cl_fase(nfase) , breaks=c(-0.1,(1:nfase)-0.1), add=TRUE)
                    Sys.sleep(.1)
          }
          x11()
          op=par(mfrow=c(2,2))
          image(0:ln, 0:cl, arena, col=cl_fase(nfase) , breaks=c(-0.1,(1:nfase)-0.1), xlab="", ylab="", main="Ernitial Conditions")
          for(ts in c(4,2,1))
          {
                    image(0:ln, 0:cl, pais[,,round(tc/ts)], col=cl_fase(nfase) , breaks=c(-0.1,(1:nfase)-0.1), main=paste("Cicle", round(tc/ts)))
          }
          par(op)
          x11()
          resulta=t(apply(pais,3, table))
          matplot(resulta, type="l", ylim=c(min(resulta)*0.8, max(resulta)*1.1), main="Stage Distribution",xlab="Number of patchs", col=cl_fase(nfase), lty=2, lwd=2)
          legend("topright", legend=paste("Stage", 1:nfase), lty=2, lwd=2, col=cl_fase(nfase), bty="n", cex=0.8)
          #resulta=as.data.frame(resulta)
          eigs_st=eigen(mat_trans)
          dom_pos=which.max(Re(eigs_st$values))
          stage_v<- Re(eigs_st[["vectors"]][, dom_pos])
          stage_stable=(stage_v/sum(stage_v))*ncel
          abline(h=stage_stable, col=cl_fase(nfase), lwd=0.8)
          legend("topleft", legend=paste("Stage Stable", 1:nfase), lty=1, lwd=0.9, col=cl_fase(nfase), bty="n", cex=0.8)
          invisible(pais)
}
