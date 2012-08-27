###Comandos R para os exercicios de Metapopulacoes
### BIE0312 e BIE0318 - Ecologia Vegetal - IB - USP


### Chuva de Propagulos ###
##
pc=0.3
pe=0.15
fi=0.5
##
paisag=array(0,dim=c(10,10,1))
paisag
##
paisag=array(0,dim=c(10,10,10))
paisag
##
paisag[,,1]=sample(c(rep(0,(100-fi*100)),rep(1,fi*100)))
paisag[,,1]
##
paisag[,,2][paisag[,,1]==1]<-sample(c(0,1),sum(paisag[,,1]),replace=T,prob=c(pe,1-pe))
paisag[,,2]
##
paisag[,,1] # paisagem no tempo inicial
paisag[,,2] # paisagem depois de um passo no tempo
##
sum(paisag[,,1]) # total de manchas ocupadas no tempo inicial
sum(paisag[,,2]) # total de manchas ocupadas no tempo 2
##
paisag[,,2][paisag[,,1]==0]<-sample(c(0,1),10*10-sum(paisag[,,1]),replace=T,prob=c(1-pc,pc))
paisag[,,2]
##
paisag[,,1] 
paisag[,,2]

sum(paisag[,,1]) # total de manchas ocupadas no tempo inicial
sum(paisag[,,2]) # total de manchas ocupadas no tempo 2
##
f1=sum(paisag[,,1])/100 # fração das manchas ocupadas no tempo 1
f2=sum(paisag[,,2])/100 # fração das manchas ocupadas no tempo 2

f1
f2
f2-f1
##
paisag=array(0,dim=c(10,10,10))
paisag[,,1]=sample(c(rep(0,(100-fi*100)),rep(1,fi*100)))
resultado=numeric()
for(t in 2:10){
	       paisag[,,t][paisag[,,(t-1)]==1]<-sample(c(0,1),sum(paisag[,,(t-1)]),replace=T,prob=c(pe,1-pe))
	       paisag[,,t][paisag[,,(t-1)]==0]<-sample(c(0,1),10*10-sum(paisag[,,(t-1)]),replace=T,prob=c(1-pc,pc))
	       resultado[t-1]=sum(paisag[,,t])/(10*10)
	      }
resultado
##
resultado=data.frame(t=1:10,f=c(fi,resultado))
resultado
##
plot(1:10,resultado$f,type="l",xlab="Tempo",ylab="Fração de manchas ocupadas",
ylim=c(0,1),main="Dinâmica de ocupação de manchas",font.lab=2,lwd=2)
##
F=pc/(pc+pe)
F
##
plot(1:10,resultado$f,type="l",xlab="Tempo",ylab="Fração de manchas ocupadas",
ylim=c(0,1),main="Dinâmica de ocupação de manchas",font.lab=2,lwd=2)
abline(h=F,col=2,lwd=2,lty=2)
##
metapop=function(tf,cl,ln,fi,pc,pe){
	paisag=array(0,dim=c(ln,cl,tf))
	paisag[,,1]=matrix(sample(c(1,0),c*l,prob=c(fi,1-fi), replace=T),l,c)
	resultado=numeric()
	for(t in 2:tf){
	       paisag[,,t][paisag[,,(t-1)]==1]<-sample(c(0,1),sum(paisag[,,1]),replace=T,prob=c(pe,1-pe))
	       paisag[,,t][paisag[,,(t-1)]==0]<-sample(c(0,1),c*l-sum(paisag[,,1]),replace=T,prob=c(1-pc,pc))
	       resultado[t-1]=sum(paisag[,,t])/(cl*ln)
	      }

	F=pc/(pc+pe)

	plot(1:tf,c(fi,resultado),type="l",xlab="Tempo",ylab="Fração de manchas ocupadas",
	ylim=c(0,1),main=paste("Chuva de Propágulos","\n cl=",cl," ln=",ln," fi=",fi," pc=",pc," pe=",pe),font.lab=2,lwd=2)
	abline(h=F,col=2,lwd=2,lty=2)
	
      return(paisag)
	}
##
metapop(tf=100,cl=10,ln=10,fi=0.5,pc=0.3,pe=0.15)

##
anima=function(dados){
          x11()
          for(i in 1:dim(dados)[3])
          {
                    image(dados[,,i], main=("Ocupação de manchas"),sub=paste("simulação no.= ", i), col=c("white","red"), bty="n",xaxt='n',yaxt='n')
                    grid(dim(dados)[1],dim(dados)[2])
                    Sys.sleep(.2)
          }
}

anima(metapop(tf=25,cl=10,ln=10,fi=.1,pc=0.1,pe=0.1))


################################################################################

### Colonizacao Interna ###

##
meta.inter=function(tf,cl,ln,fi,i,pe){
	paisag=array(0,dim=c(ln,cl,tf))
	paisag[,,1]=matrix(sample(c(1,0),cl*ln,prob=c(fi,1-fi), replace=T),ln,cl)
	resultado=numeric()
	for(t in 2:tf){
		 pc=i*sum(paisag[,,t-1])/(cl*ln)
	       paisag[,,t][paisag[,,(t-1)]==1]<-sample(c(0,1),sum(paisag[,,1]),replace=T,prob=c(pe,1-pe))
	       paisag[,,t][paisag[,,(t-1)]==0]<-sample(c(0,1),cl*ln-sum(paisag[,,1]),replace=T,prob=c(1-pc,pc))
	       resultado[t-1]=sum(paisag[,,t])/(cl*ln)
	      }

	F=1-(pe/i)

	plot(1:tf,c(fi,resultado),type="l",xlab="Tempo",ylab="Fração de manchas ocupadas",
	ylim=c(0,1),main=paste("Colonização Interna","\n cl=",cl," ln=",ln," fi=",fi," i=",i," pe=",pe),font.lab=2,lwd=2)
	abline(h=F,col=2,lwd=2,lty=2)
	
      return(paisag)
	}
##
#ex
meta.inter(tf=100,cl=10,ln=10,fi=0.1,i=1,pe=0.5)


##animação
anima2=function(dados){
          tf=dim(dados)[3]
          for(i in 1:tf){
                    image(dados[,,i], main=("Ocupação de manchas"),col=c("white","red"),bty="n",xaxt='n',yaxt='n')
                    grid(dim(dados)[1],dim(dados)[2])
                    Sys.sleep(.2)
          }
}
##
anima2(meta.inter(tf=25,cl=10,ln=10,fi=.1,i=1,pe=0.1))

################################################################################

### Efeito resgate ###

##
meta.er=function(tf,cl,ln,fi,pc,e){
          paisag=array(0,dim=c(ln,cl,tf))
          paisag[,,1]=matrix(sample(c(1,0),cl*ln,prob=c(fi,1-fi), replace=T),ln,cl)
          resultado=numeric()
          res=numeric()
          for(t in 2:tf){
                    pe=e*(1-sum(paisag[,,t-1])/(cl*ln))
                    paisag[,,t][paisag[,,(t-1)]==1]<-sample(c(0,1),sum(paisag[,,(t-1)]), replace=T, prob=c(pe,1-pe))
                    paisag[,,t][paisag[,,(t-1)]==0]<-sample(c(0,1),cl*ln-sum(paisag[,,(t-1)]), replace=T, prob=c(1-pc,pc))
                    resultado[t-1]=sum(paisag[,,t])/(cl*ln)
                    res[t-1]=pe
          }
          
          F=pc/e
          
          plot(1:tf,c(fi,resultado),type="l",xlab="Tempo",ylab="Fração de manchas ocupadas",
               ylim=c(0,1),main=paste("Chuva de Propágulos com Efeito Resgate",
                                      "\n cl=",cl," ln=",ln," fi=",fi," pc=",pc," e=",e),
               font.lab=2,lwd=2)
          abline(h=F,col=2,lwd=2,lty=2) # equilibrio F
          
          points(1:tf,c(e*(1-fi),res),type='l',lwd=2,col="blue") # pe observado
          abline(h=e-pc,col="green",lwd=2,lty=2) # pe equilibrio
          legend("topright", legend=c("proporção ocupada", "equilíbrio F", "prob. extinção (pe)", "equilíbrio pe"), lty=c(1,2,1,2), col=c("black","red","blue", "green"), bty="n")
          
          return(paisag)
}

#ex
meta.er(tf=100,cl=10,ln=10,fi=.1,pc=0.1,e=1)
##

########
#efeito resgate e colonizacao interna
meta.cier=function(tf,cl,ln,fi,i,e){
          paisag=array(0,dim=c(ln,cl,tf))
          paisag[,,1]=sample(c(rep(0,round(cl*ln-fi*cl*ln)),rep(1,round(fi*cl*ln))))
          resultado=numeric()
          rese=numeric()
          resi=numeric()
          for(t in 2:tf){
                    pe=e*(1-sum(paisag[,,t-1])/(cl*ln))
                    pc=i*sum(paisag[,,t-1])/(cl*ln)
                    paisag[,,t][paisag[,,(t-1)]==1]<-sample(c(0,1),sum(paisag[,,t-1]),replace=T,prob=c(pe,1-pe))
                    paisag[,,t][paisag[,,(t-1)]==0]<-sample(c(0,1),cl*ln-sum(paisag[,,t-1]),replace=T,prob=c(1-pc,pc))
                    resultado[t-1]=sum(paisag[,,t])/(cl*ln)
                    rese[t-1]=pe
                    resi[t-1]=pc
          }
          plot(1:tf,c(fi,resultado),type="l",xlab="Tempo",ylab="Proporção/Probabilidade",
               ylim=c(0,1),main=paste("Colonização Interna","\n cl=",cl," ln=",ln," fi=",fi," i=",i," e=",e),font.lab=2,lwd=2)
          abline(h=0,lty=2)
          
          points(1:tf,c(e*(1-fi),rese),type='l',lwd=2,col=4,lty=3)
          
          points(1:tf,c(i*fi,resi),type='l',lwd=2,col=6,lty=3)
          legend("topright", legend=c("manchas ocupadas", "prob.colonização", "prob.extinção"), lty=c(1,3,3), col=c(1,6,4), bty="n")
          
          
          return(paisag)
}



meta.cier(tf=100,cl=10,ln=10,fi=.5,i=.5,e=.5)


