## retirado do tutorial Comunidade Virtual do wiki ECOVIRTUAL # ROTEIRO MENOS DETALHADO
#melina leite


### Comunidade de plantas virtual

##funcao grafico das sps em um gradiente ambiental
graf.com=function(medias, desvios, minimo, maximo, leg=TRUE)
{
          dnorm.trunc=function(x, minimo=-Inf, maximo=Inf, media=0, desvio=1)
          {
                    res=numeric(length(x))
                    x.prov=dnorm(x,mean=media, sd=desvio)
                    ampl.norm=pnorm(maximo,mean=media, sd=desvio)-pnorm(minimo,mean=media, sd=desvio)
                    x.prov/ampl.norm
          }
          nsp=length(medias)
          cor=rainbow(nsp)
          n.min=which.min(desvios)
          curve(dnorm.trunc(x, medias[n.min], desvios[n.min], maximo=maximo, minimo=minimo),from=minimo, to=maximo, ylab="densidade da população", xlab="valor do gradiente", main="Distribuição no gradiente", col=cor[n.min])
          seqsp=1:nsp
          seqsp=seqsp[-n.min]
          for (i in seqsp)
          {
          curve(dnorm.trunc(x, medias[i], desvios[i], maximo=maximo, minimo=minimo),from=minimo, to=maximo,add=TRUE, col=cor[i])
          } 
          if(leg==TRUE)
          {
          n.medias=medias + (maximo-minimo) * 0.05
          text(n.medias[n.min], dnorm.trunc(medias[n.min], medias[n.min], desvios[n.min],maximo=maximo,minimo=minimo), labels=paste("sp",n.min,sep="_"), col=cor[n.min], cex=.7)
          text(n.medias[-n.min], dnorm.trunc(medias[-n.min], medias[-n.min], desvios[-n.min],maximo=maximo,minimo=minimo), labels=(paste("sp",seqsp,sep="_")), col=cor[-n.min], cex=.7)
          }
}  

##ex
graf.com(medias=c(2,3,4,5,6,7,8), desvios=c(1,1,1,1,1,1,1), minimo=0, maximo=10)

##
s1=seq(from=1.5, to=19.5, by=0.25)
s1
med=sample(s1, size=10)
med
desv <-runif(10,0.5,2.5)
desv

##
graf.com(medias=med, desvios=desv, minimo=0, maximo=20)

##
par(mfrow=c(2,2))
graf.com(medias=sample(2:19, size=10),desvios=sample(seq(from=0.5, to=2.5, by=0.1),10), minimo=1, maximo=20) #repetir esta funcao 3 vezes para resultar em 4 graficos na janela

dev.off()

### Comunidade discreta e continua

##
com.cont=sample(10:150, 40)
desv=sample(seq(from=4, to=10, by=0.5),40, replace=TRUE)
graf.com(medias=com.cont, desvios=desv, minimo=0, maximo=150)

##
com1=rnorm(10, mean=30, sd=5)
com2=rnorm(10, mean=60, sd=5)
com3=rnorm(10, mean=90, sd=5)
com4=rnorm(10, mean=120, sd=5)
com.disc=c(com1,com2,com3,com4)
graf.com(medias=com.disc, desvios=desv, minimo=0, maximo=150)


### Amostrando a comunidade virtual

##
amostra.com=function(medias, desvios, amostra, n.ind=100, minimo=0,
                     maximo=150)
{
          pnorm.trunc=function(x,minimo=-Inf, maximo=Inf, media=0, desvio=1)
          {
                    denom <- pnorm(maximo, mean=media, sd=desvio) - pnorm(minimo, mean=media, sd=desvio)
                    qtmp <- pnorm(x, mean=media, sd=desvio) - pnorm(minimo, mean=media, sd=desvio)
                    qtmp/denom
          }
          nsp=length(medias)
          namostra=length(amostra)
          resulta=prob.resulta=matrix(0, nrow=nsp, ncol=namostra)
          sp.name=paste("sp", 1:nsp, sep="_")
          rownames(resulta)<-sp.name
          colnames(resulta)=paste("plot", 1:namostra, sep="_")
          for(k in 1:namostra)
          {
                    for(i in 1:nsp)
                    {
                              prob.resulta[i,k]= pnorm.trunc(amostra[k]+1,minimo=minimo, maximo=maximo,media=medias[i], desvio=desvios[i])- pnorm.trunc(amostra[k],minimo=minimo, maximo=maximo,media=medias[i], desvio=desvios[i] )
                    }
                    s1=sample(sp.name, size=n.ind, prob=prob.resulta[,k], replace=TRUE)
                    conta.s1=table(s1)
                    pos.sp=match(names(conta.s1),sp.name)
                    resulta[,k][pos.sp]<-conta.s1
          }
          return(resulta)
}

##
prob.ssp=function(medias, desvios, amostra, minimo=0, maximo=150)
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
                              resulta[i,k]= pnorm.trunc(amostra[k]+1,minimo=minimo, maximo=maximo, media=medias[i], desvio=desvios[i])- pnorm.trunc(amostra[k],minimo=minimo, maximo=maximo, media=medias[i], desvio=desvios[i] )
                    }
          }
          return(resulta)
}

##
amost=seq(10,140, by=10)
amost
amost.disc<-amostra.com(medias=com.disc, desvios=desv, amostra=amost)
head(amost.disc)
amost.cont<-amostra.com(medias=com.cont, desvios=desv, amostra=amost)
head(amost.cont)

##
par(mfrow=c(2,2))
graf.com(medias=com.disc, desvios=desv, minimo=0, maximo=140)
matplot(amost,t(amost.disc), type="l", lty=2, col=rainbow(dim(amost.disc)[1]), main="Amostra",xlab='valor do gradiente',ylab='indivíduos por parcela' )
graf.com(medias=com.cont, desvios=desv, minimo=0, maximo=140)
matplot(amost,t(amost.cont), type="l", lty=2, col=rainbow(dim(amost.cont)[1]), main="Amostra",xlab='valor do gradiente',ylab='indivíduos por parcela' )

dev.off() #para fechar o dispositivo grafico e voltar o par ao "normal"