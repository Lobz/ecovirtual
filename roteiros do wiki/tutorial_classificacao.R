## retirado do tutorial de classificacao WIKI ECOVIRTUAL
#melina leite

### jaccard

##
plot1=c(2,13,0,1, 20)
plot1
plot2=c(8,6,5,15,0)
plot2
parcela=data.frame(plot1,plot2)

nsp_1=sum(plot1>0)
nsp_1
nsp_2=sum(plot2>0)
nsp_2
nsp_com=sum(plot1>0 & plot2>0)
nsp_com

jacc= nsp_com/(nsp_1 + nsp_2 - nsp_com)
jacc

### distancia euclidiana

##
eucl=sqrt(sum((plot1-plot2)^2))
eucl

##
eucl/(sum(plot1) + sum(plot2))


### comparando abundancias

##
bc.sim=sum(apply(parcela, 1, min))/(sum (parcela$plot1+parcela$plot2))
bc.sim


### Matriz de similaridade

## funcao que compara as parcelas amostradas

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
          return(round(similar,3))
}


##
sim.cont1=sim(amost.cont, indice="bc")
sim.cont1
round(sim.cont1, 3)
sim.cont1[1,4]
sim.cont1[4,1]
sim.cont1[5,7]
sim.cont1[7,5]

## nao esta no tutorial, foi pedido para fazer:
sim.disc1=sim(amost.disc, indice="bc")
sim.disc1


### Agrupamento

##
max(sim.cont1, na.rm=TRUE)
sim.cont1==max(sim.cont1, na.rm=TRUE)

##
nome.par=rownames(sim.cont1)
upper.tri(sim.cont1, diag=TRUE)
sim.cont1[upper.tri(sim.cont1, diag=TRUE)]=NA
sim.cont1

##
max1=max(sim.cont1,na.rm=TRUE)
max1
maior1=which(sim.cont1==max(sim.cont1,na.rm=TRUE), arr.ind=TRUE)
maior1

##
par1=nome.par[maior1[1,]]
par1
cat("\n\t 1a. ligação ", paste(par1[1],par1[2], sep=" x "), "; ligação = ", max1, "\n" )

##
g1.n1=strsplit(nome.par[maior1[1,2]]," ")[[1]][2]
g1.n2=strsplit(nome.par[maior1[1,1]]," ")[[1]][2]
g1.nome=paste("g", paste(g1.n1,g1.n2, sep=","))
g1.nome

mat.g1=sim.cont1[-maior1[1,],-maior1[1,]]
g1a=apply(sim.cont1[maior1[1,],-maior1[1,]],2,mean)
g1a[is.na(g1a)]=0
g1b=apply(sim.cont1[-maior1[1,],maior1[1,]],1,mean)
g1b[is.na(g1b)]=0
gr1=rbind(mat.g1,g1a+g1b)
grupo1=cbind(gr1,NA)
rownames(grupo1)<-c(nome.par[-maior1[1,]],g1.nome)
colnames(grupo1)[dim(grupo1)[2]]<-g1.nome
grupo1

##
nome.par2=rownames(grupo1)
max2=max(grupo1,na.rm=TRUE)
max2
maior2=which(grupo1==max(grupo1,na.rm=TRUE), arr.ind=TRUE)
maior2
g2.n1=strsplit(rownames(grupo1)[maior2[1,2]]," ")[[1]][2]
g2.n2=strsplit(rownames(grupo1)[maior2[1,1]]," ")[[1]][2]
g2.nome=paste(paste("g",g2.n1,sep="_"),g2.n2, sep=",")  
g2.nome
cat("\n\n\t 2a. ligação ", paste(nome.par2[maior2[1,2]],nome.par2[maior2[1,1]], sep=" x "), "; ligação = ", max2, "\n" )

mat.g2=grupo1[-maior2[1,],-maior2[1,]]
g2a=apply(grupo1[maior2[1,],-maior2[1,]],2,mean)
g2a[is.na(g2a)]=0
g2b=apply(grupo1[-maior2[1,],maior2[1,]],1,mean)
g2b[is.na(g2b)]=0
gr2=rbind(mat.g2,g2a+g2b)
grupo2=cbind(gr2,NA)
rownames(grupo2)<-c(nome.par2[-maior2[1,]],g2.nome)
colnames(grupo2)[dim(grupo2)[2]]<-g2.nome
grupo2

##
nome.par3=rownames(grupo2)
max3=max(grupo2,na.rm=TRUE)
max3
maior3=which(grupo2==max(grupo2,na.rm=TRUE), arr.ind=TRUE)
maior3
g3.n1=strsplit(rownames(grupo2)[maior3[1,2]]," ")[[1]][2]
g3.n2=strsplit(rownames(grupo2)[maior3[1,1]]," ")[[1]][2]
g3.nome=paste(paste("g",g3.n1,sep="_"),g3.n2, sep=",")  
g3.nome
cat("\n\n\t 3a. ligação ", paste(nome.par3[maior3[1,2]],nome.par3[maior3[1,1]], sep=" x "), "; ligação = ", max3, "\n" )

mat.g3=grupo2[-maior3[1,],-maior3[1,]]
g3a=apply(grupo2[maior3[1,],-maior3[1,]],2,mean)
g3a[is.na(g3a)]=0
g3b=apply(grupo2[-maior3[1,],maior3[1,]],1,mean)
g3b[is.na(g3b)]=0
gr3=rbind(mat.g3,g3a+g3b)
grupo3=cbind(gr3,NA)
rownames(grupo3)<-c(nome.par3[-maior3[1,]],g3.nome)
colnames(grupo3)[dim(grupo3)[2]]<-g3.nome
grupo3

##
clas.cont1=hclust(as.dist(1-sim.cont1), method="average")
dend.cont1=as.dendrogram(clas.cont1, hang=-1)
plot(dend.cont1)

##
par(mfrow=c(2,2))
clas.cont1a=hclust(as.dist(1-sim.cont1), method="single")
plot(as.dendrogram(clas.cont1, hang=-1), main="Ligação simples",ylab="Bray-Curtis")
clas.cont1b=hclust(as.dist(1-sim.cont1), method="complete")
plot(as.dendrogram(clas.cont1b, hang=-1), ylab="Bray-Curtis",main="Ligação completa") 
clas.cont1c=hclust(as.dist(1-sim.cont1), method="average")
plot(as.dendrogram(clas.cont1c, hang=-1), ylab="Bray-Curtis",main="Ligação média")
clas.cont1d=hclust(as.dist(1-sim.cont1), method="centroid")
plot(as.dendrogram(clas.cont1d, hang=-1),ylab="Bray-Curtis",main="Ligação centroide")

##
sim.disc=sim(amost.disc, indice="bc")
clas.disc=hclust(as.dist(1-sim.cont1), method="average")
plot(as.dendrogram(clas.disc, hang=-1), ylab="Bray-Curtis",main="Ligação média")

######
clas.cont=hclust(as.dist(1-sim.cont1, method="average"))