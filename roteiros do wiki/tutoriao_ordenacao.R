## tutorial de ordenacao do wiki ecovirtual
#melina leite

### Ordenacao

##
sp2=amost.cont[c(1,2),]
duas.sp=sp2[,apply(sp2,2,sum)>0]
plot(t(duas.sp), pch=20, col=rainbow(dim(duas.sp)[2]), xlab="sp1", ylab="sp2")
text(duas.sp[1,]+0.1, duas.sp[2,]+0.4, colnames(duas.sp),col=rainbow(dim(duas.sp)[2]) )


### ordenacao polar

## funcao de dissimilaridade de bray-curtis
dis.bc<-function(dados)
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
                              similar[m,i]=similar[i,m]=bc.dist
                              diag(similar)<-0
                    }
          }
          return(round(similar,3))          
}


### Algoritmo da ordenacao polar

##1
dis1.cont=dis.bc(amost.cont)

##2
somadist1.cont=apply(dis1.cont, 1, sum, na.rm=TRUE)
somadist1.cont

##3
max(somadist1.cont)
nomes.parc = names(somadist1.cont)
parc.ax = nomes.parc[somadist1.cont==max(somadist1.cont)][1]
parc.ax

##4
dist.ax=dis1.cont[,parc.ax]
dist.ax
max.ax=max(dist.ax)
max.ax
parc.bx=nomes.parc[dist.ax==max.ax]
parc.bx

##4a
somamax.bx=max(somadist1.cont[parc.bx])
parc.bx=parc.bx[somadist1.cont==somamax.bx][1]
parc.bx

##5.1
dist.ax
dist.bx=rbind(dis1.cont[,parc.bx], dis1.cont[parc.bx,])
dist.bx
dist.bx=apply(dist.bx,2,sum, na.rm=TRUE)
dist.bx

##5.2
xi= (max.ax^2 + dist.ax^2 - dist.bx^2)/(2*max.ax)
xi

##6
yi=sqrt((dist.ax)^2-xi^2)
yi

##6.1
yi[parc.bx]=max.ax
yi

##7
op1.cont=data.frame(xi,yi)
op1.cont
plot(op1.cont)


### funcao ordenacao polar

##
ordena.polar=function(dist)
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
          plot(op.xy, pch=19, col=rainbow(length(xi)), xlim=c(-0.1, 2.1), ylim=c(-0.1,2.1), main="Ordenação Polar", sub="Distância Bray-Curtis")
          text(op.xy-0.05, labels=rownames(op.xy))
          return(op.xy)
}

##
ordena.polar(dis1.cont)
