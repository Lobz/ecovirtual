bebado <- function(n=1,step=1,ciclo=1e5,cont=1e3,x1=NULL){
  if(is.null(x1)){
    x1 <- sample(1:200,n,replace=TRUE)
  }
  results <- matrix(NA,nrow=1+ciclo/cont,ncol=n) 
  results[1,] <- x1
  X <- x1
  for(i in 2:(1+ciclo/cont)){
    for(j in 1:cont){
      X[X<=0] <- NA
      X <- X +sample(c(step,-1*step),n,replace=T)
    }
    results[i,] <- X
  }
  results[is.na(results)] <- 0
  time <- seq(0,ciclo,by=cont)
  matplot(time,results,type="l", col=rainbow(n),lwd=2, xlab="Passos", ylab="Distância do Abismo")
  abline(h=0,lwd=4)
}

jogo <- function(aposta=1,total=20){
  X <- total/2
  results <- X
  while(X>0&X<total){
    X <- X+sample(c(aposta,-1*aposta),1)
    results <- c(results,X)
  }
  plot(1:length(results),results, type="l", col="blue",ylim=c(0,total), xlab="N de rodadas", ylab="Valor")
  lines(1:length(results),total-results, col="red")
  abline(h=c(0,total),lty=2)
}

rich <- function(x)length(unique(x)) ## funcao auxiliar

sim.hub1=function(S= 100, j=10, D=1, ciclo=2e4, step=1000){ 
  ## Tamanho da comunidade
  J <- S*j
  ##Matrizes para guardar os resultados
  ## matriz da especie de cada individuo por ciclo
  ind.mat=matrix(nrow=J,ncol=1+ciclo/step) 
  ##CONDICOES INICIAIS##
  ##Deduzidas de acordo com o modelo de Hubbell:
  ## Todas as especies comecam com o mesmo numero de individuos (j=J/S)
  ind.mat[,1] <- rep(1:S,each=j)
  cod.sp <- ind.mat[,1]
  ##Aqui comecam as simulacoes
  for(i in 2:(1+ciclo/step)){
    for(j in 1:step){
      ##Indice dos individuos que morrem
      morte <- sample(1:J,D)
      ##Indice dos individuos que produzem filhotes para substituir os mortos
      novos <- sample(1:J,D,replace=T)
      ##Substituindo
      cod.sp[morte]<-cod.sp[novos]
    }
    ## A cada step ciclos os resultados sao gravados
    ind.mat[,i] <- cod.sp
  }
  tempo <- seq(0,ciclo,by=step)
  colnames(ind.mat) <- tempo
  invisible(ind.mat)
  plot(tempo,apply(ind.mat,2,rich), xlab="Tempo (ciclos)", ylab="N de espécies", type="l",
       main=paste("Dinâmica Neutra sem Colonização", "\n S=",S," J=",J)
       ,ylim=c(0,S))
}

sim.hub2=function(S= 100, j=10, D=1, ciclo=2e4, step=1000, m=0.05){ 
  ## Tamanho da comunidade
  J <- S*j
  ##Matrizes para guardar os resultados
  ## matriz da especie de cada individuo por ciclo
  ind.mat=matrix(nrow=J,ncol=1+ciclo/step) 
  ##CONDICOES INICIAIS##
  ## Todas as especies comecam com o mesmo numero de individuos (j=J/S)
  ## Rotulo de especies para cada um dos inividuos
  ind.mat[,1] <- rep(1:S,each=j)
  ## Repetindo este rotulo no vetor que sofrera modificacoes
  cod.sp <- ind.mat[,1]
  ##Aqui comecam as simulacoes
  for(i in 2:(1+ciclo/step)){
    for(j in 1:step){
      ##Indice dos individuos que morrem
      morte <- sample(1:J,D)
      ## Indice dos individuos mortos que serao repostos por migrantes
      defora <- sample(c(TRUE,FALSE),size=D,replace=T,prob=c(m,1-m))
      ##Indice dos individuos que produzem filhotes para substituir os mortos
      novosd <- sample(1:J,D-sum(defora),replace=T)
      novosf <- sample(1:J,sum(defora),replace=T)
      ##Substituindo
      ## Mortos por propagulos de dentro
      if(length(novosd)>0){
        cod.sp[morte[!defora]]<-cod.sp[novosd]
      }
      ## Mortos por propagulos de fora
      if(length(novosf)>0){
        cod.sp[morte[defora]]<-ind.mat[,1][novosf]
      }
    }
    ## A cada step ciclos os resultados sao gravados
    ind.mat[,i] <- cod.sp
  }
  tempo <- seq(0,ciclo,by=step)
  colnames(ind.mat) <- tempo
  invisible(ind.mat)
  plot(tempo,apply(ind.mat,2,rich), xlab="Tempo (ciclos)", ylab="N de espécies", type="l",
       main=paste("Dinâmica Neutra com Colonização da Comunidade Original", "\n S=",S," J=",J," m=",m),ylim=c(0,S))
  }

sim.hub3=function(Sm=200, jm=20, S=100, j=2, m=0.01, nu=0.0001, D=1, ciclo=1e4, step=100){ 
  ## Tamanho da metacomunidade
  Jm <- Sm*jm
  ## Tamanho da comunidade
  J <- S*j
  ##Matrizes para guardar os resultados
  ## matriz da especie de cada individuo por ciclo
  ## Na metacomunidade
  meta.mat=matrix(nrow=Jm,ncol=1+ciclo/step) 
  ## Na comunidade
  ind.mat=matrix(nrow=J,ncol=1+ciclo/step)
  ##CONDICOES INICIAIS##
  ## Todas as especies comecam com o mesmo numero de individuos (j=J/S)
  ## METACOMUNIDADE
  meta.mat[,1] <- rep(1:Sm,each=jm)
  ## Repetindo este rotulo no vetor que sofrera modificacoes
  meta.sp <- meta.mat[,1]
  ##COMUNIDADE
  ## Rotulo de especies para cada um dos individuos
  ind.mat[,1] <- rep(1:S,each=j)
  ## Repetindo este rotulo no vetor que sofrera modificacoes
  cod.sp <- ind.mat[,1]
  ##Aqui comecam as simulacoes
  for(i in 2:(1+ciclo/step)){
    for(j in 1:step){
      ##Indice dos individuos que morrem
      ## Na comunidade
      morte <- sample(1:J,D)
      ## Na metacomunidade
      meta.morte <- sample(1:Jm,D)
      ## Indice dos individuos mortos da comunidade que serao repostos por migrantes 
      defora <- sample(c(TRUE,FALSE),size=D,replace=T,prob=c(m,1-m))
      ## Indice dos individuos mortos da metacomunidade que serao repostos por novas especies 
      meta.defora <- sample(c(TRUE,FALSE),size=D,replace=T,prob=c(nu,1-nu))
      ##Indice dos individuos que produzem filhotes para substituir os mortos da comunidade
      novosd <- sample(1:J,D-sum(defora),replace=T)
      novosf <- sample(1:Jm,sum(defora),replace=T)
      ##Indice dos individuos que produzem filhotes para substituir os mortos da metacomunidade
      meta.novosd <- sample(1:Jm,D-sum(meta.defora),replace=T)
      meta.novosf <- sample(1:Jm,sum(meta.defora),replace=T)
      ##Substituindo
      ## N metacomunidade ##
      ## Mortos por propagulos de dentro
      if(length(meta.novosd)>0){
        meta.sp[meta.morte[!meta.defora]]<-meta.sp[meta.novosd]
      }
      ## Mortos por novas especies
      if(length(meta.novosf)>0){
        meta.sp[meta.morte[meta.defora]]<-max(meta.sp)+1
      }
      ## Na comunidade ##
      ## Mortos por propagulos de dentro
      if(length(novosd)>0){
        cod.sp[morte[!defora]]<-cod.sp[novosd]
      }
      ## Mortos por propagulos de fora
      if(length(novosf)>0){
        cod.sp[morte[defora]]<-meta.sp[novosf]
      }
    }
    ## A cada step ciclos os resultados sao gravados
    ind.mat[,i] <- cod.sp
    meta.mat[,i] <- meta.sp
  }
  tempo <- seq(0,ciclo,by=step)
  colnames(ind.mat) <- tempo
  colnames(meta.mat) <- tempo
  resultados <- list(metacomunidade=meta.mat,comunidade=ind.mat)
  invisible(resultados)
  ## Graficos
  plot(tempo,apply(meta.mat,2,rich), xlab="Tempo (ciclos)", ylab="N de espécies", type="l",
       main=paste("Dinâmica Neutra com Colonizacao da Metacomunidade", "\n Jm=",Jm," nu=",nu," Theta=",2*Jm*nu,
         "S=",S," J=",J," m=",m), ylim=c(0,max(apply(meta.mat,2,rich))))
  lines(tempo,apply(ind.mat,2,rich),col="red")
  }
