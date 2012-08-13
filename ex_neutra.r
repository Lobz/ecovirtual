##carregando as funcoes
source("funcoes.r")

### Bebado e o abismo ###
bebado(n=10,step=10,ciclo=1e4,cont=1e3)
##bebados q balancam menos
set.seed(42) # semente de números aleatórios
bebado(n=10,step=2,ciclo=1e4,cont=1e3)
## Dando mais tempo
set.seed(42)
bebado(n=10,step=2,ciclo=5e4,cont=1e3)

### Jogo de soma zero ###
##Aumento da quantia em jogo
jogo(aposta=1,total=20)
jogo(aposta=1,total=200)


### Dinamica neutra sem migracao ###
sim.hub1(S=100, j=2)
##Efeito do tamamnho da comunidade
par(mfrow=c(2,2))## para 4 gráficos na mesma janela
sim.hub1(S=100, j=2)
sim.hub1(S=100, j=4)
sim.hub1(S=100, j=8)
sim.hub1(S=100, j=12)
par(mfrow=c(1,1)) ## Volta a um grafico por janela

### Dinamica neutra com migracao ###
par(mfrow=c(2,2)) ## abre espaço para 4 graficos na mesma janela
sim.hub2(S=100, j=2,m=0)
sim.hub2(S=100, j=2,m=0.1)
sim.hub2(S=100, j=2,m=0.2)
sim.hub2(S=100, j=2,m=0.4)
par(mfrow=c(1,1))

### Uma metacomunidade mais realista ###
##Lembrando efeito do tamamnho da comunidade sobre perda de especies
par(mfrow=c(2,1))
sim.hub1(S=100, j=2, ciclo=2e4,step=500)
sim.hub1(S=200, j=20, ciclo=2e4,step=500)
par(mfrow=c(1,1))
##Diferentes tamanhos de metacomunidade
par(mfrow=c(2,2))
sim.hub3(S=100, j=2,Sm=200,jm=20,nu=1e-9, m=0)
sim.hub3(S=100, j=2,Sm=200,jm=20,nu=1e-9, m=0.1)
sim.hub3(S=100, j=2,Sm=200,jm=20,nu=1e-9, m=0.2)
sim.hub3(S=100, j=2,Sm=200,jm=20,nu=1e-9, m=0.4)
par(mfrow=c(1,1))
