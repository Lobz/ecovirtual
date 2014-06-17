#ecovirtual - montando pacotes pelo terminal
R CMD build /home/ale/Documentos/Ale2013/AleProjetos/EcoVirtual_svn/EcoVirtual
R CMD build /home/ale/Documentos/Ale2013/AleProjetos/EcoVirtual_svn/RcmdrPlugin.EcoVirtual
#ecovirtual - checando os pacotes:
R CMD check **endereço da pasta**

#instalando o rcmdr
install.packages("Rcmdr")
#pacotes do ecovirtual
install.packages("/home/ecologia/EcoVirtual_0.05.tar.gz" ,repos=NULL)  
install.packages("/home/ecologia/RcmdrPlugin.EcoVirtual_0.05.tar.gz",repos=NULL) # VAIO new
## para iniciar o Rcmdr:
library(Rcmdr)

### conta gmail 
## ecovirtualpackage@gmail.com
## rcranecopack4224
