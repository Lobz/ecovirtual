#ecovirtual - montando pacotes pelo terminal
R CMD build /home/ale/Documentos/Ale2013/AleProjetos/EcoVirtual_svn/EcoVirtual
R CMD build /home/ale/Documentos/Ale2013/AleProjetos/EcoVirtual_svn/RcmdrPlugin.EcoVirtual
#instalando o rcmdr
install.packages("Rcmdr")
#pacotes do ecovirtual
install.packages("EcoVirtual_0.03.tar.gz" ,repos=NULL)  
install.packages("RcmdrPlugin.EcoVirtual_0.03.tar.gz" ,repos=NULL) # VAIO new
## para iniciar o Rcmdr:
library(Rcmdr)
