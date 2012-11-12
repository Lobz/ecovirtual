pkgname <- "EcoVirtual"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('EcoVirtual')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("EcoVirtual-package")
### * EcoVirtual-package

flush(stderr()); flush(stdout())

### Name: EcoVirtual-package
### Title: Simulating Metapopulation Dynamics
### Aliases: EcoVirtual-package EcoVirtual
### Keywords: package

### ** Examples

metapop(tf=100,cl=10,ln=10,fi=0.5,pc=0.3,pe=0.15)



cleanEx()
nameEx("dynPop")
### * dynPop

flush(stderr()); flush(stdout())

### Name: dynPop
### Title: Populations Dynamics Model
### Aliases: crescExp estExp estDem crescLog discrLog atrBif crescAtr
###   compLV popStr sobrevive
### Keywords: population dynamics simulation

### ** Examples

popStr(p.sj=0.4, p.jj=0.6, p.ja=0.2, p.aa=0.9, fec=0.8, ns=100,nj=40,na=20, ln=30, cl=30, tmax=100)



cleanEx()
nameEx("meta.cier")
### * meta.cier

flush(stderr()); flush(stdout())

### Name: meta.cier
### Title: Internal Colonization And Rescue Effect Simulation
### Aliases: meta.cier
### Keywords: simulation population

### ** Examples

meta.cier(tf=100, cl=10, ln=10, fi=0.2, i=0.2, e=0.15)



cleanEx()
nameEx("meta.er")
### * meta.er

flush(stderr()); flush(stdout())

### Name: meta.er
### Title: Rescue Effect Simulation
### Aliases: meta.er
### Keywords: simulation population

### ** Examples

meta.cier(tf=100, cl=10, ln=10, fi=0.2, i=0.2, e=0.15)



cleanEx()
nameEx("meta.inter")
### * meta.inter

flush(stderr()); flush(stdout())

### Name: meta.inter
### Title: Internal Colonization
### Aliases: meta.inter
### Keywords: metapopulation simulation

### ** Examples

metapop(tf=100,cl=10,ln=10,fi=0.5,pc=0.3,pe=0.15)



cleanEx()
nameEx("meta.spac")
### * meta.spac

flush(stderr()); flush(stdout())

### Name: meta.spac
### Title: Espatial Dependence Metapopulation Simulation
### Aliases: meta.spac
### Keywords: metapopulation simulation

### ** Examples

metapop(tf=100,cl=10,ln=10,fi=0.5,pc=0.3,pe=0.15)



cleanEx()
nameEx("metaComp")
### * metaComp

flush(stderr()); flush(stdout())

### Name: metaComp
### Title: Metapopulation Competition Model
### Aliases: metaComp
### Keywords: metapopulation simulation

### ** Examples

metaComp(tmax=100,cl=100,ln=100,fi1=0.1,fi2=0.4,i1=0.4,i2=0.5,pe=0.25)



cleanEx()
nameEx("metapop")
### * metapop

flush(stderr()); flush(stdout())

### Name: metapop
### Title: Metapopulation Model
### Aliases: metapop
### Keywords: metapopulation simulation

### ** Examples

metapop(tf=100,cl=10,ln=10,fi=0.5,pc=0.3,pe=0.15)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
