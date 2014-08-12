all:
	make pkg-ecovirtual
	make pkg-Rcmd
	make test
	make install

pkg-ecovirtual:
	R CMD build EcoVirtual

pkg-Rcmd:
	R CMD build RcmdrPlugin.EcoVirtual

#test target tests ALL packages
test:
	R CMD check *.tar.gz

clean:
	-rm EcoVirtual*.tar.gz RcmdrPlugin.EcoVirtual*.tar.gz
	-rm -rf *.Rcheck

install:
	pack=`ls EcoVirtual*.tar.gz` && R --vanilla <<< "install.packages(\"$$pack\", repos=NULL)"
	pack=`ls RcmdrPlugin.EcoVirtual*.tar.gz` && R --vanilla <<< "install.packages(\"$$pack\", repos=NULL)"
