PREFIX ?= /usr

hdata:
	ghc --make hdata

clean:
	rm -rf *.o
	rm -rf *.hi
	rm -rf *.db
	rm -rf hdata

install:
	@mkdir -p ${DESTDIR}${PREFIX}/bin
	@cp -f hdata ${DESTDIR}${PREFIX}/bin
	@chmod 755 ${DESTDIR}${PREFIX}/bin/hdata

uninstall:
	@rm -f ${DESTDIR}${PREFIX}/bin/hdata
