PREFIX ?= /usr

hdata:
	ghc -isrc -isrc/Tools src/hdata.hs

clean:
	rm -rf src/*.o
	rm -rf src/*.hi
	rm -rf src/Tools/*.o
	rm -rf src/Tools/*.hi
	rm -rf *.db
	rm -rf src/hdata

install:
	@mkdir -p ${DESTDIR}${PREFIX}/bin
	@cp -f src/hdata ${DESTDIR}${PREFIX}/bin
	@chmod 755 ${DESTDIR}${PREFIX}/bin/hdata

uninstall:
	@rm -f ${DESTDIR}${PREFIX}/bin/hdata
