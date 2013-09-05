PREFIX ?= /usr

hddb:
	ghc -o hddb -isrc -isrc/Tools src/Main.hs

clean:
	rm -rf src/*.o
	rm -rf src/*.hi
	rm -rf src/Tools/*.o
	rm -rf src/Tools/*.hi
	rm -rf *.db
	rm -rf src/hddb
	rm -rf hddb

install:
	@mkdir -p ${DESTDIR}${PREFIX}/bin
	@cp -f hddb ${DESTDIR}${PREFIX}/bin
	@chmod 755 ${DESTDIR}${PREFIX}/bin/hddb

uninstall:
	@rm -f ${DESTDIR}${PREFIX}/bin/hddb
