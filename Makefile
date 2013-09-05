PREFIX ?= /usr
MANPREFIX ?= ${PREFIX}/share/man/man1

hddb:
	ghc -o hddb -isrc -isrc/Tools src/Main.hs

clean:
	rm -rf src/*.o
	rm -rf src/*.hi
	rm -rf *.o
	rm -rf *.hi
	rm -rf src/Tools/*.o
	rm -rf src/Tools/*.hi
	rm -rf *.db
	rm -rf src/hddb
	rm -rf hddb

install:
	@mkdir -p ${DESTDIR}${PREFIX}/bin
	@cp -f hddb ${DESTDIR}${PREFIX}/bin
	@chmod 755 ${DESTDIR}${PREFIX}/bin/hddb
	@mkdir -p ${DESTDIR}${MANPREFIX}
	@cp -f hddb.1 ${DESTDIR}${MANPREFIX}/hddb.1

uninstall:
	@rm -f ${DESTDIR}${PREFIX}/bin/hddb
	@rm -f ${DESTDIR}${MANPREFIX}/hddb.1
