all:
	./build.sh

install:
	install -d $(DESTDIR)/usr/bin
	install _build/telkkarista.native $(DESTDIR)/usr/bin/telkkarista-dl
