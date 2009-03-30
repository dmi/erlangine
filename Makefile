all:
	(cd deps/mochiweb;$(MAKE))
	(cd deps/ecouch;$(MAKE))
	(cd deps/smtp_client;$(MAKE))
	(cd src;$(MAKE))

clean:
	(cd deps/mochiweb;$(MAKE) clean)
	(cd deps/ecouch;$(MAKE) clean)
	(cd deps/smtp_client;$(MAKE) clean)
	(cd src;$(MAKE) clean)
