OCAMLC=ocamlc
OCAMLOPT = ocamlopt

desktop_calendar : desktop_calendar.cmo holidayparser.cmo lexer.cmo calendar.cmo
	$(OCAMLC) -o desktop_calendar calendar.cmo lexer.cmo holidayparser.cmo desktop_calendar.cmo; \
	if [ ! -d images ]; \
	then \
		mkdir images; \
	fi; \
	if [ ! -d output ]; \
	then \
		mkdir output; \
	fi; \
	echo "Build successful!"

lexer.ml : lexer.mll calendar.cmi holidayparser.cmi
	ocamllex lexer.mll
	$(OCAMLC) -c lexer.ml

holidayparser.cmi : holidayparser.mly calendar.cmi
	ocamlyacc holidayparser.mly
	$(OCAMLC) -c holidayparser.mli
	$(OCAMLC) -c holidayparser.ml

calendar.cmi : calendar.mli
	$(OCAMLC) -c calendar.mli

calendar.cmo : calendar.ml calendar.cmi
	$(OCAMLC) -c calendar.ml

desktop_calendar.cmo : desktop_calendar.ml desktop_calendar.cmi calendar.cmi holidayparser.cmi lexer.ml
	$(OCAMLC) -c desktop_calendar.ml

desktop_calendar.cmi : desktop_calendar.mli calendar.cmi
	$(OCAMLC) -c desktop_calendar.mli

clean:
	rm *.cmx *.cmi *.cmo lexer.ml lexer.mli holidayparser.ml holidayparser.mli desktop_calendar
