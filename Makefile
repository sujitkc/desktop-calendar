OCAMLC=ocamlc
OCAMLOPT = ocamlopt

CSV=/usr/local/lib/ocaml/4.02.3/csv/

birthday_calendar : birthday_calendar.cmo calendar.cmo
	$(OCAMLC) -o birthday_calendar str.cma $(CSV)csv.cma calendar.cmo birthday_calendar.cmo; \
	if [ ! -d images ]; \
	then \
		mkdir images; \
	fi; \
	if [ ! -d output ]; \
	then \
		mkdir output; \
	fi; \
	echo "Build successful!"

calendar.cmi : calendar.mli
	$(OCAMLC) -c calendar.mli

calendar.cmo : calendar.ml calendar.cmi
	$(OCAMLC) -c calendar.ml

birthday_calendar.cmo : birthday_calendar.ml birthday_calendar.cmi calendar.cmi
	$(OCAMLC) -I $(CSV) -c birthday_calendar.ml

birthday_calendar.cmi : birthday_calendar.mli calendar.cmi
	$(OCAMLC) -c birthday_calendar.mli

clean:
	rm *.cmi *.cmo birthday_calendar
