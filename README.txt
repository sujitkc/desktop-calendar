System Requirements
-------------------
- Linux/Unix like environment
- Ocaml with Ocamllex and Ocamlyacc
- LaTeX with pdflatex

Directions for Use:
-------------------
The software is currently not setup to be used by external use.
Hence, these directions are for self.
- Create a data folder <dir-name> where to keep all files regarding the caledar
  to be generated. This includes:
	* images files: can be placed in <dir-name>/images folder, but not necessary.
	* special days files: can be placed in <dir-name>/special-days.txt, but not
	  necessary.
- Edit desktop_calendar.ml as follows:
    - add the new calendar function
		- the appropriate variables of this functions should point to the
		  directories where you have kept the images and special days file.
		- ensure that this function is called.
- Compile: make
- run desktop_calendar.sh as follows:
    ./desktop_calendar.sh <dir-name>
- If all goes well, the desktop calendar will be generated in
  <dir-name>/desktop_calendar.pdf.

Features included:
------------------
- One month per page
- Holidays and weekends marked.
- Holiday list per page
- photos (currently to be included manually in the images/ directory)
- Arbitrary holiday descriptions.
  For example:
    "Ram Navami", "Sujit's Birthday"
  (see 'holidays.txt' for example).
- Choice of calendar theme

Features excluded in this version
---------------------------------
- Healthiness check of the images/ directory, e.g. proper numbering of all
  files, proper file extensions (only.jpg accepted). The user has to ensure
  that the input photos are healthy. The behaviour of the program on 
  encountering an error here is unpredictable.
- Flexible rendering of calendar pages based on image aspect ratio.
- Preprocessing of images to rightsize them and fix their aspect ratio.
