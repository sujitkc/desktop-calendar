System Requirements
-------------------
- Linux/Unix like environment
- Ocaml with Ocamllex and Ocamlyacc
- LaTeX with pdflatex

Directions for Use:
-------------------
The software is currently not setup to be used by external use.

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
