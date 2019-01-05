System Requirements
-------------------
- Linux/Unix like environment
- Ocaml with Ocamllex and Ocamlyacc
- LaTeX with pdflatex

Direction for Use
-----------------
1) Setup images directory.
  - In this directory, create a directory named 'images' if not already there.
  - Image files named 0.jpg, 1.jpg, ..., 12.jpg, 13.jpg should be placed in 
    images/ directory. These files correspond to the cover page, January, ...
    December and final page respectively.
  - The application will generate valid output only if the above images are 
    provided as specified.
2) Building the application.
  - Run 'make' here.
    make
3) Running the application.
    ./desktop-calendar <year> <holidays-file-name> <image-directory>

generates a pdf file desktop-calendar.pdf in the output/ directory.

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

Features excluded in this version
---------------------------------
- Healthiness check of the images/ directory, e.g. proper numbering of all
  files, proper file extensions (only.jpg accepted). The user has to ensure
  that the input photos are healthy. The behaviour of the program on 
  encountering an error here is unpredictable.
- Flexible rendering of calendar pages based on image aspect ratio.
- Choice of calendar theme
