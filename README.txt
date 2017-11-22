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
    images/ directory. This files correspond to the cover page, January, ...
    December and final page respectively.
  - The application will generate valid output only if the above images are 
    provided as specified.
2) Running the application.
    ./desktop-calendar <year> <holidays-file-name>

generates a pdf file desktop-calendar.pdf in the output/ directory.

Features included:
------------------
- One month per page
- Holidays and weekends marked.
- Holiday list per page
- photos (currently to be included manually in the images/ directory)

Features excluded in this version
---------------------------------
- Arbitrary holiday descriptions.
  For example:
    "Ram Navami", "Sujit's Birthday"
- arbitrary image directory provided through command-line argument.
  For example:
     ./desktop-calendar <year> <holidays-file-name> <image-directory>
