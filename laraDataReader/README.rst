laraDataReader
==============

A collection of data reader functions.

 * layoutReader: importing plate layouts
 
 
layoutReader
____________

This functions import plate layout files that describes the layout of a microtiterplate/container.
It is a very flexible functions that allows multiple (almost infinite) components.
The plate layout geometry is also not limited (only restriction is rectengular shape), so, e.g. 16, 26, 96 well plates are as well definable as 384 or even 1536 well plates.

Samples are classified by a type and description.
Volumes and concentrations are also addable (s. demos and template files)

Testing
_______

 to run tests, change to tests directory and execute run_tests.R
 
    cd tests
    Rscript run_tests.R

TODO
____
  
  * unit information in all layouts
  * testing of barcode variations, intervals
  * no-comma usage warning in all templates
  * units (infile, parameter)
  * testing geometry (stability)
  * speed 
  * changing to R6 classes (https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html)

Installation of lara-R script
_______________________________

  *    


Installation of required packages
---------------------------------


References
__________

.. _pip: https://pypi.python.org/pypi/pip
.. _virtualenv: https://pypi.python.org/pypi/virtualenv
.. _virtualenvwrapper: http://virtualenvwrapper.readthedocs.org/
