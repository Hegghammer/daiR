This is a resubmission.

* The file LICENSE has been changed to match the CRAN template.

############################

This is a new submission. 

The package was archived in March after issues were not addressed in time. The issues have since been addressed and the package has been developed further as follows:

* Added functions to deal with the introduction of processor parameters in the Google Document AI API. 
* Added function to generate hOCR files from Document AI output. 
* Modified several functions for increased versatility, for example:
  * Functions that previously took only JSON files as input now also take HTTP response objects.
  * The `draw_` family of functions now allows the user to customize the colour and line width of bounding boxes.
  * `build_token_df()` and `build_block_df()` now include confidence scores in the output, allowing for filtering.

## Test environments
* Local Pop OS Linux, R 4.3.1
* Windows Server 2022 (on Github actions), R 4.3.1
* Ubuntu 22.04 (on Github actions), R 4.3.1
* Mac OS 12 (on Github actions), R 4.3.1

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* New submission
  
## Downstream dependencies
I am not aware of any downstream dependencies.
