This is a new release.

It incorporates recent changes in the Document AI API, notably the structural shift from several endpoints to one endpoint with multiple processors. Google now appears to consider the service as mature, so a major release of `daiR` appears appropriate.

Main package modifications: 

* Added several processor-related functions: `list_processor_types()`, `create_processor()`, `enable_processor()`, `disable_processor()`, and `delete_processor()`.
  
* Added `get_text()` and `get_tables()` as parsimonious replacements of `text_from_dai_response()`, `text_from_dai_file()`, `tables_from_dai_response()` and `tables_from_dai_file()`.

* Added `get_entities()` and `draw_entities()` to make use of Document AI's new form parser processor.

* Removed `dai_tab_sync()` and `dai_tab_async()` following Google's discontinuation of the v1beta2 endpoint on 31 January 2024.

* Modified the parameters of the `draw*()` functions for better consistency with other functions.

* Renamed the `.R` files and regrouped the functions.

## Test environments
* Local Pop OS Linux, R 4.3.2
* Windows Server 2022 (on Github actions), R 4.3.2
* Ubuntu 22.04 (on Github actions), R 4.3.2
* Mac OS 12.7.3 (on Github actions), R 4.3.2

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs.
  
## Downstream dependencies
I am not aware of any downstream dependencies.

############################################

This is a resubmission.

* The file LICENSE has been changed to match the CRAN template.

############################################

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

###############################################

## Resubmission

This is a resubmission. In this version I have addressed Gregor Seyers comments. I have:

* put 'daiR' in single quotes throughout, except in the top line of the DESCRIPTION
file, as `devtools::check(cran=TRUE)` throws an error if I do. 
* put 'Document AI' in single quotes throughout in the DESCRIPTION file.
* added a web reference for the API in the DESCRIPTION file.
* added `\value` to all .Rd files that didn't have it and reviewed all `\value`
entries to make sure they communicate the structure/class and meaning of the output,
including in the places where no value is returned. 
* removed all instances I could find of functions writing to the user's homespace. 
I checked all the examples, tests, vignettes, as well as readme.md and changed to tempdir()
throughout.
* removed the function that wrote to the global environment. I should mention that 
the function --- which creates an `.auth` object on load to store access tokens --- 
was borrowed from a set of large R packages currently on CRAN, notably 
['bigRQuery'](https://github.com/r-dbi/bigrquery/blob/main/R/zzz.R) and 
['googledrive'](https://github.com/tidyverse/googledrive/blob/master/R/zzz.R).
This led me to believe that CRAN makes exceptions for credential-storing functions.
My new authentication solution works, but in case it breaks, it would be useful to know 
whether CRAN does indeed allow this particular operation. (I'm assuming the
maintainers of the other packages use it for good reason.)

I also made some additional changes. I have:

* removed two functions (`dai_has_token()` and `dai_deauth`) that are redundant under 
the new authentication solution.
* removed one function (`create_folder()`) that I found on closer inspection to be  
unnecessary.
* rewritten several function descriptions (in the .Rd files) for improved clarity 
and consistency.
* revised news.md and the vignettes to reflect the above changes. 
* changed the new version number to 0.9.0 in view of the scale of the combined changes.  

## Test environments
* local Win 10 Enterprise install, R 4.1.0
* windows 10.0.17763 (on Github actions), R 4.1.0
* ubuntu 20.04 (on Github actions), R 4.1.0
* mac OS 10.15 (on Github actions), R 4.1.0
* windows (on WinBuilder), R Devel
* fedora 24 (on rhub), R Devel

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE on rhub and WinBuilder:

* New submission 

## Downstream dependencies
I am not aware of any downstream dependencies.

################################################

## Package history 
This is a first submission.

## Test environments
* local Win 10 Enterprise install, R 4.1.0
* windows 10.0.17763 (on Github actions), R 4.1.0
* ubuntu 20.04 (on Github actions), R 4.1.0
* mac OS 10.15 (on Github actions), R 4.1.0
* windows (on WinBuilder), R Devel
* fedora 24 (on rhub), R Devel

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE on rhub and WinBuilder:

* Possibly mis-spelled words in DESCRIPTION:
  JSON (14:39)
  daiR (13:15, 14:77)
  
  These are proper names. 

## Downstream dependencies
I am not aware of any downstream dependencies.
