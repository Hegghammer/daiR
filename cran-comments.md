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
