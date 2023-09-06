# daiR 0.9.9
- Added the function `make_hocr` to convert DAI output to hOCR files, thereby faciliting the creation of searchable PDFs. 

# daiR 0.9.8
- Changed the `build_token_df()` and `build_block_df()` functions so they can take as input response objects from `dai_sync()` in addition to json files from `dai_async()`. 
- Changed the `build_token_df()` and `build_block_df()` functions to include confidence scores in the dataframe, so as to enable filtering on confidence.

# daiR 0.9.7
- Changed the "`draw_*`" functions so they work with response objects (from `dai_sync()` and `dai_sync_tab()`) as well as with json files from `dai_async_tab()`.
- Changed the "`draw_*`" functions to allow customizing color and thickness of lines around bounding boxes.
- Modified `tables_from_dai_response()` so that it handles response objects from `dai_sync()` with form parser processors.

# daiR 0.9.6
- Added three new functions relating to processors: `get_processors()`, `get_processor_info()`, and `get_processor_versions()`. 
- Added parameter `proc_v` to `dai_sync()` and `dai_async`, allowing for specification of processor version.

# daiR 0.9.5

- Added two new functions: `dai_notify()` and `merge_shards()`
- Modified `text_from_dai_response()` and `text_from_dai_file()` to allow saving the output straight to a text file. 
- Fixed a bug in `dai_status()` that caused an error when processing responses from the v1beta2 endpoint (`dai_tab_async()`).  

# daiR 0.9.3

- Changed the "`draw_*`" functions to allow custom output filenames. 

# daiR 0.9.2

- Added more support documentation in connection with JOSS release.

# daiR 0.9.1

- Added new function (`draw_blocks_new()`) to inspect block bounding boxes after reprocessing.

# daiR 0.9.0 

- Initial CRAN version.

# daiR 0.8.0 

- Simplified the auth functions to avoid modifying the global environment. Tokens are no longer held in an internal .auth object. Removed dai_has_token(), dai_deauth(), and create_folder().  

# daiR 0.7.0

- Revised `draw_blocks()`, `draw_paragraphs()`, `draw_lines()`, and `draw_tokens()` functions. These functions no longer require supplying a pdf file, as they get the images from a base64-encoded string in the json file. 

# daiR 0.6.0

- New processing functions adapted to the new stable release of Document AI API (v1). The `dai_sync()`/`dai_async()` functions now access the new v1 endpoint, which has foreign language support. However, the v1 endpoint currently does not support table extraction, so the old processing functions (which access the v1beta2 endpoint) are kept under the new names `dai_sync_tab()` and `dai_async_tab()`. I expect this to be a temporary solution until DAI's capabilities are consolidated in a single endpoint, at which stage the `*_tab()` functions will be phased out.

# daiR 0.4.0

- New table extraction functions. 

# daiR 0.2.0

- New helper functions and more robust code.

# daiR 0.1.0

- Revised and expanded auth functions.

# daiR 0.0.1

- Initial Github release.
