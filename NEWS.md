# daiR 0.7.0

- Revised draw_blocks(), draw_paragraphs(), draw_lines(), and draw_tokens() functions. These functions no longer require supplying a pdf file, as they get the images from a base64-encoded string in the json file. 

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
