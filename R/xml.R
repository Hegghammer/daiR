#' Make hOCR file
#'
#' @description Creates a hOCR file from Document AI output.

#' @param type one of "sync" or "async" depending on
#' the function used to process the original document.
#' @param output either a HTTP response object (from `dai_sync()`) or
#' the path to a JSON file (from `dai_async`).
#' @param outfile_name a string with the desired filename. Must end with
#' either `.hocr`, `.html`, or `.xml`. 
#' @param dir a string with the path to the desired output directory.
#' @return no return value, called for side effects.
#'
#' @details hOCR is an open standard of data representation for formatted
#' text obtained from optical character recognition. It can be used to
#' generate searchable PDFs and many other things. This function generates
#' a file compliant with the official hOCR specification 
#' (https://github.com/kba/hocr-spec) complete with token-level confidence 
#' scores. It also works with non-latin scripts and right-to-left languages.
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' make_hocr(type = "async", output = "output.json")
#' resp <- dai_sync("file.pdf")
#' make_hocr(type = "sync", output = resp)
#' make_hocr(type = "sync", output = resp, outfile_name = "myfile.xml")
#' }

make_hocr <- function(type,
                      output,
                      outfile_name = "out.hocr",
                      dir = getwd()
) {

    # checks
    if (!(length(type) == 1) || !(type %in% c("sync", "async"))) {
        stop("Invalid type parameter.")
    }

    if (!(inherits(output, "response") || is_json(output))) {
        stop("Invalid output parameter.")
    }

    if (length(dir) > 1 || !(is.character(dir))) {
        stop("Invalid dir parameter. Must be a valid folder path.")
    }

    if (!(length(outfile_name) == 1) || !(is.character(dir))) {
        stop("Invalid outfile_name parameter. Must be a string ending with .hocr, .html, or .xml.")
    }

    extension <- tolower(stringr::str_extract(outfile_name, "(?<=\\.)\\w{3,4}$"))
    supported <- c("hocr", "html", "xml")

    if (!(extension %in% supported)) {
        stop("outfile_name extension must be of type .hocr, .html, or .xml")
    }

    if (grepl("/|\\\\", outfile_name)) {
        stop("outfile_name cannot contain slashes. Use the dir parameter for directory selection.")
    }

    # Create doc
    hocr_stem <- fs::path_package("extdata", "hocr_stem.xml", package = "daiR")
    doc <- xml2::read_xml(hocr_stem)

    message("Generating hOCR file. This may take a few seconds.")

    if (type == "sync") {

        parsed_sync <- httr::content(output)

        # Add metadata
        langs <- paste(parsed_sync[["document"]][["pages"]][[1]][["detectedLanguages"]][[1]][["languageCode"]])
        n_pages <- length(parsed_sync[["document"]][["pages"]])
        head <- xml2::xml_children(doc)[[1]]
        lang_node <- xml2::xml_children(head)[[4]]
        xml2::xml_attrs(lang_node)[[2]] <- langs
        pages_node <- xml2::xml_children(head)[[5]]
        xml2::xml_attrs(pages_node)[[2]] <- n_pages

        # Add text content
        text <- parsed_sync[["document"]][["text"]]
        body <- xml2::xml_children(doc)[[2]]
        pages <- parsed_sync[["document"]][["pages"]]

        for (i in seq_along(pages)) {

            message(glue::glue("Processing page {i} of {n_pages} .."))
            id <- paste0("page_", i)
            x2_page <- parsed_sync[["document"]][["pages"]][[i]][["dimension"]][["width"]]
            y2_page <- parsed_sync[["document"]][["pages"]][[i]][["dimension"]][["height"]]
            bbox <- paste0("bbox 0 0 ", x2_page, " ", y2_page)
            xml2::xml_add_child(body, "div", class = 'ocr_page', id = id, title = bbox)
            div1 <- xml2::xml_children(body)[[i]]
            blocks <- parsed_sync[["document"]][["pages"]][[i]][["blocks"]]
            block_coords <- get_vertices(blocks)
            block_coords <- purrr::map(block_coords, transpose_block)

            for (q in seq_along(block_coords)) {
                block_coords[[q]][block_coords[[q]] < 0] = 0 # avoid negatives
                block_coords[[q]][["xs"]] <- round(block_coords[[q]][["xs"]] * x2_page)
                block_coords[[q]][["ys"]] <- round(block_coords[[q]][["ys"]] * y2_page)
            }

            block_segments <- purrr::map(blocks, ~.x[["layout"]][["textAnchor"]][["textSegments"]][[1]])
            if (is.null(block_segments[[1]][["startIndex"]])) block_segments[[1]][["startIndex"]] <- 0

            for (j in seq_along(block_coords)) {

                id <- paste0("block_", i, "_", j)
                x1 <- block_coords[[j]][["xs"]][[1]]
                y1 <- block_coords[[j]][["ys"]][[1]]
                x2 <- block_coords[[j]][["xs"]][[3]]
                y2 <- block_coords[[j]][["ys"]][[3]]
                bbox <- paste0("bbox ", x1, " ", y1, " ", x2, " ", y2)
                xml2::xml_add_child(div1, "div", class = 'ocr_carea', id = id, title = bbox)
                div2 <- xml2::xml_children(div1)[[j]]

                paras <- parsed_sync[["document"]][["pages"]][[i]][["paragraphs"]]
                all_para_coords <- purrr::map(paras, ~.x[["layout"]][["boundingPoly"]][["normalizedVertices"]])
                all_para_coords <- purrr::map(all_para_coords , transpose_block)

                for (q in seq_along(all_para_coords)) {
                    all_para_coords[[q]][all_para_coords[[q]] < 0] = 0
                    all_para_coords[[q]][["xs"]] <- round(all_para_coords[[q]][["xs"]] * x2_page)
                    all_para_coords[[q]][["ys"]] <- round(all_para_coords[[q]][["ys"]] * y2_page)
                }
                all_para_segments <- purrr::map(paras, ~.x[["layout"]][["textAnchor"]][["textSegments"]][[1]])
                if (is.null(all_para_segments[[1]][["startIndex"]])) all_para_segments[[1]][["startIndex"]] <- 0
                block_start_ind <- as.integer(block_segments[[j]][["startIndex"]])
                block_end_ind <- as.integer(block_segments[[j]][["endIndex"]])
                para_coords <- list()
                para_segments <- list()
                for (z in seq_along(all_para_segments)) {
                    start <- as.integer(all_para_segments[[z]][["startIndex"]])
                    end <- as.integer(all_para_segments[[z]][["endIndex"]])
                    if ((start >= block_start_ind) && (end <= block_end_ind)) {
                        para_coords <- append(para_coords, all_para_coords[z])
                        para_segments <- append(para_segments, all_para_segments[z])
                    }
                }

                for (k in seq_along(para_coords)) {

                    id <- paste0("par_", i, "_", k)
                    x1 <- para_coords[[k]][["xs"]][[1]]
                    y1 <- para_coords[[k]][["ys"]][[1]]
                    x2 <- para_coords[[k]][["xs"]][[3]]
                    y2 <- para_coords[[k]][["ys"]][[3]]
                    bbox <- paste0("bbox ", x1, " ", y1, " ", x2, " ", y2)
                    xml2::xml_add_child(div2, "p", class = 'ocr_par', id = id, title = bbox)
                    p <- xml2::xml_children(div2)[[k]]

                    lines <- parsed_sync[["document"]][["pages"]][[i]][["lines"]]
                    all_line_coords <- purrr::map(lines, ~.x[["layout"]][["boundingPoly"]][["normalizedVertices"]])
                    all_line_coords <- purrr::map(all_line_coords , transpose_block)

                    for (q in seq_along(all_line_coords)) {
                        all_line_coords[[q]][all_line_coords[[q]] < 0] = 0
                        all_line_coords[[q]][["xs"]] <- round(all_line_coords[[q]][["xs"]] * x2_page)
                        all_line_coords[[q]][["ys"]] <- round(all_line_coords[[q]][["ys"]] * y2_page)
                    }
                    all_line_segments <- purrr::map(lines, ~.x[["layout"]][["textAnchor"]][["textSegments"]][[1]])
                    if (is.null(all_line_segments[[1]][["startIndex"]])) all_line_segments[[1]][["startIndex"]] <- 0
                    para_start_ind <- as.integer(para_segments[[k]][["startIndex"]])
                    para_end_ind <- as.integer(para_segments[[k]][["endIndex"]])
                    line_coords <- list()
                    line_segments <- list()
                    for (z in seq_along(all_line_segments)) {
                        start <- as.integer(all_line_segments[[z]][["startIndex"]])
                        end <- as.integer(all_line_segments[[z]][["endIndex"]])
                        if ((start >= para_start_ind) && (end <= para_end_ind)) {
                            line_coords <- append(line_coords, all_line_coords[z])
                            line_segments <- append(line_segments, all_line_segments[z])
                        }
                    }

                    for (l in seq_along(line_coords)) {

                        id <- paste0("line_", i, "_", l)
                        x1 <- line_coords[[l]][["xs"]][[1]]
                        y1 <- line_coords[[l]][["ys"]][[1]]
                        x2 <- line_coords[[l]][["xs"]][[3]]
                        y2 <- line_coords[[l]][["ys"]][[3]]
                        bbox <- paste0("bbox ", x1, " ", y1, " ", x2, " ", y2)
                        xml2::xml_add_child(p, "span", class = 'ocr_line', id = id, title = bbox)
                        span1 <- xml2::xml_children(p)[[l]]

                        tokens <- parsed_sync[["document"]][["pages"]][[i]][["tokens"]]
                        all_token_coords <- purrr::map(tokens, ~.x[["layout"]][["boundingPoly"]][["normalizedVertices"]])
                        all_token_coords <- purrr::map(all_token_coords , transpose_block)

                        for (q in seq_along(all_token_coords)) {
                            all_token_coords[[q]][all_token_coords[[q]] < 0] = 0
                            all_token_coords[[q]][["xs"]] <- round(all_token_coords[[q]][["xs"]] * x2_page)
                            all_token_coords[[q]][["ys"]] <- round(all_token_coords[[q]][["ys"]] * y2_page)
                        }
                        all_token_segments <- purrr::map(tokens, ~.x[["layout"]][["textAnchor"]][["textSegments"]][[1]])
                        if (is.null(all_token_segments[[1]][["startIndex"]])) all_token_segments[[1]][["startIndex"]] <- 0

                        all_token_confs <- unlist(purrr::map(tokens, ~.x[["layout"]][["confidence"]]))
                        all_token_confs <- round(all_token_confs * 100, 2)
                        line_start_ind <- as.integer(line_segments[[l]][["startIndex"]])
                        line_end_ind <- as.integer(line_segments[[l]][["endIndex"]])
                        token_coords <- list()
                        token_segments <- list()
                        token_confs <- list()
                        for (z in seq_along(all_token_segments)) {
                            start <- as.integer(all_token_segments[[z]][["startIndex"]])
                            end <- as.integer(all_token_segments[[z]][["endIndex"]])
                            if ((start >= line_start_ind) && (end <= line_end_ind)) {
                                token_coords <- append(token_coords, all_token_coords[z])
                                token_segments <- append(token_segments, all_token_segments[z])
                                token_confs <- append(token_confs, all_token_confs[z])
                            }
                        }

                        for (m in seq_along(token_coords)) {

                            id <- paste0("word_", i, "_", m)
                            x1 <- token_coords[[m]][["xs"]][[1]]
                            y1 <- token_coords[[m]][["ys"]][[1]]
                            x2 <- token_coords[[m]][["xs"]][[3]]
                            y2 <- token_coords[[m]][["ys"]][[3]]
                            conf <- token_confs[[m]]
                            bbox <- paste0("bbox ", x1, " ", y1, " ", x2, " ", y2, "; x_wconf ", conf)
                            xml2::xml_add_child(span1, "span", class = 'ocrx_word', id = id, title = bbox)
                            span2 <- xml2::xml_children(span1)[[m]]
                            start_ind <- end_ind <- token_segments[[m]][["startIndex"]]
                            if (is.null(start_ind)) start_ind <- 0
                            end_ind <- token_segments[[m]][["endIndex"]]
                            word <- substr(text, start_ind, end_ind)
                            if (substr(word, 2, 2) %in% c(".", ",", ";")) {
                                word <- sub('^.', '', word)
                            }
                            if (substr(word, 1, 1) == "-") {
                                word <- sub('^-', '', word)
                            }
                            xml2::xml_text(span2) <- word
                        }
                    }
                }
            }
        }
    }

    else if (type == "async") {

        parsed <- jsonlite::fromJSON(output)

        # Add metadata
        langs <- paste(parsed[["pages"]][["detectedLanguages"]][[1]][["languageCode"]], collapse = " ")
        n_pages <- nrow(parsed$pages)
        head <- xml2::xml_children(doc)[[1]]
        lang_node <- xml2::xml_children(head)[[4]]
        xml2::xml_attrs(lang_node)[[2]] <- langs
        pages_node <- xml2::xml_children(head)[[5]]
        xml2::xml_attrs(pages_node)[[2]] <- n_pages

        # Add text content
        text <- parsed$text
        body <- xml2::xml_children(doc)[[2]]
        page_coords <- parsed[["pages"]][["layout"]][["boundingPoly"]][["vertices"]]

        for (i in seq_along(page_coords)) {

            message(glue::glue("Processing page {i} of {n_pages} .."))
            id <- paste0("page_", i)
            x2_page <- page_coords[[i]][["x"]][[3]]
            y2_page <- page_coords[[i]][["y"]][[3]]
            bbox <- paste0("bbox 0 0 ", x2_page, " ", y2_page)
            xml2::xml_add_child(body, "div", class = 'ocr_page', id = id, title = bbox)
            div1 <- xml2::xml_children(body)[[i]]
            block_coords <- parsed[["pages"]][["blocks"]][[i]][["layout"]][["boundingPoly"]][["normalizedVertices"]]
            for (q in seq_along(block_coords)) {
                block_coords[[q]][block_coords[[q]] < 0] = 0 # avoid negatives
                block_coords[[q]][["x"]] <- round(block_coords[[q]][["x"]] * x2_page)
                block_coords[[q]][["y"]] <- round(block_coords[[q]][["y"]] * y2_page)
            }
            block_segments <- parsed[["pages"]][["blocks"]][[i]][["layout"]][["textAnchor"]][["textSegments"]]
            if (is.null(block_segments[[1]][["startIndex"]])) block_segments[[1]][["startIndex"]] <- 0

            for (j in seq_along(block_coords)) {

                id <- paste0("block_", i, "_", j)
                x1 <- block_coords[[j]][["x"]][[1]]
                y1 <- block_coords[[j]][["y"]][[1]]
                x2 <- block_coords[[j]][["x"]][[3]]
                y2 <- block_coords[[j]][["y"]][[3]]
                bbox <- paste0("bbox ", x1, " ", y1, " ", x2, " ", y2)
                xml2::xml_add_child(div1, "div", class = 'ocr_carea', id = id, title = bbox)
                div2 <- xml2::xml_children(div1)[[j]]

                all_para_coords <- parsed[["pages"]][["paragraphs"]][[i]][["layout"]][["boundingPoly"]][["normalizedVertices"]]
                for (q in seq_along(all_para_coords)) {
                    all_para_coords[[q]][all_para_coords[[q]] < 0] = 0
                    all_para_coords[[q]][["x"]] <- round(all_para_coords[[q]][["x"]] * x2_page)
                    all_para_coords[[q]][["y"]] <- round(all_para_coords[[q]][["y"]] * y2_page)
                }

                all_para_segments <- parsed[["pages"]][["paragraphs"]][[i]][["layout"]][["textAnchor"]][["textSegments"]]
                if (is.null(all_para_segments[[1]][["startIndex"]])) all_para_segments[[1]][["startIndex"]] <- 0
                block_start_ind <- as.integer(block_segments[[j]][["startIndex"]])
                block_end_ind <- as.integer(block_segments[[j]][["endIndex"]])
                para_coords <- list()
                para_segments <- list()
                for (z in seq_along(all_para_segments)) {
                    start <- as.integer(all_para_segments[[z]][["startIndex"]])
                    end <- as.integer(all_para_segments[[z]][["endIndex"]])
                    if ((start >= block_start_ind) && (end <= block_end_ind)) {
                        para_coords <- append(para_coords, all_para_coords[z])
                        para_segments <- append(para_segments, all_para_segments[z])
                    }
                }

                for (k in seq_along(para_coords)) {

                    id <- paste0("par_", i, "_", k)
                    x1 <- para_coords[[k]][["x"]][[1]]
                    y1 <- para_coords[[k]][["y"]][[1]]
                    x2 <- para_coords[[k]][["x"]][[3]]
                    y2 <- para_coords[[k]][["y"]][[3]]
                    bbox <- paste0("bbox ", x1, " ", y1, " ", x2, " ", y2)
                    xml2::xml_add_child(div2, "p", class = 'ocr_par', id = id, title = bbox)
                    p <- xml2::xml_children(div2)[[k]]

                    all_line_coords <- parsed[["pages"]][["lines"]][[i]][["layout"]][["boundingPoly"]][["normalizedVertices"]]
                    for (q in seq_along(all_line_coords)) {
                        all_line_coords[[q]][all_line_coords[[q]] < 0] = 0
                        all_line_coords[[q]][["x"]] <- round(all_line_coords[[q]][["x"]] * x2_page)
                        all_line_coords[[q]][["y"]] <- round(all_line_coords[[q]][["y"]] * y2_page)
                    }
                    all_line_segments <- parsed[["pages"]][["lines"]][[i]][["layout"]][["textAnchor"]][["textSegments"]]
                    if (is.null(all_line_segments[[1]][["startIndex"]])) all_line_segments[[1]][["startIndex"]] <- 0
                    para_start_ind <- as.integer(para_segments[[k]][["startIndex"]])
                    para_end_ind <- as.integer(para_segments[[k]][["endIndex"]])
                    line_coords <- list()
                    line_segments <- list()
                    for (z in seq_along(all_line_segments)) {
                        start <- as.integer(all_line_segments[[z]][["startIndex"]])
                        end <- as.integer(all_line_segments[[z]][["endIndex"]])
                        if ((start >= para_start_ind) && (end <= para_end_ind)) {
                            line_coords <- append(line_coords, all_line_coords[z])
                            line_segments <- append(line_segments, all_line_segments[z])
                        }
                    }

                    for (l in seq_along(line_coords)) {

                        id <- paste0("line_", i, "_", l)
                        x1 <- line_coords[[l]][["x"]][[1]]
                        y1 <- line_coords[[l]][["y"]][[1]]
                        x2 <- line_coords[[l]][["x"]][[3]]
                        y2 <- line_coords[[l]][["y"]][[3]]
                        bbox <- paste0("bbox ", x1, " ", y1, " ", x2, " ", y2)
                        xml2::xml_add_child(p, "span", class = 'ocr_line', id = id, title = bbox)
                        span1 <- xml2::xml_children(p)[[l]]

                        all_token_coords <- parsed[["pages"]][["tokens"]][[i]][["layout"]][["boundingPoly"]][["normalizedVertices"]]
                        for (q in seq_along(all_token_coords)) {
                            all_token_coords[[q]][all_token_coords[[q]] < 0] = 0
                            all_token_coords[[q]][["x"]] <- round(all_token_coords[[q]][["x"]] * x2_page)
                            all_token_coords[[q]][["y"]] <- round(all_token_coords[[q]][["y"]] * y2_page)
                        }
                        all_token_segments <- parsed[["pages"]][["tokens"]][[i]][["layout"]][["textAnchor"]][["textSegments"]]
                        if (is.null(all_token_segments[[1]][["startIndex"]])) all_token_segments[[1]][["startIndex"]] <- 0
                        all_token_confs <- parsed[["pages"]][["tokens"]][[i]][["layout"]][["confidence"]]
                        all_token_confs <- round(all_token_confs * 100, 2)

                        line_start_ind <- as.integer(line_segments[[l]][["startIndex"]])
                        line_end_ind <- as.integer(line_segments[[l]][["endIndex"]])
                        token_coords <- list()
                        token_segments <- list()
                        token_confs <- list()
                        for (z in seq_along(all_token_segments)) {
                            start <- as.integer(all_token_segments[[z]][["startIndex"]])
                            end <- as.integer(all_token_segments[[z]][["endIndex"]])
                            if ((start >= line_start_ind) && (end <= line_end_ind)) {
                                token_coords <- append(token_coords, all_token_coords[z])
                                token_segments <- append(token_segments, all_token_segments[z])
                                token_confs <- append(token_confs, all_token_confs[z])
                            }
                        }

                        for (m in seq_along(token_coords)) {

                            id <- paste0("word_", i, "_", m)
                            x1 <- token_coords[[m]][["x"]][[1]]
                            y1 <- token_coords[[m]][["y"]][[1]]
                            x2 <- token_coords[[m]][["x"]][[3]]
                            y2 <- token_coords[[m]][["y"]][[3]]
                            conf <- token_confs[[m]]
                            bbox <- paste0("bbox ", x1, " ", y1, " ", x2, " ", y2, "; x_wconf ", conf)
                            xml2::xml_add_child(span1, "span", class = 'ocrx_word', id = id, title = bbox)
                            span2 <- xml2::xml_children(span1)[[m]]
                            start_ind <- end_ind <- token_segments[[m]][["startIndex"]]
                            if (is.null(start_ind)) start_ind <- 0
                            end_ind <- token_segments[[m]][["endIndex"]]
                            word <- substr(text, start_ind, end_ind)
                            if (substr(word, 2, 2) %in% c(".", ",", ";")) {
                                word <- sub('^.', '', word)
                            }
                            if (substr(word, 1, 1) == "-") {
                                word <- sub('^-', '', word)
                            }
                            xml2::xml_text(span2) <- word
                        }
                    }
                }
            }
        }
    }
    dest <- file.path(dir, outfile_name)
    xml2::write_xml(doc, dest)
    message(glue::glue("hOCR file named '{outfile_name}' generated in {dir}."))
}