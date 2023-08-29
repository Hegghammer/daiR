#' Get text from HTTP response object
#'
#' @description Extracts the text OCRed by Document AI (DAI) in a
#' synchronous processing request.
#'
#' @param response an HTTP response object returned by \code{dai_sync()}
#' @param save_to_file boolean; whether to save the text as a .txt file
#' @param dest_dir folder path for the .txt output file if \code{save_to_file = TRUE}
#' @param filename string to form the stem of the .txt output file
#' @return a string (if \code{save_to_file = FALSE})
#' @export
#'
#' @examples
#' \dontrun{
#' text <- text_from_dai_response(response)
#'
#' text_from_dai_response(response, save_to_file = TRUE)
#' }

text_from_dai_response <- function(response,
                                   save_to_file = FALSE,
                                   dest_dir = getwd(),
                                   filename = "output"
                                   ) {

  # checks
  if (!(inherits(response, "response"))) {
    stop("Input is not a valid HTTP response.")
    }

  parsed <- httr::content(response, as = "parsed")

  if (!("pages" %in% names(parsed$document) || "pages" %in% names(parsed))) {
    stop("Input not recognized. Is it from dai_async?")
  }

  if (!("text" %in% names(parsed$document) || "text" %in% names(parsed))) {
    stop("DAI found no text. Was the page blank?")
  }

  if (!(save_to_file %in% c(TRUE, FALSE))) {
    stop("Invalid save_to_file argument. Must be either TRUE or FALSE.")
  }

  if (!(length(dest_dir) == 1)) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }

  if (!(is.character(dest_dir))) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }

  if (!(length(filename) == 1)) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }

  if (!(is.character(filename))) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }

  if (nchar(filename) > 200) {
    stop("Filename too long.")
  }

  # get text
  if ("text" %in% names(parsed$document)) {
    text <- parsed$document$text
  } else {
  	text <- parsed$text
  }

  # save to file if requested
  if (isTRUE(save_to_file)) {
    write(text, file.path(dest_dir, paste0(filename, ".txt")))
    } else {
      return(text)
      }

  }

#' Get text from output file
#'
#' @description Extracts the text OCRed by Document AI (DAI) in an
#' asynchronous processing request.
#'
#' @param file filepath of a JSON file obtained using \code{dai_async()}
#' @param save_to_file boolean; whether to save the text as a .txt file
#' @param dest_dir folder path for the .txt output file if save_to_file=TRUE
#' @return a string (if \code{save_to_file = FALSE})
#' @export
#'
#' @examples
#' \dontrun{
#' text <- text_from_dai_file("mydoc-0.json")
#' text_from_dai_file("mydoc-0.json", save_to_file = TRUE)
#' }

text_from_dai_file <- function(file,
                               save_to_file = FALSE,
                               dest_dir = getwd()
                               ) {

  # checks
  if (!(is.character(file) && length(file) == 1)) {
    stop("Invalid file input.")
    }

  if (!(is_json(file))) {
    stop("Input file not .json. Is the file in your working directory?")
    }

  output <- jsonlite::fromJSON(file)

  if (!("pages" %in% names(output))) {
    stop("JSON not in right format. Is it from DAI?")
    }

  if (!("text" %in% names(output))) {
    stop("DAI found no text. Was the document blank?")
    }

  if (!(save_to_file %in% c(TRUE, FALSE))) {
    stop("Invalid save_to_file argument. Must be either TRUE or FALSE.")
  }

  if (!(length(dest_dir) == 1)) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }

  if (!(is.character(dest_dir))) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
  }

  # get text
  text <- output$text

  if (isTRUE(save_to_file)) {
    stem <- gsub("\\.json", "", basename(file))
    write(text, file.path(dest_dir, paste0(stem, ".txt")))
    } else {
      return(text)
    }
  }

#' Merge shards
#'
#' @description Merges text files from Document AI output shards into a
#' single text file corresponding to the parent document.
#'
#' @param source_dir folder path for input files
#' @param dest_dir folder path for output files
#' @return no return value, called for side effects
#'
#' @export
#'
#' @details The function works on .txt files generated from .json output files,
#' not on .json files directly. It also presupposes that the .txt filenames
#' have the same name stems as the .json files from which they were extracted.
#' For the v1 API, this means files ending with "-0.txt", "-1.txt", "-2.txt",
#' and so forth. For the v1beta2 API, it means files ending with
#' "-page-1-to-100.txt", "-page-101-to-200.txt", etc. The safest approach is
#' to generate .txt files using `text_from_dai_file()` with the `save_to_file`
#' parameter set to TRUE.
#'
#' @examples
#' \dontrun{
#' merge_shards(source_dir = getwd(), dest_dir = tempdir())
#' }

merge_shards <- function(source_dir,
                         dest_dir
                         ) {

  if (length(source_dir) > 1) {
    stop("Invalid source_dir argument. Must be a valid folder path.")
    }

  if (!(is.character(source_dir))) {
    stop("Invalid source_dir argument. Must be a valid folder path.")
    }

  if (!(length(dest_dir) == 1)) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
    }

  if (!(is.character(dest_dir))) {
    stop("Invalid dest_dir argument. Must be a valid folder path.")
    }

  files <- list.files(source_dir, pattern = "*txt$", full.names = TRUE)

  if (length(files) == 0) {
    stop("No .txt files found.")
    }

  if (isFALSE(grepl("-\\d{1,3}\\.txt", files)) && isFALSE(grepl("-page-\\d{1,4}-to-\\d{1,4}", files))) {
    stop("The .txt files are incorrectly formatted. Are they from Document AI output shards?")
    }

  all <- grep("-\\d{1,3}\\.txt", files, value = TRUE)

  v1beta2 <- grep("-page-\\d{1,4}-to-\\d{1,4}", files, value = TRUE)

  v1 <- all[!all %in% v1beta2]

  v1beta2_stems <- unique(gsub("\\-page-\\d{1,4}-to-\\d{1,4}\\.txt", "", v1beta2))

  v1_stems <- unique(gsub("-\\d{1,2}\\.txt", "", v1))

  if (length(v1beta2) > 0) {
    for (i in seq_along(v1beta2_stems)) {
      cluster <- grep(v1beta2_stems[i], v1beta2, value = TRUE)
      cluster_df <- readtext::readtext(cluster)
      text <- paste(cluster_df$text, collapse = "\n")
      write(text, file.path(dest_dir, paste0(basename(v1beta2_stems[i]), ".txt")))
      }
    }

  if (length(v1) > 0) {
    for (i in seq_along(v1_stems)) {
      cluster <- grep(v1_stems[i], v1, value = TRUE)
      cluster_df <- readtext::readtext(cluster)
      text <- paste(cluster_df$text, collapse = "\n")
      write(text, file.path(dest_dir, paste0(basename(v1_stems[i]), ".txt")))
      }
    }

  message("Shards merged.")

}

#' Draw block bounding boxes on source document
#'
#' @description Plots the block bounding boxes identified by
#' Document AI (DAI) onto images of the submitted document.
#' Generates an annotated .png file for each page in the original
#' document.
#'
#' @param type one of "sync", "async", "sync-tab" or "async-tab", depending on
#' the function used to process the original document.
#' @param output either a HTTP response object (from `dai_sync()` or
#' `dai_sync_tab()`) or the path to a JSON file (from `dai_async` or
#' `dai_async_tab()`).
#' @param doc filepath to the source document (pdf, tiff, or gif file); only
#' necessary for documents processed with `dai_sync_tab()` or `dai_async_tab()`.
#' @param prefix string to be prepended to the output png filename.
#' @param dir path to the desired output directory.
#' @param linecol color of the bounding box line.
#' @param linewd width of the bounding box line.
#' @param fontcol color of the box numbers.
#' @param fontsize size of the box numbers.
#' @return no return value, called for side effects.
#'
#' @details Not vectorized, but documents can be multi-page.
#'
#' @export
#' @examples
#' \dontrun{
#' resp <- dai_sync("page.pdf")
#' draw_blocks(type = "sync",
#'             output = resp)
#'
#' resp <- dai_sync_tab("page.pdf")
#' draw_blocks(type = "sync-tab",
#'             output = resp,
#'             doc = "page.pdf")
#'
#' draw_blocks(type = "async",
#'             output = "page.json")
#'
#' draw_blocks(type = "async-tab",
#'             output = "page.json",
#'             doc = "page.pdf")
#' }

draw_blocks <- function(type,
						output,
						doc = NA,
						prefix = NULL,
						dir = getwd(),
						linecol = "red",
						linewd = 3,
						fontcol = "blue",
						fontsize = 4
) {

	# checks
	if (!(length(type) == 1) || !(type %in% c("sync", "async", "sync-tab", "async-tab"))) {
		stop("Invalid type parameter.")
	}

	if (!(inherits(output, "response") || is_json(output))) {
		stop("Invalid output parameter.")
	}

	if (length(doc) > 1 || is.numeric(doc)) {
		stop("Invalid doc parameter.")
	}

	if (length(prefix) > 1 || is.numeric(prefix)) {
		stop("Invalid prefix parameter.")
	}

	if (length(dir) > 1 || !(is.character(dir))) {
		stop("Invalid dir parameter. Must be a valid folder path.")
	}

	if (!(length(linecol) == 1) || !(daiR::is_colour(linecol)) || is.na(linecol)) {
		stop("Invalid linecol parameter. Must be a single valid colour representation.")
	}

	if (!(is.numeric(linewd) && length(linewd) == 1)) {
		stop("Invalid linewd parameter. Must be a single number.")
	}

	if (!(length(fontcol) == 1) || !(daiR::is_colour(fontcol)) || is.na(fontcol)) {
		stop("Invalid fontcol parameter. Must be a single valid colour representation.")
	}

	if (!(is.numeric(fontsize) && length(fontsize) == 1)) {
		stop("Invalid fontsize parameter. Must be a single number.")
	}

	# Helper functions
	get_vertices <- function(lst) {
		boxes <- purrr::map(lst, ~.x$layout$boundingPoly$normalizedVertices)
		return(boxes)
	}

	transpose_block <- function(lst) {
        xs <- c(lst[[1]][["x"]], lst[[2]][["x"]], lst[[3]][["x"]], lst[[4]][["x"]])
        ys <- c(lst[[1]][["y"]], lst[[2]][["y"]], lst[[3]][["y"]], lst[[4]][["y"]])
        df <- data.frame(xs, ys)
        return(df)
	}

	transpose_page <- function(x) {
		blocks <- purrr::map(x, transpose_block)
		return(blocks)
	}

	if (type == "sync") {

		if (!(inherits(output, "response"))) {
			stop("Output parameter not pointing to valid response object.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- httr::content(output)
		pages <- parsed$document$pages
		pages_blocks <- purrr::map(pages, ~.x$blocks)
		pagewise_block_sets <- purrr::map(pages_blocks, get_vertices)
		pagewise_block_sets <- purrr::map(pagewise_block_sets, transpose_page)

		# decode base64 and save to temp images
		page_imgs_base64 <- unlist(purrr::map(pages, ~.x$image$content))
		imgs <- character()
		for (i in seq_along(page_imgs_base64)) {
			path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
			outconn <- file(path, "wb")
			base64enc::base64decode(page_imgs_base64[i], outconn)
			close(outconn)
			imgs <- c(imgs, path)
		}

	} else if (type == "async") {

		if (!(is_json(output))) {
			stop("Output parameter not pointing to valid JSON file.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- jsonlite::fromJSON(output)
		pages_blocks <- parsed$pages$blocks
		pagewise_block_sets <- purrr::map(pages_blocks, ~.x$layout$boundingPoly$normalizedVertices)

		# decode base64 and save to temp images
		page_imgs_base64 <- parsed$pages$image$content
		imgs <- character()
		for (i in seq_along(page_imgs_base64)) {
			path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
			outconn <- file(path, "wb")
			base64enc::base64decode(page_imgs_base64[i], outconn)
			close(outconn)
			imgs <- c(imgs, path)
		}

	} else if (type == "sync-tab") {

		if (!(inherits(output, "response"))) {
			stop("Output parameter not pointing to valid response object.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- httr::content(output)
		pages <- parsed$pages
		pages_blocks <- purrr::map(pages, ~.x$blocks)
		pagewise_block_sets <- purrr::map(pages_blocks, get_vertices)
		pagewise_block_sets <- purrr::map(pagewise_block_sets, transpose_page)

		# Get vector of images from source doc
		if (grepl("pdf$", doc)) {
			pgs <- magick::image_read_pdf(doc)
			imgs <- character()
			for (i in seq_along(pgs)) {
				path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
				magick::image_write(pgs[i], path)
				imgs <- c(imgs, path)
			}
		} else {
			imgs <- doc
		}

	} else if (type == "async-tab") {

		if (!(is_json(output))) {
			stop("Output parameter not pointing to valid JSON file.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- jsonlite::fromJSON(output)
		pages_blocks <- parsed$pages$blocks
		pagewise_block_sets <- purrr::map(pages_blocks, ~.x$layout$boundingPoly$normalizedVertices)

		# Get vector of images from source doc
		if (grepl("pdf$", doc)) {
			pgs <- magick::image_read_pdf(doc)
			imgs <- character()
			for (i in seq_along(pgs)) {
				path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
				magick::image_write(pgs[i], path)
				imgs <- c(imgs, path)
			}
		} else {
			imgs <- doc
		}

	}

	# loop over the pagewise sets
	for (i in seq_along(pagewise_block_sets)) {

		img <- magick::image_read(imgs[i])

		# get image dimensions
		info <- magick::image_info(img)

		# prepare for plotting on image
		canvas <- magick::image_draw(img)

		# set counter for box number
		counter <- 1

		#loop over boxes on the page
		for (box in pagewise_block_sets[[i]]) {

			# handle NAs in boxes on top or left edge
			if (is.na(box$y[1])) box$y[1] <- 0
			if (is.na(box$y[2])) box$y[2] <- 0
			if (is.na(box$x[1])) box$x[1] <- 0
			if (is.na(box$x[4])) box$x[4] <- 0

			# transform from relative to absolute coordinates
			box$x1 <- box$x * info$width

			box$y1 <- box$y * info$height

			# draw polygon
			graphics::polygon(x = box$x1,
												y = box$y1,
												border = linecol,
												lwd = linewd
			)

			graphics::text(x = box$x1[1],
										 y = box$y1[1],
										 label = counter,
										 col = fontcol,
										 cex = fontsize,
										 family = "Liberation Sans"
			)

			counter <- counter + 1

		}

		# write annotated image to file

		if (type %in% c("async", "async-tab")) {
			default_prefix <- substr(basename(output), 1, nchar(basename(output)) - 5)
		} else {
			default_prefix <- "document"
		}

		if (is.null(prefix)) {
			filename <- glue::glue("{default_prefix}_page{i}_blocks.png")
		} else {
			filename <- glue::glue("{prefix}_page{i}_blocks.png")
		}

		dest <- file.path(dir, filename)

		magick::image_write(canvas, format = "png", dest)

		grDevices::dev.off()

	}

	pages <- length(pages_blocks)

	message(glue::glue("Generated {pages} annotated image(s)."))

}

#' Draw paragraph bounding boxes on source document
#'
#' @description Plots the paragraph bounding boxes identified by
#' Document AI (DAI) onto images of the submitted document.
#' Generates an annotated .png file for each page in the original
#' document.
#'
#' @param type one of "sync", "async", "sync-tab" or "async-tab", depending on
#' the function used to process the document.
#' @param output either a HTTP response object (from `dai_sync()` or
#' `dai_sync_tab()`) or the path to a JSON file (from `dai_async` or
#' `dai_async_tab()`).
#' @param doc filepath to the source document (pdf, tiff, or gif file); only
#' necessary for documents processed with `dai_sync_tab()` or `dai_async_tab()`.
#' @param prefix string to be prepended to the output png filename.
#' @param dir path to the desired output directory.
#' @param linecol color of the bounding box line.
#' @param linewd width of the bounding box line.
#' @param fontcol color of the box numbers.
#' @param fontsize size of the box numbers.
#' @return no return value, called for side effects.
#'
#' @details Not vectorized, but documents can be multi-page.
#'
#' @export
#' @examples
#' \dontrun{
#' resp <- dai_sync("page.pdf")
#' draw_paragraphs(type = "sync",
#'                 output = resp)
#'
#' resp <- dai_sync_tab("page.pdf")
#' draw_paragraphs(type="sync-tab",
#'                 output = resp,
#'                 doc = "page.pdf")
#'
#' draw_paragraphs(type = "async",
#'                 output = "page.json")
#'
#' draw_paragraphs(type = "async-tab",
#'                 output = "page.json",
#'                 doc = "page.pdf")
#' }


draw_paragraphs <- function(type,
														output,
														doc = NA,
														prefix = NULL,
														dir = getwd(),
														linecol = "red",
														linewd = 3,
														fontcol = "blue",
														fontsize = 4
) {

	# checks
	if (!(length(type) == 1) || !(type %in% c("sync", "async", "sync-tab", "async-tab"))) {
		stop("Invalid type parameter.")
	}

	if (!(inherits(output, "response") || is_json(output))) {
		stop("Invalid output parameter.")
	}

	if (length(doc) > 1 || is.numeric(doc)) {
		stop("Invalid doc parameter.")
	}

	if (length(prefix) > 1 || is.numeric(prefix)) {
		stop("Invalid prefix parameter.")
	}

	if (length(dir) > 1 || !(is.character(dir))) {
		stop("Invalid dir parameter. Must be a valid folder path.")
	}

	if (!(length(linecol) == 1) || !(daiR::is_colour(linecol)) || is.na(linecol)) {
		stop("Invalid linecol parameter. Must be a single valid colour representation.")
	}

	if (!(is.numeric(linewd) && length(linewd) == 1)) {
		stop("Invalid linewd parameter. Must be a single number.")
	}

	if (!(length(fontcol) == 1) || !(daiR::is_colour(fontcol)) || is.na(fontcol)) {
		stop("Invalid fontcol parameter. Must be a single valid colour representation.")
	}

	if (!(is.numeric(fontsize) && length(fontsize) == 1)) {
		stop("Invalid fontsize parameter. Must be a single number.")
	}

	# Helper functions
	get_vertices <- function(lst) {
		boxes <- purrr::map(lst, ~.x$layout$boundingPoly$normalizedVertices)
		return(boxes)
	}

	transpose_block <- function(lst) {
        xs <- c(lst[[1]][["x"]], lst[[2]][["x"]], lst[[3]][["x"]], lst[[4]][["x"]])
        ys <- c(lst[[1]][["y"]], lst[[2]][["y"]], lst[[3]][["y"]], lst[[4]][["y"]])
        df <- data.frame(xs, ys)
        return(df)
	}

	transpose_page <- function(x) {
		paragraphs <- purrr::map(x, transpose_block)
		return(paragraphs)
	}

	if (type == "sync") {

		if (!(inherits(output, "response"))) {
			stop("Output parameter not pointing to valid response object.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- httr::content(output)
		pages <- parsed$document$pages
		pages_paragraphs <- purrr::map(pages, ~.x$paragraphs)
		pagewise_block_sets <- purrr::map(pages_paragraphs, get_vertices)
		pagewise_block_sets <- purrr::map(pagewise_block_sets, transpose_page)

		# decode base64 and save to temp images
		page_imgs_base64 <- unlist(purrr::map(pages, ~.x$image$content))
		imgs <- character()
		for (i in seq_along(page_imgs_base64)) {
			path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
			outconn <- file(path, "wb")
			base64enc::base64decode(page_imgs_base64[i], outconn)
			close(outconn)
			imgs <- c(imgs, path)
		}

	} else if (type == "async") {

		if (!(is_json(output))) {
			stop("Output parameter not pointing to valid JSON file.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- jsonlite::fromJSON(output)
		pages_paragraphs <- parsed$pages$paragraphs
		pagewise_block_sets <- purrr::map(pages_paragraphs, ~.x$layout$boundingPoly$normalizedVertices)

		# decode base64 and save to temp images
		page_imgs_base64 <- parsed$pages$image$content
		imgs <- character()
		for (i in seq_along(page_imgs_base64)) {
			path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
			outconn <- file(path, "wb")
			base64enc::base64decode(page_imgs_base64[i], outconn)
			close(outconn)
			imgs <- c(imgs, path)
		}

	} else if (type == "sync-tab") {

		if (!(inherits(output, "response"))) {
			stop("Output parameter not pointing to valid response object.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- httr::content(output)
		pages <- parsed$pages
		pages_paragraphs <- purrr::map(pages, ~.x$paragraphs)
		pagewise_block_sets <- purrr::map(pages_paragraphs, get_vertices)
		pagewise_block_sets <- purrr::map(pagewise_block_sets, transpose_page)

		# Get vector of images from source doc
		if (grepl("pdf$", doc)) {
			pgs <- magick::image_read_pdf(doc)
			imgs <- character()
			for (i in seq_along(pgs)) {
				path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
				magick::image_write(pgs[i], path)
				imgs <- c(imgs, path)
			}
		} else {
			imgs <- doc
		}

	} else if (type == "async-tab") {

		if (!(is_json(output))) {
			stop("Output parameter not pointing to valid JSON file.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- jsonlite::fromJSON(output)
		pages_paragraphs <- parsed$pages$paragraphs
		pagewise_block_sets <- purrr::map(pages_paragraphs, ~.x$layout$boundingPoly$normalizedVertices)

		# Get vector of images from source doc
		if (grepl("pdf$", doc)) {
			pgs <- magick::image_read_pdf(doc)
			imgs <- character()
			for (i in seq_along(pgs)) {
				path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
				magick::image_write(pgs[i], path)
				imgs <- c(imgs, path)
			}
		} else {
			imgs <- doc
		}

	}

	# loop over the pagewise sets
	for (i in seq_along(pagewise_block_sets)) {

		img <- magick::image_read(imgs[i])

		# get image dimensions
		info <- magick::image_info(img)

		# prepare for plotting on image
		canvas <- magick::image_draw(img)

		# set counter for box number
		counter <- 1

		#loop over boxes on the page
		for (box in pagewise_block_sets[[i]]) {

			# handle NAs in boxes on top or left edge
			if (is.na(box$y[1])) box$y[1] <- 0
			if (is.na(box$y[2])) box$y[2] <- 0
			if (is.na(box$x[1])) box$x[1] <- 0
			if (is.na(box$x[4])) box$x[4] <- 0

			# transform from relative to absolute coordinates
			box$x1 <- box$x * info$width

			box$y1 <- box$y * info$height

			# draw polygon
			graphics::polygon(x = box$x1,
												y = box$y1,
												border = linecol,
												lwd = linewd
			)

			graphics::text(x = box$x1[1],
										 y = box$y1[1],
										 label = counter,
										 col = fontcol,
										 cex = fontsize,
										 family = "Liberation Sans"
			)

			counter <- counter + 1

		}

		# write annotated image to file

		if (type %in% c("async", "async-tab")) {
			default_prefix <- substr(basename(output), 1, nchar(basename(output)) - 5)
		} else {
			default_prefix <- "document"
		}

		if (is.null(prefix)) {
			filename <- glue::glue("{default_prefix}_page{i}_paragraphs.png")
		} else {
			filename <- glue::glue("{prefix}_page{i}_paragraphs.png")
		}

		dest <- file.path(dir, filename)

		magick::image_write(canvas, format = "png", dest)

		grDevices::dev.off()

	}

	pages <- length(pages_paragraphs)

	message(glue::glue("Generated {pages} annotated image(s)."))

}

#' Inspect line bounding boxes
#'
#' @description Plots the line bounding boxes identified by
#' Document AI (DAI) onto images of the submitted document.
#' Generates an annotated .png file for each page in the original
#' document.
#'
#' @param type one of "sync", "async", "sync-tab" or "async-tab", depending on
#' the function used to process the document.
#' @param output either a HTTP response object (from `dai_sync()` or
#' `dai_sync_tab()`) or the path to a JSON file (from `dai_async` or
#' `dai_async_tab()`).
#' @param doc filepath to the source document (pdf, tiff, or gif file); only
#' necessary for documents processed with `dai_sync_tab()` or `dai_async_tab()`.
#' @param prefix string to be prepended to the output png filename.
#' @param dir path to the desired output directory.
#' @param linecol color of the bounding box line.
#' @param linewd width of the bounding box line.
#' @param fontcol color of the box numbers.
#' @param fontsize size of the box numbers.
#' @return no return value, called for side effects.
#'
#' @details Not vectorized, but documents can be multi-page.
#'
#' @export
#' @examples
#' \dontrun{
#' resp <- dai_sync("page.pdf")
#' draw_lines(type = "sync",
#'            output = resp)
#'
#' resp <- dai_sync_tab("page.pdf")
#' draw_lines(type = "sync-tab",
#'            output = resp,
#'            doc = "page.pdf")
#'
#' draw_lines(type = "async",
#'            output = "page.json")
#'
#' draw_lines(type = "async-tab",
#'            output = "page.json",
#'            doc = "page.pdf")
#' }

draw_lines <- function(type,
											 output,
											 doc = NA,
											 prefix = NULL,
											 dir = getwd(),
											 linecol = "red",
											 linewd = 3,
											 fontcol = "blue",
											 fontsize = 4
) {

	# checks
	if (!(length(type) == 1) || !(type %in% c("sync", "async", "sync-tab", "async-tab"))) {
		stop("Invalid type parameter.")
	}

	if (!(inherits(output, "response") || is_json(output))) {
		stop("Invalid output parameter.")
	}

	if (length(doc) > 1 || is.numeric(doc)) {
		stop("Invalid doc parameter.")
	}

	if (length(prefix) > 1 || is.numeric(prefix)) {
		stop("Invalid prefix parameter.")
	}

	if (length(dir) > 1 || !(is.character(dir))) {
		stop("Invalid dir parameter. Must be a valid folder path.")
	}

	if (!(length(linecol) == 1) || !(daiR::is_colour(linecol)) || is.na(linecol)) {
		stop("Invalid linecol parameter. Must be a single valid colour representation.")
	}

	if (!(is.numeric(linewd) && length(linewd) == 1)) {
		stop("Invalid linewd parameter. Must be a single number.")
	}

	if (!(length(fontcol) == 1) || !(daiR::is_colour(fontcol)) || is.na(fontcol)) {
		stop("Invalid fontcol parameter. Must be a single valid colour representation.")
	}

	if (!(is.numeric(fontsize) && length(fontsize) == 1)) {
		stop("Invalid fontsize parameter. Must be a single number.")
	}

	# Helper functions
	get_vertices <- function(lst) {
		boxes <- purrr::map(lst, ~.x$layout$boundingPoly$normalizedVertices)
		return(boxes)
	}

	transpose_block <- function(lst) {
        xs <- c(lst[[1]][["x"]], lst[[2]][["x"]], lst[[3]][["x"]], lst[[4]][["x"]])
        ys <- c(lst[[1]][["y"]], lst[[2]][["y"]], lst[[3]][["y"]], lst[[4]][["y"]])
        df <- data.frame(xs, ys)
        return(df)
	}

	transpose_page <- function(x) {
		lines <- purrr::map(x, transpose_block)
		return(lines)
	}

	if (type == "sync") {

		if (!(inherits(output, "response"))) {
			stop("Output parameter not pointing to valid response object.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- httr::content(output)
		pages <- parsed$document$pages
		pages_lines <- purrr::map(pages, ~.x$lines)
		pagewise_block_sets <- purrr::map(pages_lines, get_vertices)
		pagewise_block_sets <- purrr::map(pagewise_block_sets, transpose_page)

		# decode base64 and save to temp images
		page_imgs_base64 <- unlist(purrr::map(pages, ~.x$image$content))
		imgs <- character()
		for (i in seq_along(page_imgs_base64)) {
			path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
			outconn <- file(path, "wb")
			base64enc::base64decode(page_imgs_base64[i], outconn)
			close(outconn)
			imgs <- c(imgs, path)
		}

	} else if (type == "async") {

		if (!(is_json(output))) {
			stop("Output parameter not pointing to valid JSON file.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- jsonlite::fromJSON(output)
		pages_lines <- parsed$pages$lines
		pagewise_block_sets <- purrr::map(pages_lines, ~.x$layout$boundingPoly$normalizedVertices)

		# decode base64 and save to temp images
		page_imgs_base64 <- parsed$pages$image$content
		imgs <- character()
		for (i in seq_along(page_imgs_base64)) {
			path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
			outconn <- file(path, "wb")
			base64enc::base64decode(page_imgs_base64[i], outconn)
			close(outconn)
			imgs <- c(imgs, path)
		}

	} else if (type == "sync-tab") {

		if (!(inherits(output, "response"))) {
			stop("Output parameter not pointing to valid response object.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- httr::content(output)
		pages <- parsed$pages
		pages_lines <- purrr::map(pages, ~.x$lines)
		pagewise_block_sets <- purrr::map(pages_lines, get_vertices)
		pagewise_block_sets <- purrr::map(pagewise_block_sets, transpose_page)

		# Get vector of images from source doc
		if (grepl("pdf$", doc)) {
			pgs <- magick::image_read_pdf(doc)
			imgs <- character()
			for (i in seq_along(pgs)) {
				path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
				magick::image_write(pgs[i], path)
				imgs <- c(imgs, path)
			}
		} else {
			imgs <- doc
		}

	} else if (type == "async-tab") {

		if (!(is_json(output))) {
			stop("Output parameter not pointing to valid JSON file.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- jsonlite::fromJSON(output)
		pages_lines <- parsed$pages$lines
		pagewise_block_sets <- purrr::map(pages_lines, ~.x$layout$boundingPoly$normalizedVertices)

		# Get vector of images from source doc
		if (grepl("pdf$", doc)) {
			pgs <- magick::image_read_pdf(doc)
			imgs <- character()
			for (i in seq_along(pgs)) {
				path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
				magick::image_write(pgs[i], path)
				imgs <- c(imgs, path)
			}
		} else {
			imgs <- doc
		}

	}

	# loop over the pagewise sets
	for (i in seq_along(pagewise_block_sets)) {

		img <- magick::image_read(imgs[i])

		# get image dimensions
		info <- magick::image_info(img)

		# prepare for plotting on image
		canvas <- magick::image_draw(img)

		# set counter for box number
		counter <- 1

		#loop over boxes on the page
		for (box in pagewise_block_sets[[i]]) {

			# handle NAs in boxes on top or left edge
			if (is.na(box$y[1])) box$y[1] <- 0
			if (is.na(box$y[2])) box$y[2] <- 0
			if (is.na(box$x[1])) box$x[1] <- 0
			if (is.na(box$x[4])) box$x[4] <- 0

			# transform from relative to absolute coordinates
			box$x1 <- box$x * info$width

			box$y1 <- box$y * info$height

			# draw polygon
			graphics::polygon(x = box$x1,
												y = box$y1,
												border = linecol,
												lwd = linewd
			)

			graphics::text(x = box$x1[1],
										 y = box$y1[1],
										 label = counter,
										 col = fontcol,
										 cex = fontsize,
										 family = "Liberation Sans"
			)

			counter <- counter + 1

		}

		# write annotated image to file

		if (type %in% c("async", "async-tab")) {
			default_prefix <- substr(basename(output), 1, nchar(basename(output)) - 5)
		} else {
			default_prefix <- "document"
		}

		if (is.null(prefix)) {
			filename <- glue::glue("{default_prefix}_page{i}_lines.png")
		} else {
			filename <- glue::glue("{prefix}_page{i}_lines.png")
		}

		dest <- file.path(dir, filename)

		magick::image_write(canvas, format = "png", dest)

		grDevices::dev.off()

	}

	pages <- length(pages_lines)

	message(glue::glue("Generated {pages} annotated image(s)."))

}

#' Inspect token bounding boxes
#'
#' @description Plots the token (i.e., word) bounding boxes identified
#' by Document AI (DAI) onto images of the submitted document.
#' Generates an annotated .png file for each page in the original
#' document.
#'
#' @param type one of "sync", "async", "sync-tab" or "async-tab", depending on
#' the function used to process the document.
#' @param output either a HTTP response object (from `dai_sync()` or
#' `dai_sync_tab()`) or the path to a JSON file (from `dai_async` or
#' `dai_async_tab()`).
#' @param doc filepath to the source document (pdf, tiff, or gif file); only
#' necessary for documents processed with `dai_sync_tab()` or `dai_async_tab()`.
#' @param prefix string to be prepended to the output png filename.
#' @param dir path to the desired output directory.
#' @param linecol color of the bounding box line.
#' @param linewd width of the bounding box line.
#' @param fontcol color of the box numbers.
#' @param fontsize size of the box numbers.
#' @return no return value, called for side effects.
#'
#' @details Not vectorized, but documents can be multi-page.
#'
#' @export
#' @examples
#' \dontrun{
#' resp <- dai_sync("page.pdf")
#' draw_tokens(type = "sync",
#'             output = resp)
#'
#' resp <- dai_sync_tab("page.pdf")
#' draw_tokens(type = "sync-tab",
#'             output = resp,
#'             doc = "page.pdf")
#'
#' draw_tokens(type = "async",
#'             output = "page.json")
#'
#' draw_tokens(type = "async-tab",
#'             output = "page.json",
#'             doc = "page.pdf")
#' }

draw_tokens <- function(type,
						output,
						doc = NA,
						prefix = NULL,
						dir = getwd(),
						linecol = "red",
						linewd = 3,
    					fontcol = "blue",
						fontsize = 4
) {

	# checks
	if (!(length(type) == 1) || !(type %in% c("sync", "async", "sync-tab", "async-tab"))) {
		stop("Invalid type parameter.")
	}

	if (!(inherits(output, "response") || is_json(output))) {
		stop("Invalid output parameter.")
	}

	if (length(doc) > 1 || is.numeric(doc)) {
		stop("Invalid doc parameter.")
	}

	if (length(prefix) > 1 || is.numeric(prefix)) {
		stop("Invalid prefix parameter.")
	}

	if (length(dir) > 1 || !(is.character(dir))) {
		stop("Invalid dir parameter. Must be a valid folder path.")
	}

	if (!(length(linecol) == 1) || !(daiR::is_colour(linecol)) || is.na(linecol)) {
		stop("Invalid linecol parameter. Must be a single valid colour representation.")
	}

	if (!(is.numeric(linewd) && length(linewd) == 1)) {
		stop("Invalid linewd parameter. Must be a single number.")
	}

	if (!(length(fontcol) == 1) || !(daiR::is_colour(fontcol)) || is.na(fontcol)) {
		stop("Invalid fontcol parameter. Must be a single valid colour representation.")
	}

	if (!(is.numeric(fontsize) && length(fontsize) == 1)) {
		stop("Invalid fontsize parameter. Must be a single number.")
	}

	# Helper functions
	get_vertices <- function(lst) {
		boxes <- purrr::map(lst, ~.x$layout$boundingPoly$normalizedVertices)
		return(boxes)
	}

	transpose_block <- function(lst) {
        xs <- c(lst[[1]][["x"]], lst[[2]][["x"]], lst[[3]][["x"]], lst[[4]][["x"]])
        ys <- c(lst[[1]][["y"]], lst[[2]][["y"]], lst[[3]][["y"]], lst[[4]][["y"]])
        df <- data.frame(xs, ys)
        return(df)
	}

	transpose_page <- function(x) {
		tokens <- purrr::map(x, transpose_block)
		return(tokens)
	}

	if (type == "sync") {

		if (!(inherits(output, "response"))) {
			stop("Output parameter not pointing to valid response object.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- httr::content(output)
		pages <- parsed$document$pages
		pages_tokens <- purrr::map(pages, ~.x$tokens)
		pagewise_block_sets <- purrr::map(pages_tokens, get_vertices)
		pagewise_block_sets <- purrr::map(pagewise_block_sets, transpose_page)

		# decode base64 and save to temp images
		page_imgs_base64 <- unlist(purrr::map(pages, ~.x$image$content))
		imgs <- character()
		for (i in seq_along(page_imgs_base64)) {
			path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
			outconn <- file(path, "wb")
			base64enc::base64decode(page_imgs_base64[i], outconn)
			close(outconn)
			imgs <- c(imgs, path)
		}

	} else if (type == "async") {

		if (!(is_json(output))) {
			stop("Output parameter not pointing to valid JSON file.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- jsonlite::fromJSON(output)
		pages_tokens <- parsed$pages$tokens
		pagewise_block_sets <- purrr::map(pages_tokens, ~.x$layout$boundingPoly$normalizedVertices)

		# decode base64 and save to temp images
		page_imgs_base64 <- parsed$pages$image$content
		imgs <- character()
		for (i in seq_along(page_imgs_base64)) {
			path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
			outconn <- file(path, "wb")
			base64enc::base64decode(page_imgs_base64[i], outconn)
			close(outconn)
			imgs <- c(imgs, path)
		}

	} else if (type == "sync-tab") {

		if (!(inherits(output, "response"))) {
			stop("Output parameter not pointing to valid response object.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- httr::content(output)
		pages <- parsed$pages
		pages_tokens <- purrr::map(pages, ~.x$tokens)
		pagewise_block_sets <- purrr::map(pages_tokens, get_vertices)
		pagewise_block_sets <- purrr::map(pagewise_block_sets, transpose_page)

		# Get vector of images from source doc
		if (grepl("pdf$", doc)) {
			pgs <- magick::image_read_pdf(doc)
			imgs <- character()
			for (i in seq_along(pgs)) {
				path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
				magick::image_write(pgs[i], path)
				imgs <- c(imgs, path)
			}
		} else {
			imgs <- doc
		}

	} else if (type == "async-tab") {

		if (!(is_json(output))) {
			stop("Output parameter not pointing to valid JSON file.")
		}

		# extract a list with pagewise sets of block boundary boxes
		parsed <- jsonlite::fromJSON(output)
		pages_tokens <- parsed$pages$tokens
		pagewise_block_sets <- purrr::map(pages_tokens, ~.x$layout$boundingPoly$normalizedVertices)

		# Get vector of images from source doc
		if (grepl("pdf$", doc)) {
			pgs <- magick::image_read_pdf(doc)
			imgs <- character()
			for (i in seq_along(pgs)) {
				path <- file.path(tempdir(), glue::glue("page{i}.jpg"))
				magick::image_write(pgs[i], path)
				imgs <- c(imgs, path)
			}
		} else {
			imgs <- doc
		}

	}

	# loop over the pagewise sets
	for (i in seq_along(pagewise_block_sets)) {

		img <- magick::image_read(imgs[i])

		# get image dimensions
		info <- magick::image_info(img)

		# prepare for plotting on image
		canvas <- magick::image_draw(img)

		# set counter for box number
		counter <- 1

		#loop over boxes on the page
		for (box in pagewise_block_sets[[i]]) {

			# handle NAs in boxes on top or left edge
			if (is.na(box$y[1])) box$y[1] <- 0
			if (is.na(box$y[2])) box$y[2] <- 0
			if (is.na(box$x[1])) box$x[1] <- 0
			if (is.na(box$x[4])) box$x[4] <- 0

			# transform from relative to absolute coordinates
			box$x1 <- box$x * info$width

			box$y1 <- box$y * info$height

			# draw polygon
			graphics::polygon(x = box$x1,
							  y = box$y1,
							  border = linecol,
							  lwd = linewd
			)

			graphics::text(x = box$x1[1],
						   y = box$y1[1],
						   label = counter,
						   col = fontcol,
						   cex = fontsize,
						   family = "Liberation Sans"
			)

			counter <- counter + 1

		}

		# write annotated image to file

		if (type %in% c("async", "async-tab")) {
			default_prefix <- substr(basename(output), 1, nchar(basename(output)) - 5)
		} else {
			default_prefix <- "document"
		}

		if (is.null(prefix)) {
			filename <- glue::glue("{default_prefix}_page{i}_tokens.png")
		} else {
			filename <- glue::glue("{prefix}_page{i}_tokens.png")
		}

		dest <- file.path(dir, filename)

		magick::image_write(canvas, format = "png", dest)

		grDevices::dev.off()

	}

	pages <- length(pages_tokens)

	message(glue::glue("Generated {pages} annotated image(s)."))

}
