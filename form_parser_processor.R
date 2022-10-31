download.file("https://archive.org/download/tobacco_lpnn0000/lpnn0000.pdf", 
							destfile = "tobacco.pdf",
							mode = "wb")

form_parser <- "df638427f47dedae"
resp <- dai_sync("tobacco.pdf", proc_id = form_parser) # simply change the processor
parsed_form <- content(resp)
# Yes, the form parser identifies tables.

resp_regular <- dai_sync("tobacco.pdf")
parsed_regular <- content(resp_regular) # no tables

resp_v1beta3 <- dai_sync3("tobacco.pdf")
parsed_v1beta3 <- content(resp_v1beta3) # no tables

resp_v1beta2 <- dai_sync_tab("tobacco.pdf")
parsed_v1beta2 <- content(resp_v1beta2) # tables, but no image

tabs <- tables_from_dai_response(resp_v1beta2)

##
txt <- text_from_dai_response(resp)
cat(txt)

draw_blocks(type="sync", output=resp)
#works

draw_blocks(type="sync-tab", output=resp_v1beta2, doc = "tobacco.pdf")
# not very different

####################################################################
# get tables from form parser
# OK

object <- resp
object <- resp_v1beta2


tables_from_dai_response2 <- function(object) {
	
	# checks
	if (!(inherits(object, "response"))) {
		stop("Object is not a valid HTTP response.")
	}
	
	parsed <- httr::content(object, as="parsed")
	
	if (!("pages" %in% names(parsed) || "pages" %in% names(parsed$document))) {
		stop("Object not a positive dai_sync response.")
	}
	
	if (!("text" %in% names(parsed) || "text" %in% names(parsed$document))) {
		stop("DAI found no text. Was the page blank?")
	}
	
	# Compile a list of table entries
	
	if ("pages" %in% names(parsed$document)) {
		table_list_raw <- purrr::map(parsed$document$pages, ~ .x$tables)
	} else {
		table_list_raw <- purrr::map(parsed$pages, ~ .x$tables)
	}
	
	if (all(sapply(table_list_raw, is.null))) {
		stop("DAI found no tables in the document.")
	}
	
	table_list <- purrr::flatten(table_list_raw)
	
	# Function to get the text of an individual cell
	resp_get_cell_text <- function(cell) {
		anchors <- cell$layout$textAnchor
		if (length(anchors) == 0) {
			txt <- ""
		} else {
			indices <- cell$layout$textAnchor$textSegments
			txt <- character()
			for (i in indices) {
				if (is.null(i$startIndex)){
					line_start <- 1
				} else {
					line_start <- i$startIndex
				}
				line_end <- i$endIndex
				line_txt <- substr(text, line_start, line_end)
				txt <- paste(txt, line_txt, sep = "\n")
			}
		}
		return(txt)
		
	}
	
	# Function to compile cell entries into a row vector
	resp_get_row_vector <- function(elem) {
		cells <- elem$cells
		vector <- unlist(purrr::map(cells, resp_get_cell_text))
		return(vector)
		
	}
	
	# Function to build a table from row vectors
	resp_build_table <- function(table) {
		headers_list <- table$headerRows
		rows_list <- table$bodyRows
		headervectors <- purrr::map(headers_list, resp_get_row_vector)
		rowvectors <- purrr::map(rows_list, resp_get_row_vector)
		table <- data.frame(matrix(nrow= 0, ncol=6))
		for (i in rowvectors) {
			table <- rbind(table, as.data.frame(t(i)))
		}
		table <- stats::setNames(table, headervectors[[1]])
		return(table)
		
	}
	
	# Get reference text for indices
	
	if ("text" %in% names(parsed$document)) {
		
		text <- parsed$document$text
		
	} else {
		
		text <- parsed$text
		
	}
	
	# Build all tables
	all_tables <- purrr::map(table_list, resp_build_table)
	
	return(all_tables)
	
}

tables <- tables_from_dai_response(resp_v1beta2)
tables_from_dai_response2(resp_v1beta2)
# this works

tables_from_dai_response2(resp)
# not this


resp_build_table(table_list[[1]])
resp_build_table(table_list[[2]])
resp_build_table(table_list[[3]])
resp_build_table(table_list[[4]])

table <- table_list[[1]]

headers_list <- table$headerRows
rows_list <- table$bodyRows
headervectors <- purrr::map(headers_list, resp_get_row_vector)
rowvectors <- purrr::map(rows_list, resp_get_row_vector)
table <- data.frame(matrix(nrow= 0, ncol=6))
for (i in rowvectors) {
	table <- rbind(table, as.data.frame(t(i)))
}
table <- stats::setNames(table, headervectors[[1]])
return(table)



###########

df <- get_processors()

dai_async("tobacco.pdf", proc_id = "df638427f47dedae")
contents <- gcs_list_objects()
jsons <- grep("json$", contents$name, value = TRUE)
gcs_get_object(jsons, saveToDisk = "tobacco_form_parser.json")

file <- "tobacco_form_parser.json"

tables_from_dai_file("tobacco_form_parser.json")
# works, no need to do anything.
