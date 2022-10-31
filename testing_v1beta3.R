######################################3

#v1beta3


dai_sync3 <- function(file,
											proj_id = get_project_id(),
											proc_id = Sys.getenv("DAI_PROCESSOR_ID"),
											proc_v = NA,
											skip_rev = "true",
											loc = "eu",
											token = dai_token()
) {
	
	# Check inputs
	if (!(is.character(file) && length(file) == 1)) {
		stop("Invalid file input.")
	}
	
	extension <- tolower(stringr::str_extract(file, "(?<=\\.)\\w{3,4}$"))
	supported <- c("bmp", "gif", "jpeg", "jpg", "pdf", "png", "tiff")
	
	if (!(extension %in% supported)) {
		stop("Unsupported file format. See documentation for details.")
	}
	
	if (extension == "pdf" && !(is_pdf(file))) {
		stop("Input file not a real pdf. Is the file in your working directory?")
	}
	
	if (!(is.character(proj_id) && length(proj_id) == 1)) {
		stop("Invalid proj_id.")
	}
	
	if (!(is.character(proc_id) && length(proc_id) == 1) || proc_id == "") {
		stop("Invalid proc_id.")
	}
	
	if (!(length(proc_v) == 1)) {
		stop("Invalid proc_v.")
	}
	
	if (!(is.na(proc_v) || is.character(proc_v))) {
		stop("Invalid proc_v.")
	}
	
	skip_rev <- tolower(skip_rev)
	
	if (!(skip_rev %in% c("true", "false") && length(skip_rev) == 1)) {
		stop("Invalid skip_rev parameter.")
	}
	
	loc <- tolower(loc)
	
	if (!(loc %in% c("eu", "us"))) {
		stop("Invalid location parameter.")
	}
	
	# Encode
	if (extension == "pdf"){
		encoded_file <- pdf_to_binbase(file)
	} else {
		encoded_file <- img_to_binbase(file)
	}
	
	## Create json request body
	req <- list("skipHumanReview" = skip_rev,
							"rawDocument" = list("content" = encoded_file,
																	 "mimeType" = "image/tiff"
							)
	)
	
	bod <- jsonlite::toJSON(req, auto_unbox = TRUE)
	
	## Build URL and submit API request
	
	base_url <- glue::glue("https://{loc}-documentai.googleapis.com/")
	
	path <- glue::glue("v1beta3/projects/{proj_id}/locations/{loc}/processors/{proc_id}")
	
	if (is.na(proc_v)) {
		version <- ""	
	} else {
		version <- glue::glue("/processorVersions/{proc_v}")
	}
	
	method <- ":process"
	
	url <- glue::glue("{base_url}{path}{version}{method}")
	
	response <- httr::POST(url,
												 httr::config(token = token),
												 body = bod
	)
	
	if (response$status_code == 200) {
		message(glue::glue("File submitted at {response$date}. HTTP status: 200 - OK."))
	} else {
		parsed <- httr::content(response)
		message(glue::glue('File submitted at {response$date}. HTTP status: {response$status_code} - unsuccessful.\nError: "{parsed$error$message}"'))
	}
	
	return(response)
	
}

resp_sync_v1 <- dai_sync("saratan.pdf")
resp_sync_v1beta2 <- dai_sync_tab("saratan.pdf")
resp_sync_v1beta3 <- dai_sync3("saratan.pdf")

parsed1 <- content(resp_sync_v1)
parsed2 <- content(resp_sync_v1beta2)
parsed3 <- content(resp_sync_v1beta3)

# I can't see any difference between v1 and v1beta3 in the type of information contained. 
# 

# document quality processor
# https://cloud.google.com/document-ai/docs/processors-list?hl=en-GB#processor_doc-quality-processor
# NOT AVAILABLE yet

resp <- dai_sync("saratan.pdf", proc_v = "pretrained-document-quality-v1.0-2021-01-20")

#########