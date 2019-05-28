#' Download CTPP Data
#'
#' This function allows you to download CTPP tables into R
#' @param id The identifier of the CTPP table you want to download (e.g. "A101100"). Type \link[CTPPr]{ctpp_tables}() for a list of tables and their ids.
#' @param geography The geography level of the requested table. Type ctpp_geography_list to see available geographies, or see Details below.
#' @param state The state your request should subset to. Takes name ("Alabama"), abbreviation ("AL") or FIPS ("01"). Type ctpp_state_list to see available states.
#' @param output How you want your records labelled. Default is "Name" unless geography is "Tract", "TAD", or "TAZ", in which case "FIPS Code" is used to reduce size of request. See Details below for example outputs.
#' @param dataset "2010" or "2016" to specify which 5-year dataset to query.
#' @export
#' @import httr
#' @import XML
#' @import data.table
#' @return returns a data.table based on a default option CSV download request (tables available at the \href{http://data5.ctpp.transportation.org/ctpp/Browse/BrowseTables.aspx}{online CTPP browser})
#' @section Details:
#'
#' This function makes a bulk download request for the provided table id. Use this function with caution and subset to a specific state for smaller geography requests. See the example section for some recommendations on downloading and storing your results.
#'
#' \subsection{geography}{
#' {Bold words below are accepted by geography=}
#' {For Residence and Workplace Tables:}
#'   \itemize{
#'     \item \strong{County} - State-County
#'     \item \strong{MCD} - State-County-MCD (for 12 strong MCD states)
#'     \item \strong{Place} - State-Place
#'     \item \strong{MSA} - Metropolitan Statistical Area
#'     \item \strong{City} - Metropolitan Statistical Area - EACH Principal City
#'     \item \strong{PUMA} - State-PUMA5
#'     \item \strong{UA} - Urbanized Area (UA)
#'     \item \strong{OUSA} - Worked Outside United States (WORKPLACE tables only)
#'     \item \strong{Tract} - State-County-Tract
#'     \item \strong{TAD} - TAD
#'     \item \strong{TAZ} - TAZ
#'   }
#' {For Flow (Residence->Workplace) Tables:}
#'   \itemize{
#'     \item \strong{State->State}
#'     \item \strong{County->County}
#'     \item \strong{MCD->MCD}
#'     \item \strong{Place->Place}
#'     \item \strong{City->City}
#'     \item \strong{PUMA->PUMA}
#'     \item \strong{County->Place}
#'     \item \strong{MCD->Place}
#'     \item \strong{PUMA->Place}
#'     \item \strong{Tract->Tract}
#'     \item \strong{TAD->TAD}
#'     \item \strong{TAD->TAZ}
#'     \item \strong{TAZ->TAZ}
#'     \item \strong{TAZ->TAD}
#'     \item \strong{Place->TAZ}
#'     \item \strong{TAZ->Place}
#'   }
#' }
#' \subsection{state}{
#' {Use state= in combination with geography=. If you don't, the requested table will be downloaded for the entire country and may not finish.}
#' }
#' \subsection{output}{
#' {\strong{Option} - Example}
#'   \itemize{
#'     \item \strong{Name} - "New York"
#'     \item \strong{Key} - "C0300US13001"
#'     \item \strong{FIPS Code} - "36"
#'     \item \strong{FIPS and Name} - "36 | New York"
#'     \item \strong{Split FIPS Code} - "36 | 001"
#'   }
#' }
#' @examples
#' \donttest{
#' A102102 <- download_ctpp(
#'   id = "A102102",
#'   geography = "County",
#'   state = "GA"
#' )
#' A202100 <- download_ctpp(
#'   id = "A202100",
#'   geography = "MSA",
#'   state = "FL"
#' )
#' # save your tables for working with later
#' save(A102102, A202100, "CTPP-tables.RData")
#' # load tables back
#' load("CTPP-tables.RData")
#' }


download_ctpp <- function(id, geography="State", state="", dataset="", output="Name") {

	debug_request <- FALSE # development only, set globally

	# dataset ----
	dataset_parameter <- dataset
	dataset_id <- gsub(" ", "", dataset)
	dataset_id <- ifelse(nchar(dataset_id)==2,paste0("20",dataset_id),dataset_id)
	if (grepl("-",dataset_id)) {
		year1 <- strsplit(dataset_id,"-")[[1]][1]
		year2 <- strsplit(dataset_id,"-")[[1]][2]
		year1 <- ifelse(nchar(year1)==2,paste0("20",year1),year1)
		year2 <- ifelse(nchar(year2)==2,paste0("20",year2),year2)
		dataset_id <- paste0(year1,"-",year2)
		dataset_id <- ctpp_dataset_list[ctpp_dataset_list$Dataset==dataset_id, "Short Label"]
	}

	report_id <- as.character(substitute(id))
	report_id_parameter <- report_id
	report_id <- toupper(report_id)

	if(!output %in% c('Name','Key','FIPS Code','FIPS and Name','Split FIPS Code')) {
		stop("\n'output' parameter must be one of: \n'Name', 'Key', 'FIPS Code', 'FIPS and Name', 'Split FIPS Code'")
	}

	# id ----
	# use dataset and id to look up the url components for http requests
	if(dataset_id %in% c("2010","2016") && !dataset_id %in% c(CTPPr::tables[name==report_id, dataset])) {
		stop("\nReport id ", report_id_parameter, " is not available for dataset='",dataset_id,"'. Type ctpp_tables() to view available tables by dataset.")
	}
	url_id <- CTPPr::tables[name==report_id, id]
	if(length(url_id)==0) {
		stop("\nReport id ", report_id_parameter, " does not exist. Type ctpp_tables() to view available tables.")
	} else if(length(url_id)==1) {
		if(!dataset_id %in% c("2010","2016")) { # allow user to not provide dataset by looking up dataset availability and defaulting to 2016 if multiple hits
			dataset_id <- CTPPr::tables[id==url_id, dataset]
		}
	} else if(length(url_id)>1) {
		if(dataset_id=="") {
			# cat("Report id ", report_id_parameter, " exists in multiple datasets and 'dataset' not specified. Using dataset='2016'..")
			dataset_id <- "2016"
		}
		url_id <- CTPPr::tables[name==report_id & dataset==dataset_id, id]
	}

	dataset_url_id <- ctpp_dataset_list[ctpp_dataset_list$`Short Label`==dataset_id, "URL ID"]

	# 1=RESIDENCE, 2=WORKPLACE, 3=FLOW
	report_types <- data.frame(
		"ID" = c("1","2","3"),
		"Name" = c("RESIDENCE","WORKPLACE","FLOW"),
		stringsAsFactors = F
	)
	table_base_name <- CTPPr::tables[dataset==dataset_id & id==url_id, name]
	table_base_name <- strsplit(table_base_name,"_")[[1]]
	table_base_name <- table_base_name[length(table_base_name)] # table_base_name <- unlist(lapply(table_base_name, FUN=function(x) { split <- strsplit(x,"_")[[1]]; return(split[length(split)]) }))
	report_type <- substr(table_base_name,2,2)
	if(length(report_type)==0) {
		stop("Report id ", report_id_parameter, " does not exist. Type ctpp_tables() to view available tables.")
	}

	if(report_type %in% c("1","2") & grepl("->",geography)) {
		stop("The FLOW report type geography you selected (\"", geography,"\") is incompatible with the CTPP table identifier you selected (\"", report_id_parameter, "\").")
	}
	report_type_name <- report_types[report_types$ID==report_type, "Name"]

	# geography ----
	geography_parameter <- geography
	# We can make State requests (the highest level for a table request since National does not really count) but not in the usual bulk method. So odd behavior where _BulkLevel form parameter gets set to the County geography code (03) for State requests. In addition _UseBulkSelection and _SetSelected need to be toggled for State requests
	if(toupper(geography) %in% c("","2","02","STATE")) {
		geography <- "03" # 03=County see note above
		geography_attribute_value <- "State"
	}
	geography <- ifelse(geography %in% c("3","4","5","6","7","8","9"), paste0("0",geography), geography)
	if(!geography %in% ctpp_geography_list$ID) {
		if(grepl("->",geography)) {
			geography <- gsub(" ","",geography)
		}
			geography <- ctpp_geography_list[toupper(ctpp_geography_list$`Short Label`)==toupper(geography), "ID"]
			geography_attribute_value <- ctpp_geography_list[ctpp_geography_list$ID==geography, "Short Label"]
	}
	if(!geography %in% ctpp_geography_list$ID) {
		stop("geography \"", geography_parameter, "\" does not exist. Valid inputs are:\n", paste(ctpp_geography_list$`Short Label`, collapse=","))
	}
	# Parse out RESIDENCE and WORKFLOW identifiers from geography for FLOW geography reports (RESIDENCE->WORKPLACE)
	if(report_type=="3") {
		if(!grepl("->",geography_parameter)) {
			geography <- "42" # paste0(geography,"->",geography)
			geography_attribute_value <- "State->State"
			# cat("Table is a FLOW type table but geography parameter does not specify a FLOW geography. Defaulting to geography=\"State->State\"")
		}
		geography_flow <- ctpp_geography_list[ctpp_geography_list$ID==geography, "Short Label"]
		geography_residence <- strsplit(geography_flow,"->")[[1]][1]
		geography_residence <- ctpp_geography_list[ctpp_geography_list$`Short Label`==geography_residence, "ID"]
		geography_workplace <- strsplit(geography_flow,"->")[[1]][2]
		geography_workplace <- ctpp_geography_list[ctpp_geography_list$`Short Label`==geography_workplace, "ID"]
		# WORKPLACE ID is +20 RESIDENCE ID.
		geography_workplace <- as.character(as.numeric(geography_workplace)+20)
	}
	# WORKPLACE ID is +20 RESIDENCE ID, except for "Urbanized Area" which is replace with "Worked Outside United States". This function does not support that table.
	if(report_type=="2") {
		if(geography == "10") {
			stop("Urbanized Area (UA) is not an available geography for the ", report_type_name, " table ", CTPPr::tables[dataset==dataset_id & id==url_id, name])
		}
		geography <- as.character(as.numeric(geography)+20)
	}

	# state ----
	if(state!="") {
		state <- gsub(" ","",state)
		# bulk selection allows selecting multiple states using comma separated numeric FIPS
		if(grepl(",",state)) {
			state <- strsplit(state,",")[[1]]
		}
		state <- sapply(state, FUN=function(x) {
			# allow state parameter to be name, or alpha FIPS, or numeric FIPS
			x <- ifelse(x %in% c("1","2","3","4","5","6","7","8","9"), paste0("0",x), x)
			if(!x %in% ctpp_state_list$FIPS) {
				if(nchar(x)==2) {
					x <- ctpp_state_list[toupper(ctpp_state_list$Abbreviation)==toupper(x), "FIPS"]
				} else {
					x <- ctpp_state_list[toupper(ctpp_state_list$Name)==toupper(x), "FIPS"]
				}
			}
			return(x)
		})
	state <- paste(state, collapse=",")
	}

	# URL prep ----
	base_report_url <- paste0("http://data5.ctpp.transportation.org/",dataset_url_id,"/View/dispview.aspx?ReportId=") # "http://data5.ctpp.transportation.org/ctpp/View/dispview.aspx?ReportId="
	report_url <- paste0(base_report_url, url_id)
	bulk_selection_url <- paste0("http://ctpp.beyond2020.com/",dataset_url_id,"/Dim/InsideGeoSelection.aspx?GeoType=", ifelse(report_type_name=="FLOW","RESIDENCE",report_type_name))
	browse_tables_url <- paste0("http://ctpp.beyond2020.com/",dataset_url_id,"/Browse/BrowseTables.aspx")
	report_csv_url <- paste0("http://data5.ctpp.transportation.org/",dataset_url_id,"/View/dispview.aspx?download=yes&DownloadFormat=CSV&CsvDataFormat=CSV_LF")

	# (1) Go to the bulk download page ----
	# Page is different for each report type and holds all the settings we need for bulk request. Some need editing. This GET may be skippable if the package makes a way to store the parameters for each report type.
	html <- httr::RETRY(times = 4, pause_cap = 16, quiet = TRUE,
		verb = "GET",
		url = bulk_selection_url
	)
	if(html$status_code != 200) {
		stop(
			"\nWeb service may be down (HTTP error request code = ", html$status_code, ").",
			"\nURL attempted (GET): ", bulk_selection_url,
			"\nYou may try again in a few seconds if you know the requested report table exists. View available tables with ctpp_tables()"
		)
	}
	request_body1 <- get_post_body_list(html, debug_request = debug_request)

	# (1b) Set parameters as the web app requires them for our request
	if(report_type %in% c("1","2")) {
		request_body1[names(request_body1)=="GeoType"] <- report_type_name
		request_body1[names(request_body1)==paste0(report_type_name,"_UseBulkSelection")] <- ifelse(state=="" && geography %in% c("03","23"),"0","1")
		request_body1[names(request_body1)==paste0(report_type_name,"_BulkLevel")] <- paste0("C",geography)
		request_body1[names(request_body1)==paste0(report_type_name,"_BulkStates")] <- state
		request_body1[names(request_body1)==paste0(report_type_name,"_SelectedLabel")] <- output
		request_body1[names(request_body1)==paste0(report_type_name,"_SetSelected")] <- ifelse(state=="" && geography %in% c("03","23"),"0","-1")
		request_body1[names(request_body1)=="ReportFolderId"] <- ""
		request_body1[names(request_body1)=="ReportCubeId"] <- NULL
		request_body1[names(request_body1)=="ReportId"] <- NULL
		request_body1[["BulkLabelSet"]] <- output
	} else if(report_type=="3") {
		if(geography=="42") { # State->State
			request_body1[names(request_body1)=="GeoType"] <- "RESIDENCE"
			request_body1[names(request_body1)=="FlowLevel"] <- paste0("C",geography)
			request_body1[["BulkLabelSet"]] <- output
		} else {
			request_body1[names(request_body1)=="GeoType"] <- "RESIDENCE"
			request_body1[names(request_body1)=="FlowLevel"] <- paste0("C",geography)
			request_body1[["BulkLabelSet"]] <- output
			request_body1[names(request_body1)=="RESIDENCE_UseBulkSelection"] <- ifelse(state=="","0","1")
			request_body1[names(request_body1)=="RESIDENCE_BulkLevel"] <- paste0("C",geography_residence)
			request_body1[names(request_body1)=="RESIDENCE_BulkStates"] <- state
			request_body1[names(request_body1)=="RESIDENCE_SelectedLabel"] <- output
			request_body1[names(request_body1)=="RESIDENCE_SetSelected"] <- ifelse(state=="","0","-1")
			request_body1[names(request_body1)=="WORKPLACE_UseBulkSelection"] <- ifelse(state=="","0","1")
			request_body1[names(request_body1)=="WORKPLACE_BulkLevel"] <- paste0("C",geography_workplace)
			request_body1[names(request_body1)=="WORKPLACE_BulkStates"] <- state
			request_body1[names(request_body1)=="WORKPLACE_SelectedLabel"] <- output
			request_body1[names(request_body1)=="WORKPLACE_SetSelected"] <- ifelse(state=="","0","-1")

		}
	}

	# (2) Go to the report table page ----
	# Page has additional parameters set that are unique to the report being requested
	response_2 <- get_post_response(
		url = report_url,
		referer_url = browse_tables_url, # this may be unnecessary
		request_body = request_body1,
		debug_request = debug_request
	)
	request_body3 <- get_post_body_list(httr::content(response_2,"text"), debug_request = debug_request)
	# (2b) Set error type output to "SE" (parameter is named "MOE" regardless of error type). Consider creating an error_type parameter instead of forcing SE on user.
	if(length(request_body3[grepl("MOE",request_body3)]) > 0) {
		request_body3[grepl("MOE",request_body3)] <- gsub("MOE","SE",request_body3[grepl("MOE",request_body3)][[1]])
	}

	# (3) Go to the csv download page ----
	# Page will return a text/csv response based on the parameters we've compiled and send in the body
	response_3 <- get_post_response(
		url = report_csv_url,
		referer_url = report_url,
		request_body = request_body3,
		debug_request = debug_request
	)

	# (4) Parse csv text to table ----
	# Handle raw binary text if attempt to parse text returns NA (meaning server returned a binary blob. may be due to exceeding a row count or size)
	raw_text <- httr::content(response_3, type="text", encoding="UTF-8")
	if(is.na(raw_text)) {
		raw_text <- httr::content(response_3, as="raw")
		raw_text <- readBin(raw_text, what='characer')
	}
	line_count <- length(strsplit(raw_text,"\r")[[1]])
	raw_text <- gsub(",\\r\\n","\r\n",raw_text) # text has unwanted trailing column in header row
	table <- data.table::fread(raw_text, skip=2, nrows=(line_count-6), colClasses="character") # skip=2 removes title in row 1 and date in row 2. nrows integer removes citation in 4 tail rows
	# tidyverse option
	# table <- readr::read_csv(raw_text, skip=2, n_max=(line_count-6)) # skip=2 removes title in row 1 and date in row 2. n_max integer removes citation in 4 tail rows

	# (5) Clean and standardize table ----
	# (5a) row-to-column the Standard Error value if it's present (almost always)
	# (5b) rename value output column to "Estimate"
	if(length(unique(table$Output))==1 && unique(table$Output)=="Estimate") {
		names(table)[ncol(table)] <- "Estimate"
		table$Output <- NULL
	} else {
		names(table)[ncol(table)] <- "Estimate"
		SE <- table[Output=="Standard Error", ncol(table), with=F]
		names(SE) <- "SE"
		table <- cbind(table[!table$Output=="Standard Error"], SE)
		table$Output <- NULL
	}

	# (5c) character-to-numeric output
	table[, Estimate:=gsub(",","",Estimate), ]
	class(table$Estimate) <- "numeric"
	if("SE" %in% names(table)) {
		table[, SE:=gsub(",","",SE), ]
		class(table$SE) <- "numeric"
	}

	# (6) Return table with attributes ----
	setattr(table, name = "id", value = report_id)
	setattr(table, name = "name", value = gsub("\"","",(data.table::fread(raw_text, nrows=1, header=F, quote="", sep="`")[[1]])))
	setattr(table, name = "geography", value = geography_attribute_value)
	setattr(table, name = "report_type", value = report_type_name)
	setattr(table, name = "dataset", value = dataset_id)
	setattr(table, name = "output", value = output)

	return(table)

}
