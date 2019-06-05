#==================================================================================================#
#' @export
get_post_body_list <- function(html, debug_request=F) {
	# resolve issue with some encodings truncating the html values we read in
	html <- gsub("&#28;","",html) # this special html code causes problems: FS file separator %1C
	html <- gsub("&#29;","",html) # this special html code causes problems: GS group separator %1D
	html <- gsub("&#24;","",html) # this special html code causes problems: CAN cancel %18
	html_parsed <- XML::htmlParse(html, trim=F, asText=T, replaceEntities=T)
	# turn hidden form values into a list of POST body parameters
	hidden_inputs <- XML::getNodeSet(html_parsed, "//input[@type = \"hidden\"]")
	hidden_inputs <- setNames(
		lapply(hidden_inputs, function(x) {XML::xmlGetAttr(node = x, name="value")} ),
		lapply(hidden_inputs, function(x) {XML::xmlGetAttr(node = x, name="name")} )
	)
	hidden_inputs <- as.list(hidden_inputs)
	hidden_inputs <- replace(hidden_inputs, sapply(hidden_inputs, is.null), "")
	request_body <- c(
		list(
			`__EVENTTARGET` = "",
			`__EVENTARGUMENT` = ""
		),
		hidden_inputs
	)
	if(debug_request) {
		df <- stack(request_body)
		df <- df[, c(2, 1)]
		write.table(df, paste0("request_body(",proc.time()[3][1],").csv"), quote=FALSE, sep=":", row.names=F)
	}
	return(request_body)
}

#==================================================================================================#
#' @export
get_post_response <- function(url, host_url, referer_url, request_body, debug_request=F) {

	response <- httr::RETRY(times = 6, pause_cap = 12, quiet = TRUE,
		verb = "POST",
		url = url,
		config = httr::add_headers(
			`Referer` = referer_url,
			`Upgrade-Insecure-Requests` = "1",
			`Content-Type` = "application/x-www-form-urlencoded",
			`Accept` = "text/csv,text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3",
			`Connection` = "keep-alive",
			`Cache-Control` = "max-age=0"
		),
		body = request_body,
		encode = "form",
		{ if(debug_request) { httr::verbose() } }
	)

	if(response$status_code != 200) {
		stop(
			"\nWeb service may be down (HTTP error request code = ", response$status_code, ").",
			"\nURL attempted: ", url,
			"\nYou may try again in a few seconds if you know the requested report table exists. View available tables with ctpp_tables()"
		)
	}

	return(response)

}

#==================================================================================================#
.onAttach <- function(libname, pkgname) {

	# put surveyvisualize functions in namespace on load while avoiding showing a mess of messages (from surveyvisualize's dependency on ggplot2)
	# this also let's us avoid using Depends: in DESCRIPTION file while still downloading and installing it when this package is installed with devtools::install_*
	invisible(suppressWarnings(suppressPackageStartupMessages((library(surveyvisualize, quietly=T)))))

  if(interactive()) {
  	packageStartupMessage('___________________________________________________________')
    packageStartupMessage("CTPPr ", paste0(packageVersion("CTPPr")))
    packageStartupMessage("Developed by Westat: https://www.westat.com")
    packageStartupMessage('===========================================================')
    packageStartupMessage("GitHub page: https://github.com/Westat-Transportation/CTPPr")
    packageStartupMessage("CTPP Program page: https://ctpp.transportation.org/\n")
  }

}
