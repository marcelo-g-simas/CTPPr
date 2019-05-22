#' @import surveyvisualize
#==================================================================================================#
#' @export
get_post_body_list <- function(html) {
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
	return(request_body)
}

#==================================================================================================#
#' @export
get_post_response <- function(url, host_url, referer_url, request_body, debug_request=F) {

	response <- httr::POST(
		url = url,
		httr::add_headers(
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
			"\nSomething wrong with web request (HTTP error request code = ", response$status_code, ").",
			"\nURL attempted: ", url,
			"\nMake sure report table exists. View available tables by calling: ctpp_tables()"
		)
	}

	return(response)

}
