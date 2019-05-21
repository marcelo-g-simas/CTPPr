#' View CTPP Tables Available for Download
#'
#' This function returns a list of CTPP table identifiers and descriptions
#' @param style "raw" or "r" to return a r data.table object or "browser" (default) to return a javascript datatable
#' @export
#' @examples
#' ctpp_tables()
#' ctpp_tables("raw")
#' @import DT

ctpp_tables <- function(style="browser", select=NA) {

	table_list <- CTPPr::tables

	# allow user to input a list of table id's to prefilter the table output
	if(length(style)>1) {
		select <- c(style,select)
		style <- "browser"
	}
	if(length(select)>=1) {
		table_list <- table_list[name %in% c(style, select), ]
		if(nrow(table_list)==0) {
			table_list <- CTPPr::tables
		}
	}

	style <- tolower(style)

	names(table_list)[names(table_list)=="name"] <- "ID"
	names(table_list)[names(table_list)=="label"] <- "Name"

	if(is.na(style) || style %in% c("raw","r","normal","data.frame","data.table")) {

		table_list[, `:=` (Datasets = paste0(dataset, collapse=", ")), by = ID]
		table_list$dataset <- NULL
		table_list$id <- NULL
		return(table_list)
	}

	if(style %in% c("browser","html","web")) {

		url_text <- '<svg xmlns="http://www.w3.org/2000/svg" width="12" height="12"> <path fill="#fff" stroke="#36c" d="M1.5 4.518h5.982V10.5H1.5z"/> <path fill="#36c" d="M5.765 1H11v5.39L9.427 7.937l-1.31-1.31L5.393 9.35l-2.69-2.688 2.81-2.808L4.2 2.544z"/> <path fill="#fff" d="M9.995 2.004l.022 4.885L8.2 5.07 5.32 7.95 4.09 6.723l2.882-2.88-1.85-1.852z"/> </svg>'
		table_list$`Definition` <- paste0("<a target=\"_blank\" href='http://data5.ctpp.transportation.org/ctpp",ifelse(table_list$dataset=="2016","1216",""),"/View/ReportDefinitionSpawned.aspx?ReportId=",table_list$id, "'>",url_text,"</a>")
		table_list$`CTPP Web Tool` <- paste0("<a target=\"_blank\" href='http://data5.ctpp.transportation.org/ctpp",ifelse(table_list$dataset=="2016","1216",""),"/View/dispview.aspx?ReportId=",table_list$id, "'>",url_text,"</a>")
		table_list$id <- NULL
		table_list$Name <- toupper(table_list$Name)
		table_list <- table_list[, list(Definition = paste0(paste0(dataset, " ", Definition), collapse="<br>"), `CTPP Web Tool` = paste0(paste0(dataset, " ", `CTPP Web Tool`), collapse="<br>")), by = list(ID,Name)]

		table_list <- DT::datatable(
			table_list,
			class = "compact row-border hover cell-border data-elements",
			filter = "top",
			rownames = FALSE,
			escape = FALSE,
			selection = "none",
			#plugins = c("searchHighlight"), # not working
			extensions = c("FixedHeader"),
			options = list(
				dom = "t",
		  	pageLength = nrow(table_list),
		  	columnDefs = list(
		  		list(width="10%", targets=0),
		  		list(width="70%", targets=1),
		  		list(width="10%", targets=2, orderable = FALSE),
		  		list(width="10%", targets=3, orderable = FALSE)
		  		),
				#searchHighlight = TRUE, # not working
				fixedHeader = TRUE
		  ),
			callback = htmlwidgets::JS(paste0("
				//$('<h4>Click on an ID to copy the download_ctpp() request to your clipboard.</h4>').insertBefore('#DataTables_Table_0_wrapper');
				$('#DataTables_Table_0 td:first-child').css('cursor','pointer');
				$('#DataTables_Table_0 td:first-child').attr('title','Copy to clipboard');
				$('#DataTables_Table_0 td:first-child').hover( function(e){ $(this).css('font-weight', 'bold');},function(e){ $(this).css('font-weight', '');});
				function addRowHandlers() {
					var rows = document.getElementById('DataTables_Table_0').getElementsByTagName('tbody')[0].rows;
					for (i = 0; i < rows.length; i++) {
						rows[i].onclick = function(){ return function(){
							var id = this.cells[0].innerHTML;
							var table_label = this.cells[1].innerHTML;
							if(id.slice(-1)==='*') {
								this.cells[0].innerHTML = id.slice(0,-1);
								var id = this.cells[0].innerHTML;
								$(this).css('font-weight', '');
							} else {
								this.cells[0].innerHTML = id + '*';
								$(this).css('font-weight', 'bold');
							}
							var $temp = $('<textarea>');
				      $(\'body\').append($temp);
							syntax_to_copy =  '# ' + id + ' - ' + table_label + '\\r\\n' + id + ' <- download_ctpp(\\r\\n\\tid = \"' + id + '\", \\r\\n\\tgeography = \"County\", \\r\\n\\tstate = \"Maryland\", \\r\\n\\tdataset = \"2016\", \\r\\n\\toutput = \"Name\"\\r\\n)';
				      $temp.val(syntax_to_copy).select();
				      document.execCommand('copy');
				      $temp.remove();
						};}(rows[i]);
					}
				}
				window.onload = addRowHandlers();
			"))
		)

		return(table_list)

	}

}
