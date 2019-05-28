#' View CTPP Tables Available for Download
#'
#' This function returns a list of CTPP table identifiers and descriptions. You can click on the ID in the default version to copy an example download_ctpp() request to your clipboard.
#' @param style "raw" or "r" to return a r data.table object or "browser" (default) to return a javascript datatable
#' @param select character vector of table id's. This will return only the tables specified, which may be useful for presentations or when you would like to hide records by default.
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

	table_list[, "Type":=regmatches(ID, regexpr("\\d", ID))]
	table_list[Type==1, "Type":="RESIDENCE"]
	table_list[Type==2, "Type":="WORKPLACE"]
	table_list[Type==3, "Type":="FLOW"]
	setcolorder(table_list, c("Type",setdiff(names(table_list),"Type")))

	if(is.na(style) || style %in% c("raw","r","normal","data.frame","data.table")) {

		table_list[, `:=` (Datasets = paste0(dataset, collapse=", ")), by = ID]
		table_list$dataset <- NULL
		table_list$id <- NULL
		return(table_list)
	}

	if(style %in% c("browser","html","web")) {

		url_text <- '<svg xmlns="http://www.w3.org/2000/svg" width="12" height="12"> <path fill="#fff" stroke="#36c" d="M1.5 4.518h5.982V10.5H1.5z"/> <path fill="#36c" d="M5.765 1H11v5.39L9.427 7.937l-1.31-1.31L5.393 9.35l-2.69-2.688 2.81-2.808L4.2 2.544z"/> <path fill="#fff" d="M9.995 2.004l.022 4.885L8.2 5.07 5.32 7.95 4.09 6.723l2.882-2.88-1.85-1.852z"/> </svg>'
		table_list$`Definition` <- paste0("<a target=\"_blank\" href='http://data5.ctpp.transportation.org/ctpp",ifelse(table_list$dataset=="2016","1216",""),"/View/ReportDefinitionSpawned.aspx?ReportId=",table_list$id, "'>",url_text,"</a>")
		table_list$`CTPP Web&nbsp;Tool` <- paste0("<a target=\"_blank\" href='http://data5.ctpp.transportation.org/ctpp",ifelse(table_list$dataset=="2016","1216",""),"/View/dispview.aspx?ReportId=",table_list$id, "'>",url_text,"</a>")
		table_list$id <- NULL
		table_list$Name <- toupper(table_list$Name)
		table_list <- table_list[, list(Definition = paste0(paste0(dataset, " ", Definition), collapse="<br>"), `CTPP Web&nbsp;Tool` = paste0(paste0(dataset, " ", `CTPP Web&nbsp;Tool`), collapse="<br>")), by = list(ID,Name,Type)]

		# select-input style a datatable column by making it a factor
		# table_list[, Type:=factor(Type, c('RESIDENCE', 'WORKPLACE', 'FLOW'), c('RESIDENCE', 'WORKPLACE', 'FLOW')), ]

		random_element_id <- paste0("DT_",paste(sample(LETTERS, 6), collapse="")) # allows us to call JS on multiple datatables on the same page. html ID's must be unique

		table_list <- DT::datatable(
			table_list,
			class = "compact row-border hover cell-border",
			filter = "top",
			rownames = FALSE,
			escape = FALSE,
			selection = "none",
			elementId = random_element_id,
			#plugins = c("searchHighlight"), # not working
			extensions = c("FixedHeader"),
			options = list(
				dom = "t",
		  	pageLength = nrow(table_list),
		  	columnDefs = list(
					list(width="8%", targets=0),
		  		list(width="68%", targets=1),
		  		list(width="10%", targets=2),
		  		list(width="7%", targets=3, orderable = FALSE, searchable = FALSE),
		  		list(width="7%", targets=4, orderable = FALSE, searchable = FALSE)
		  		),
				#searchHighlight = TRUE, # not working
				fixedHeader = TRUE
		  ),
			callback = htmlwidgets::JS(paste0("
				//$('<h4>Click on an ID to copy the download_ctpp() request to your clipboard.</h4>').insertBefore('#",random_element_id,"_wrapper');
				$('#",random_element_id," td:nth-child(1)').css('cursor','pointer');
				$('#",random_element_id," td:nth-child(1)').attr('title','Copy to clipboard');
				$('#",random_element_id," td:nth-child(1)').hover( function(e){ $(this).css('font-weight', 'bold');},function(e){ $(this).css('font-weight', '');});
				function addRowHandlers() {
					var rows = document.getElementById('",random_element_id,"').getElementsByTagName('tbody')[0].rows;
					for (i = 0; i < rows.length; i++) {
						rows[i].onclick = function(){ return function(){
							var id = this.cells[0].innerHTML;
							var name = this.cells[1].innerHTML;
							if(id.slice(-1)==='*') {
								this.cells[0].innerHTML = id.slice(0,-1);
								var id = this.cells[0].innerHTML;
								$(this).css('font-weight', '');
							} else {
								this.cells[0].innerHTML = id + '*';
								$(this).css('font-weight', 'bold');
							}
				      if (this.cells[3].innerText.indexOf('2016') >= 0) { var dataset_example = '2016'; } else { var dataset_example = '2010'; }
				      if (this.cells[2].innerText.indexOf('FLOW') >= 0) { var geography_example = 'County->County'; } else { var geography_example = 'County'; }
				      var $temp = $('<textarea>');
				      $(\'body\').append($temp);
							syntax_to_copy =  '# ' + id + ' - ' + name + '\\r\\n' + id + ' <- download_ctpp(\\r\\n\\tid = \"' + id + '\", \\r\\n\\tgeography = \"' + geography_example + '\", \\r\\n\\tstate = \"Maryland\", \\r\\n\\tdataset = \"' + dataset_example + '\", \\r\\n\\toutput = \"FIPS and Name\"\\r\\n)';
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
