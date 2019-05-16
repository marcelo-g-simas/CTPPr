#==================================================================================================#
# geographies ----
ctpp_geography_list <- data.frame(rbind(
	#c("01","National","National"),
	#c("02","State","State"),
	c("03","County","State-County"),
	c("04","MCD","State-County-MCD (for 12 strong MCD states)"),
	c("05","Place","State-Place"),
	c("07","MSA","Metropolitan Statistical Area"),
	c("08","City","Metropolitan Statistical Area – EACH Principal City"),
	c("09","PUMA","State-PUMA5"),
	c("10","UA","Urbanized Area (UA)"),
	c("11","Tract","State-County-Tract"),
	c("12","TAD","TAD"),
	c("13","TAZ","TAZ"),
	c("42","State->State","State -> POW State"),
	c("43","County->County","State-County -> POW State-County"),
	c("44","MCD->MCD","State-County-MCD (for 12 strong MCD states) -> POW State-County-MCD (for 12 strong MCD states)"),
	c("45","Place->Place","State-Place -> POW State-Place"),
	c("48","City->City","Metropolitan Statistical Area – EACH Principal City -> POW Metropolitan Statistical Area – EACH Principal City"),
	c("49","PUMA->PUMA","State-PUMA5 -> POW State-POWPUMA"),
	c("51","County->Place","State-County -> POW State-Place"),
	c("52","MCD->Place","State-County-MCD (for 12 strong MCD states) -> POW State-Place"),
	c("53","PUMA->Place","State-PUMA5 -> POW State-Place"),
	c("54","Tract->Tract","State-County-Tract -> State-County-Tract "),
	c("55","TAD->TAD","TAD -> TAD"),
	c("57","TAD->TAZ","TAZ -> TAZ"),
	c("56","TAZ->TAZ","TAD -> TAZ"),
	c("58","TAZ->TAD","TAZ -> TAD"),
	c("59","Place->TAZ","State-Place -> TAZ"),
	c("60","TAZ->Place","TAZ -> POW State-Place")
), stringsAsFactors = F)
names(ctpp_geography_list) <- c("ID","Short Label","Label")

usethis::use_data(ctpp_geography_list, overwrite=T)

#==================================================================================================#
# states ----
ctpp_state_list <- data.frame(
	"Name" = c("Alabama","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Puerto Rico","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"),
	"Abbreviation" = c("AL","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID","IL","IN","IA","KS","KY","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA","PR","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"),
	"FIPS" = c("01","04","05","06","08","09","10","11","12","13","15","16","17","18","19","20","21","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","72","44","45","46","47","48","49","50","51","53","54","55","56"),
	stringsAsFactors = F
)

usethis::use_data(ctpp_state_list)

#==================================================================================================#
# datasets ----
ctpp_dataset_list <- data.frame(rbind(
	c("2006-2010","2010","2006-2010 (5-year)","ctpp"),
	c("2012-2016","2016","2012-2016 (5-year)","ctpp1216")
	# c("2009-2013","2013","ACS 2009-2013 County to County Flows","0913")
), stringsAsFactors = F)
names(ctpp_dataset_list) <- c("Dataset","Short Label","Label","URL ID")

usethis::use_data(ctpp_dataset_list)

#==================================================================================================#
# tables ----
html_for_table_list <- httr::GET("http://data5.ctpp.transportation.org/ctpp1216/Browse/BrowseTables.aspx")
table_list <- XML::htmlParse(html_for_table_list, trim=F, asText=T, replaceEntities=T)
table_list <- XML::getNodeSet(table_list, "//a[@class = \"rtIn DataBodyTablesTreeView\"]")
table_list <- setNames(
	lapply(table_list, function(x) {XML::xmlValue(x)} ),
	lapply(table_list, function(x) {XML::xmlGetAttr(node = x, name="href")} )
)

tables <- data.table::rbindlist(lapply(table_list, function(x) { as.data.frame(x, stringsAsFactors=F) }))
names(tables) <- c("name")
tables$label <- substr(tables$name,regexpr("\\-",tables$name)+1,nchar(tables$name))
tables$label <- trimws(tables$label)
tables$name <- substr(tables$name,1,regexpr("\\-",tables$name)-1)
tables$name <- trimws(tables$name)
tables$id <- names(table_list)
tables$id <- gsub("javascript:OnLoadView(","",tables$id,fixed=T)
tables$id <- gsub(",1,true)","",tables$id, fixed=T)
tables$dataset <- "2016"
tables <- tables[, c("dataset","name","id","label")]

# tables <- rbind(tables10, tables16)
usethis::use_data(tables)

# insert new/found table records that do not exist in CTPPr::tables
new_tables <- tables[!tables$name %in% CTPPr::tables[dataset=="2016", name], ]
# new_tables <- tables[!tables$label %in% CTPPr::tables[dataset=="2016", label], ]

tables <- rbind(CTPPr::tables, new_tables)
tables <- rbind(CTPPr::tables[!dataset="2016"], new_tables)
setorder(tables, dataset, name)

#==================================================================================================#
# table definitions ----
# table_definitions <- lapply(tables$id, function(x) {
# 	report_definition <- xml2::read_html(paste0("http://ctpp.beyond2020.com/ctpp/View/ReportDefinitionSpawned.aspx?ReportId=", x))
# 	report_definition <- rvest::html_nodes(report_definition, css="div > table")
# 	return(as.character(report_definition[[1]]))
# })
# names(table_definitions) <- tables$id
#
# usethis::use_data(table_definitions)

