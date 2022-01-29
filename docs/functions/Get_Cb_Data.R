Get_Cb_Data <- function(Serie = "TP.FG.J0",
                        Start_Date = "01-01-2013",
                        End_Date = lubridate::today(),
                        API = "4VwjGbTONS"){
  URL <- paste0(
      "https://evds2.tcmb.gov.tr/service/evds/series=",
      paste0(Serie, collapse = "-"),
      "&startDate=",
      Start_Date,
      "&endDate=",
      format(End_Date, "%d-%m-%Y"),
      "&type=xml&key=",
      API
    )
  doc <- xmlTreeParse(GET(URL))
  root <- xmlRoot(doc)
  my_data <- xmlSApply(root, function(x)
    xmlSApply(x, xmlValue))
  my_data <- data.frame(my_data, row.names = NULL)
  my_data <- t(my_data)
  colnames(my_data) <- unique(names(root[2]$items))
  my_data <- my_data[-1, c(1:(ncol(my_data) - 1))]
  rownames(my_data) <- NULL
  my_data <- as_tibble(my_data)
  my_data
}


