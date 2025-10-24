get_download_locations <- function() {
  c(Home = fs::path_home(), "Downloads" = fs::path_home("Downloads"))
}


show_datatable <- function(.data, app_number, edit = FALSE) {
  if(app_number == 1) {
    DT::datatable(
      .data,
      editable = edit,
      options = list(
        dom = 't', # Only show the table, no controls
        ordering = FALSE,
        scrolly = '800px',
        searchHighlight = TRUE
      ),
      rownames = FALSE
    )
  } else if(app_number == 2) {
    # uses the same as 1 for now
  } else if(app_number == 3) {
    DT::datatable(
      .data,
      options = list(dom = 't', ordering = FALSE, pageLength = 50),
      rownames = FALSE,
      editable = edit
    )
  }
}
