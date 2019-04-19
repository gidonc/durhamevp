

compile_report <- function(event_report_id, data_location="database", output_format="html_document", output_file=NULL, date_report_format="%A, %d %B %Y", report_title="Event Reports from Election Violence Database"){
  #' Produces a readable summary of event reports from the election violence database.
  #'
  #' @param event_report_id the event report id or ids to produce the report for.
  #' @param data_location where data is retrived from (currently only database supported).
  #' @param output_format produces reports as html_document (default), word_document, or pdf_document.
  #' @param output_file the file name to save the report to (optional - if not supplied a temp file will be created).
  #' @param date_report_format desired date format for output.
  #' @export

  if(data_location=="database"){
    this_er <- durhamevp::get_event_report(event_report_id = event_report_id)
  }


  ## will need loop here calling compile event report
  #this_er <- event_reports   %>%
  #  filter(event_report_id %in% c(this_er_id))

  markhead <- c('---',
                paste('title:', report_title),
                'output: html_document',
                '---',
                '')
  markbody <- NULL
  for (i in 1:nrow(this_er)){
    markbody <- c('', markbody, durhamevp::compile_event_report_markdown(this_er[i,], data_location=data_location, date_report_format=date_report_format))
    markbody <- c(markbody, "----", "")
  }


  mark <- c(markhead, markbody)


  tmp <- base::tempfile(fileext =".Rmd")
  base::writeLines(mark, con=tmp)

  if(is.null(output_file)){
    if(output_format=="word_document"){
      output_file<- base::tempfile(fileext =".docx")
    } else if(output_format=="pdf_document"){
      output_file<- base::tempfile(fileext =".pdf")
    } else
      output_file<- base::tempfile(fileext =".html")
  }
  rmarkdown::render(tmp, output_file=output_file, output_format=output_format, quiet=TRUE)
  utils::browseURL(output_file)


}


compile_event_report_markdown<- function (this_er, data_location="database", date_report_format="%A, %d %B %Y"){
  #'
  #'@export

  this_user_doc_id <- this_er$user_doc_id
  this_er_id <- this_er$event_report_id
  if(data_location=="database"){
    this_user_doc <- durhamevp::get_allocation_connect_to_docs(user_doc_id = this_user_doc_id)
    these_tags <- durhamevp::get_tag(event_report_id = this_er_id)
  }

  this_locations <- these_tags %>%
    dplyr::filter(tag_table=="location", event_report_id == this_er_id) %>%
    dplyr::select(-tag_id, -tag_table, -event_report_id) %>%
    dplyr::rename("location level"="tag_variable", place=tag_value)

  this_tags <- these_tags %>%
    dplyr::filter(tag_table!="location") %>%
    dplyr::arrange(tag_table, desc(tag_variable), tag_value) %>%
    dplyr::mutate(tag_variable = ifelse(tag_variable=="", "unspecified", tag_variable)) %>%
    dplyr::mutate(tag_value = ifelse(tag_value=="", "unspecified", tag_value))

  last_tag_table <- "nothing"
  last_tag_variable <- "nothing"
  tags_output <- NULL
  for (i in 1:nrow(this_tags)){
    this_tag <- this_tags[i, ]
    this_tag_table <- this_tag$tag_table
    this_tag_variable <- this_tag$tag_variable
    this_tag_value <- this_tag$tag_value
    this_tag_comment<- ifelse(this_tag$comment_tags=="", "", paste0("(comment: ", this_tag$comment_tags, ")"))
    this_tag_id <- this_tag$tag_id

    if (this_tag_table!=last_tag_table){
      tags_output <- c(tags_output,
                       '',
                       paste("###", this_tag_table),
                       '')
    }
    if (this_tag_variable!=last_tag_variable){
      tags_output <- c(tags_output,
                       '',
                       paste("####", this_tag_variable),
                       '')
    }
    tags_output <- c(tags_output,
                     '',
                     paste(this_tag_value, create_attribute_info_markdown(this_tag_id, data_location), this_tag_comment),
                     '')
    last_tag_table <- this_tag_table
    last_tag_variable <- this_tag_variable
  }


  my_dateform <- function(x){
    format(x, date_report_format)
  }

  pub_date<- my_dateform(lubridate::ymd(this_user_doc$publication_date))
  if(is.na(pub_date)){
    pub_date <- paste(this_user_doc$publication_date, ": date stamp not producing result")
  }

  kmarkobj <- c(
               paste0('',
                      '## Event Report Id:', this_er$event_report_id),
               paste0("[article pdf](http://coders.victorianelectionviolence.uk", this_user_doc$pdf_location, ")"),
               '',
               '### Summary',
               this_er[,c("summary")],
               '',
               '### Locations',
               knitr::kable(this_locations),
               '### Dates',
               paste("Event date start:", this_er$event_timeframe_quantifier, my_dateform(lubridate::dmy(this_er$event_start))),
               '',
               paste("Event date end:", this_er$event_timeframe_quantifier, my_dateform(lubridate::dmy(this_er$event_end))),
               '',
               paste("Article Publication date:", pub_date),
               '',
               '### Environment and Election Point',
               knitr::kable(this_er[,c("environment", "election_point")]),
               tags_output)
  kmarkobj
}







  create_attribute_info_markdown<- function (the_tag_id, data_location="database"){
    #' @export

    if (data_location=="database"){
      att <- get_attribute(tag_id=the_tag_id)
    }
    these_attributes <- att %>%
      dplyr::filter(tag_id==the_tag_id) %>%
      dplyr::arrange(attribute, attribute_value) %>%
      dplyr::mutate(attribute = ifelse(attribute=="", "unspecified", attribute))
    if(nrow(these_attributes)==0){
      attributes_output <- NULL
    } else {
      attributes_output <- "("
      last_attribute <- "nothing"
      for (i in 1:nrow(these_attributes)){
        this_attribute_row <- these_attributes[i, ]
        this_attribute <- this_attribute_row$attribute
        this_attribute_value <- this_attribute_row$attribute_value

        if (this_attribute!=last_attribute){
          attributes_output <- paste0(attributes_output, this_attribute, ": ")
        } else {
          attributes_output <- paste0(attributes_output, ", ", this_attribute)
        }
        attributes_output <- paste0(attributes_output, " ",
                                    this_attribute_value)
        last_tag_table <- this_tag_table
        last_tag_variable <- this_tag_variable
      }
      attributes_output <- paste0(attributes_output, ")")
    }

    attributes_output
  }




