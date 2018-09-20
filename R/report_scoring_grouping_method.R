
coding_summary <- function(user_doc_id){
  #' Summary of coding of document allocation.
  #'
  #' Returns tibble with number of items of predefined types coded in the document by the user.
  #'
  #'
  #' @return Returns tibble with number of items of predefined types coded in the document by the user.
  #' @export
  user_docs<-durhamevp::get_allocation(user_doc_id = user_doc_id)

  event_report <- durhamevp::get_event_report(user_doc_id=dplyr::pull(user_docs, "id"))
  #model_event_report <- durhamevp::get_event_report(model_event_report_id)
  tags<-durhamevp::get_tag(event_report_id = dplyr::pull(event_report, "id"))
  attributes<-durhamevp::get_attribute(tag_id = tags$id)

  tags<-dplyr::left_join(tags, event_report, by=c("event_report_id"="id"), suffix=c(".tags", "event_report")) %>%
    left_join(user_docs, by=c("user_doc_id"="id"), suffix=c(".x", "user_doc"))
  attributes<-dplyr::left_join(attributes, tags, by=c("tag_id"="id"), suffix=c("attributes", "tags"))


  # process by tag type
  # tag types location, action (violence), action (cause, cause_association, consequence), actors, others

  sorted_event_reports<-get_sorted(model_event_report, user_event_report)
  sorted_tags<-NULL
  sorted_attributes<-NULL

  for (the_pair in 1:nrow(sorted_event_reports)){
    user_event_report_id <- sorted_event_reports[the_pair, "user_var"]
    model_event_report_id<- sorted_event_reports[the_pair, "model_var"]
    user_event_report_tags <- user_tags[user_tags$event_report_id %in% as.numeric(user_event_report_id), ]
    model_event_report_tags <- model_tags[model_tags$event_report_id %in% as.numeric(model_event_report_id), ]


    for (type_n in 1:5){
      if (type_n ==1){
        user_this_type <- user_event_report_tags[user_event_report_tags$tag_table=="location",]
        model_this_type <- model_event_report_tags[model_event_report_tags$tag_table=="location",]
      } else if (type_n==2){
        user_this_type <- user_event_report_tags[user_event_report_tags$tag_table=="actions" & user_event_report_tags$tag_variable=="violence",]
        model_this_type <- model_event_report_tags[model_event_report_tags$tag_table=="actions"  & model_event_report_tags$tag_variable=="violence",]
      } else if (type_n==3){
        user_this_type <- user_event_report_tags[user_event_report_tags$tag_table=="actions" & user_event_report_tags$tag_variable %in% c("cause", "cause_association", "consequence"),]
        model_this_type <-model_event_report_tags[model_event_report_tags$tag_table=="actions"  & model_event_report_tags$tag_variable %in% c("cause", "cause_association", "consequence"),]
      } else if (type_n==4){
        user_this_type <- user_event_report_tags[user_event_report_tags$tag_table=="actors",]
        model_this_type <-model_event_report_tags[model_event_report_tags$tag_table=="actors",]
      } else if (type_n==5){
        user_this_type<-user_event_report_tags[!(user_event_report_tags$id %in% sorted_tags$user_var),]
        model_this_type<-model_event_report_tags[!(model_event_report_tags$id %in% sorted_tags$model_var),]
      }
      if (is.null(sorted_tags)){
        sorted_tags<- get_sorted(model_this_type, user_this_type, get_distance)
        # print(the_pair)
        # print(type_n)
        # print(sorted_tags)
      } else {
        sorted_tags <- dplyr::bind_rows(sorted_tags,
                                        get_sorted(model_this_type, user_this_type, get_distance))
        # print(the_pair)
        # print(type_n)
        # print(sorted_tags)
      }
    }
  }

  for (the_pair in 1:nrow(sorted_tags)){
    user_tag_id <- sorted_tags[the_pair, "user_var"]
    model_tag_id<- sorted_tags[the_pair, "model_var"]
    user_tag_attributes <- user_attributes[user_attributes$tag_id %in% as.numeric(user_tag_id), ]
    model_tag_attributes <- model_attributes[model_attributes$tag_id %in% as.numeric(model_tag_id), ]

    if (is.null(sorted_attributes)){
      sorted_attributes<-get_sorted(model_tag_attributes, user_tag_attributes, get_distance)
    } else {
      sorted_attributes <- dplyr::bind_rows(sorted_attributes,
                                            get_sorted(model_tag_attributes, user_tag_attributes, get_distance))
    }
  }

  list(sorted_event_reports, sorted_tags, sorted_attributes)
}

