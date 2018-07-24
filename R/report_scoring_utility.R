
get_data_for_comparison <- function(model_user_doc_id, user_user_doc_id){
  #' Returns a list of directly comparable components (contained in tuples)
  #' the tuples contain the most similar components; the similarity is measured using the 
  #' Levenshtein distance between them using all of their fields in the serialization process.
  #' 
  #' for every pair of events (and consequently, for any tags and attributes associated with these events), we use the 
  #' get_sorted method to find the best match and save them in the objects represeting user answers (such as user_event, user_violence))
  model_document_allocation<-durhamevp::get_allocation(user_doc_id = model_user_doc_id)
  user_document_allocation<-durhamevp::get_allocation(user_doc_id = user_user_doc_id)
  
  
  model_event_report <- durhamevp::get_event_report(user_doc_id=dplyr::pull(model_document_allocation, "id"))
  #model_event_report <- durhamevp::get_event_report(model_event_report_id)
  model_tags<-durhamevp::get_tag(event_report_id = dplyr::pull(model_event_report, "id"))
  model_attributes<-durhamevp::get_attribute(tag_id = model_tags$id)
  
  user_event_report <- durhamevp::get_event_report(user_doc_id=dplyr::pull(user_document_allocation, "id"))
  #user_event_report <- durhamevp::get_event_report(user_event_report_id)
  user_tags<-dplyr::as_tibble(durhamevp::get_tag(event_report_id = user_event_report$id))
  user_attributes<-dplyr::as_tibble(durhamevp::get_attribute(tag_id = user_tags$id))
  # print(user_attributes)
  
  sorted_event_reports<-get_sorted(model_event_report, user_event_report)
  sorted_tags<-NULL
  sorted_attributes<-NULL
  
  for (the_pair in 1:nrow(sorted_event_reports)){
    user_event_report_id <- sorted_event_reports[the_pair, "user_var"]
    model_event_report_id<- sorted_event_reports[the_pair, "model_var"]
    user_event_report_tags <- user_tags[user_tags$event_report_id %in% as.numeric(user_event_report_id), ]
    model_event_report_tags <- model_tags[model_tags$event_report_id %in% as.numeric(model_event_report_id), ]
    
    # process by tag type
    # tag types location, action (violence), action (cause, cause_association, consequence), actors, others
    
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


get_sorted <- function(model_vars, user_vars, get_var_score=get_distance, blank_var_gen=NULL){
  #'Uses a scoring matrix to decide which model (model answer) component should be used to score
  #'every user (user answer) component
  #'throughout the function, 'lowest' scores are the ones that represent a near-perfect match
  #'@param get_var_score I think that this is a function 
  
  # the scoring matrix is initialized and filled with values
  # the first entry of each row is the index of the user_var that is scored in that row
  
  n_user_vars <- nrow(user_vars)
  n_model_vars <- nrow(model_vars)
  scoring_matrix = data.frame(matrix(nrow=n_user_vars * n_model_vars, ncol=3))
  names(scoring_matrix)<- c("user_var", "model_var", "score")
  if (n_user_vars==0 & n_model_vars==0){
    matched_tuple<-dplyr::as_tibble(matrix(nrow=0, ncol=3))
    names(matched_tuple)<- c("user_var", "model_var", "score")
    return(matched_tuple)
  }
  if (n_user_vars==0){
    matched_tuple<-dplyr::as_tibble(matrix(nrow=n_model_vars, ncol=3))
    names(matched_tuple)<- c("user_var", "model_var", "score")
    matched_tuple[, "model_var"] <- model_vars[, "id"]
    return(matched_tuple)
  }
  if (n_model_vars ==0){
    matched_tuple<-dplyr::as_tibble(matrix(nrow=n_user_vars, ncol=3))
    names(matched_tuple)<- c("user_var", "model_var", "score")
    matched_tuple[, "user_var"] <- user_vars[, "id"]
    return(matched_tuple)
  }
  if (n_user_vars == 1 & n_model_vars == 1){
    matched_tuple<-dplyr::as_tibble(matrix(nrow=1, ncol=3))
    names(matched_tuple)<- c("user_var", "model_var", "score")
    matched_tuple[1, c("user_var", "model_var", "score")] <- c(user_vars[1, "id"], model_vars[1, "id"], get_var_score(model_vars, user_vars))
    return(matched_tuple)
  }
  counter<-1
  for(i in seq_along(1:n_user_vars)){
    for(j in seq_along(1:n_model_vars)){
      scoring_matrix[counter, 1] <- user_vars[i, 1]
      scoring_matrix[counter, 2] <- model_vars[j, 1] 
      scoring_matrix[counter, 3] <- get_var_score(model_vars[j,], user_vars[i,])
      counter<- counter + 1
    }
  }

  # the rows of the scoring matrix are arranged in such a way that the ones containing the lowest
  # scores have the smallest indexes
  scoring_matrix$score_order <- order(scoring_matrix[,"score"])
  model_var_indexes_to_match = seq_along(1:n_model_vars)
  user_var_indexes_to_match = seq_along(1:n_user_vars)
  
  matched_vars = matrix()
  counter = 1

  # here we check row by row what is the lowest score are and couple individual componets
  # together in tuples which we then add to the matched_vars list
  # print(scoring_matrix)
  scoring_line<- dplyr::as_tibble(scoring_matrix)
  matched_tuple<-dplyr::as_tibble(data.frame(matrix(nrow=0, ncol=3))) #dplyr::as_tibble(data.frame(matrix(nrow=max(c(n_user_vars, n_model_vars)), ncol=3)))
  names(matched_tuple)<- c("user_var", "model_var", "score")
  for (i in seq_along(1:min(c(n_user_vars, n_model_vars)))){
    lowest_score_index <- which.min(scoring_line$score)
    matched_tuple[i, c("user_var", "model_var", "score")]<-scoring_line[lowest_score_index, c("user_var", "model_var", "score")]
    #print(matched_tuple)
    scoring_line <- scoring_line[scoring_line$model_var != scoring_line$model_var[lowest_score_index] & scoring_line$user_var != scoring_line$user_var[lowest_score_index],]
    #print(scoring_line)
  }
  unmatched_model_vars <- model_vars$id[!model_vars$id %in% matched_tuple$model_var]
  # print(unmatched_model_vars)
  unmatched_user_vars <- user_vars$id[!user_vars$id %in% matched_tuple$user_var]
  # print(unmatched_user_vars)
  if (length(unmatched_model_vars)>0){
  matched_tuple <- dplyr::bind_rows(matched_tuple,
                             data.frame(model_var=unmatched_model_vars, user_var=NA, score=NA))
}
if (length(unmatched_user_vars)>0){
  matched_tuple <- dplyr::bind_rows(matched_tuple,
                             data.frame(model_var=NA, user_var=unmatched_user_vars, score=NA))
}
  matched_tuple
}

get_event_distance <- function(model_event, user_event){
  serialized_model <- serialize_row(model_event, "primitive")
  serialized_user <- serialize_row(user_event, "primitive")
  
  get_similarity_score(serialized_model, serialized_user)
}

get_event_distance <- function(model_event, user_event){
  serialized_model <- serialize_primitive_fields(model_event)
  serialized_user <- serialize_primitive_fields(user_event)
  
  get_similarity_score(serialized_model, serialized_user)
}

get_distance<- function(model, user, type="primitive"){
  s_model <- serialize_row(model, type)
  s_user <- serialize_row(user, type)
  
  get_similarity_score(s_model, s_user)
}



serialize_row<- function(df_row, serialization_type="primitive"){
  if(serialization_type=="primitive"){
    all_columns <- c(
      c("article_date_man_verify", "article_type", "geo_relevant", "time_relevant", "recommend_qualitative", 
        "electoral_nature", "violent_nature", "violent_focus"),
      c("environment", "meeting", "event_date", "election_point", "comment", "summary"),
      c("tag_variable", "tag_value", "contested"),
      c("attribute", "attribute_values")
    )
      
  } else if (serialization_type=="testing"){
    all_columns <- c(
      c("geo_relevant", "time_relevant", 
        "electoral_nature", "violent_nature", "violent_focus"),
      c("environment", "meeting", "election_point"),
      c("tag_variable", "tag_value", "contested"),
      c("attribute", "attribute_values")
    )
  } else if (serialization_type=="coding"){
    all_columns <- c(
      c("geo_relevant", "time_relevant", 
        "electoral_nature", "violent_nature", "violent_focus"),
      c("environment", "meeting", "election_point"),
      c("tag_variable", "tag_value", "contested"),
      c("attribute", "attribute_values")
    )
  }
  relevant_cols<- all_columns[all_columns %in% names(df_row)]
  relevant_cols<-unique(relevant_cols)
  paste(df_row[,relevant_cols], collapse="")
}



