
get_data_for_comparison <- function(model_user_doc_id, user_user_doc_id){
  #' Matches elements from two document allocations (presumably different codings of the same document).
  #' Returns a list of matched event reports, tags and attributes.
  #'
  #' The similarity is measured using the Levenshtein distance between all of their fields of the serialized record.
  #' For every pair of events (and consequently, for any tags and attributes associated with these events), we use the \code{get_sorted} function to find the best match.
  #' @return A list of length 3 containing 1. dataframe of matched event report ids 2. dataframe of matched tag ids 3. dataframe of matched attributes.
  #' @param model_user_doc_id the user_doc_id of the first set of codings to compare
  #' @param user_user_doc_id the user_doc_id of the second set of codings to compare
  #' @export
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
  #'@param model_vars dataframe containing the first set of codings to be matched
  #'@param user_vars dataframe containing the second set of codings to be matched
  #'@param get_var_score the function to do the comparison
  #'@export

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

get_distance<- function(model, user, type="primitive"){
  #' Gets the Levenshtein distance between two separate rows after serializing them.
  #' @param model the first row
  #' @param user the second row
  #' @param type the type of comparison to conduction (implying which fields are included in the comparison)
  #' @export
  s_model <- serialize_row(model, type)
  s_user <- serialize_row(user, type)

  get_similarity_score(s_model, s_user)
}



serialize_row<- function(df_row, serialization_type="primitive"){
  #' Turns a database row into a string
  #' @param df_row the database row
  #' @param serialization_type defines which columns to serialize ("primitive", "testing", "coding")
  #' @export
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

get_similarity_score <- function(a, b){
  #' Gets a similarity score between two strings
  #'
  #' @param a string 1
  #' @param b string 2
  #' @return Returns a similarity score between 0.0 (completely identical) and 1.0 (completely different)
  #' @export
  #'
  init <- fuzzywuzzyR::SequenceMatcher$new(a, b)
  1 - init$ratio()
}

extract_matches <- function(){
  #' Creates tibble of all matches from the election violence database
  #'
  #' \code{extract_matches} is a wrapper function for applications of record matching.
  #' @return A tibble of all the database entries which have been matched. The columns of the tibble are as follows \code{pair_no} unique id for the user_doc allocations (i.e. identifies the two user allocations at the document allocation level. \code{case_no} a unique id at the level one up from the match (i.e. identifying the two tag_ids for attributes, identifying the two event_report ids for tags, identifying the two document allocations for event reports). \code{model_var} is the id of the first record being compared, \code{user_var} is the id of the second record being compared, \code{match_type} is the database table the match is from (values: "user_doc", "ev_report", "tags", "attributes"), \code{score} is the matching score on which the match is based.
  #' @export
  all_allocations<-durhamevp::get_allocation() %>%
    dplyr::filter(status=="COMPLETED")


  for_analysis<-all_allocations %>%
    dplyr::group_by(document_id, status=="COMPLETED") %>%
    dplyr::tally() %>%
    dplyr::filter(n>1) %>%
    dplyr::left_join(all_allocations)

  # create unique pairs of respondents

  coder_pairs <- for_analysis %>%
    dplyr::filter(!user_id %in% c(1:5, 7)) %>%
    dplyr::filter(allocation_type=="coding", status=="COMPLETED") %>%
    dplyr::group_by(document_id) %>%
    tidyr::expand(user_id, user_id) %>%
    dplyr::filter(user_id < user_id1) %>%
    tibble::rowid_to_column("pair_no")

  long_coder_pairs <- coder_pairs %>%
    tidyr::gather(which_user_id, user_id, user_id, user_id1) %>%
    dplyr::left_join(dplyr::select(all_allocations, user_id, document_id, user_doc_id=id), by = c("document_id", "user_id")) %>%
    dplyr::arrange(pair_no)

  matched_user_docs<-long_coder_pairs %>%
    dplyr::ungroup() %>%
    dplyr::select(-document_id, -user_id) %>%
    dplyr::mutate(which_user_id=ifelse(which_user_id=="user_id", "model_var", "user_var")) %>%
    tidyr::spread(which_user_id, user_doc_id) %>%
    mutate(match_type="user_doc") %>%
    left_join(coder_pairs, by="pair_no") %>%
    dplyr::select(-document_id)


  get_first<-function(x){
    x[[1]]
  }

  get_second<-function(x){
    x[[2]]
  }

  get_third<-function(x){
    x[[3]]
  }

  matched_nested<-long_coder_pairs %>%
    dplyr::ungroup() %>%
    dplyr::select(-document_id, -user_id) %>%
    tidyr::spread(which_user_id, user_doc_id) %>%
    #dplyr::slice(1:40) %>%
    tidyr::nest(pair_no) %>%
    dplyr::mutate(cal=purrr::map2(user_id, user_id1, get_data_for_comparison)) %>%
    dplyr::mutate(ev_report=purrr::map(cal, get_first), tags=purrr::map(cal, get_second), attributes=purrr::map(cal, get_third)) %>%
    gather(match_type, match_data, ev_report, tags, attributes) %>%
    unnest(data) %>%
    dplyr::select(-cal) %>%
    unnest()

  matched_all <- dplyr::bind_rows(matched_user_docs, matched_nested) %>%
    arrange(pair_no) %>%
    tibble::rowid_to_column("case_no") %>%
    dplyr::select(-user_id, -user_id1)

  matched_all
}


find_agreement_from_matches<- function (matched_all, match_type, drop_cols=c("score", "report_type", "process_point", "event_type", "event_id", "comment", "election_date", "summary", "contested", "tag_id", "event_report_id", "proximity_relative", "id", "allocated_by", "allocation_date", "allocation_type", "assigned_at", "coding_complete", "difficulty_ranking", "document_id", "score.x", "score.y", "article_date_man_verify", "ideal_coding_comments", "last_updated", "status", "user_id", "event_timeframe_quantifier", "event_end", "event_duration", "user_doc_id")){
  #' Checks agreement on variables from dataframe of matched records.
  #'
  #' @param matched_all The dataframe of matches
  #' @param match_type The type of match (i.e. the database table the match comes from "user_doc", "ev_report", "tags", "attributes")
  #' @param drop_cols Names of columns to exclude from the comparison
  #' @export
  #'

  if(match_type=="user_doc"){
    the_cases <- filter(matched_all, match_type=="user_doc")
    the_matches <- durhamevp::get_allocation()
  } else if (match_type=="ev_report"){
    the_cases <- filter(matched_all, match_type=="ev_report")
    the_matches <- durhamevp::get_event_report()
  } else if (match_type=="tags"){
    the_cases <- filter(matched_all, match_type=="tags")
    the_matches <- durhamevp::get_tag()
  } else if (match_type=="attributes"){
    the_cases <- filter(matched_all, match_type=="attributes")
    the_matches <- durhamevp::get_attribute()
  }
  the_cases2 <- gather(the_cases, which_var, id, -case_no, -pair_no, -match_type, -score)
  the_cases2 <- dplyr::left_join(the_cases2, the_matches, by="id")
  the_cases2 <- the_cases2[,!names(the_cases2) %in% drop_cols]

  if(match_type %in% c("user_doc", "ev_report")){
    the_agreement<- tidyr::gather(the_cases2, variable, value, -pair_no, -case_no, -match_type, -which_var)
    the_agreement <- tidyr::spread(the_agreement, which_var, value)
  } else if (match_type=="tags"){
    the_agreement<- dplyr::rename(the_cases2, value=tag_value, variable=tag_variable)
    the_agreement <- tidyr::spread(the_agreement, which_var, value)
  } else if (match_type=="attributes"){
    the_agreement<- dplyr::rename(the_cases2, value=attribute_value, variable=attribute)
    the_agreement <- tidyr::spread(the_agreement, which_var, value)
  }
  the_agreement <- dplyr::mutate(the_agreement, agree = model_var==user_var )
}
