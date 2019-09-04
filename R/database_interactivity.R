keyring_evp_available<-function(username="not implemented"){

  #' Checks if an evp password is available.
  #'
  #' Checks if an evp password is available on this computer.
  #' @export
  these_keys<-keyring::key_list()
  needed_key<- dplyr::filter(these_keys, service=="evp", username=="data_writer")
  if(nrow(needed_key)==0){
    available <- FALSE
  } else if(nrow(needed_key)>0){
    available <- TRUE
  } else {
    warning(paste0("There are an unexpected number of entries in the keyring: ", nrow(needed_key)))
    available <- FALSE
  }

  return(available)
}

manage_dbcons <- function(host="coders.victorianelectionviolence.uk", username="data_writer", dbname="evp", port=3306){
  #' Returns a connection to the election violence database
  #'
  #' This function is primarily for use within other functions.
  #' This is now a wrapper function around pool::dbPool to handle connections to the database.
  #' Database password is obtained from the keyring if there is a password for the database in the keyring. Otherwise the user is prompted for the database password.
  #' @export
  if(keyring_evp_available()){
    password_method <- "keyring"
  } else {
    password_method <- "ask"
  }

  if (password_method=="keyring"){
    password <- keyring::key_get(dbname, username)
  } else if (password_method=="config"){
    stop("config password method no longer supported")
    # password<-config::get()$datawarehouse$pwd
  } else{
    password<- rstudioapi::askForPassword("Database password")
  }
  env<-globalenv()

  poolproblem<-!exists(".evp_db_pool", envir=env) | class(env$.evp_db_pool)[1]!="Pool"

  if(poolproblem){
    .evp_db_pool<<- pool::dbPool(drv=RMySQL::MySQL(),
                                 host=host,
                                 dbname = dbname,
                                 port=port,
                                 username = username,
                                 password = password)

  }

 env$.evp_db_pool
}
evdb_connect <- function(host="coders.victorianelectionviolence.uk", user="data_writer", dbname="evp", port=3306, password_method="ask"){
  #' Set connection to election violence database
  #'
  #' @param host Location of database host.
  #' @param user Username for accessing the database.
  #' @param dbname Name of database.
  #' @param port Value of port connection.
  #' @param password_method Method of obtaining database password for specified user.
  #' @return The connection to the election violence database.
  #' @export
  #' @examples
  #' # for default connection to the database
  #' con <- evdb_connect()
  #' # using alternative method of entering password information
  #' con <- evdb_connect(password_method="keyring")

  if (password_method=="keyring"){
    password <- keyring::key_get(dbname, user)
  } else if (password_method=="config"){
    stop("config password method no longer supported")
    # password<-config::get()$datawarehouse$pwd
  } else{
    password<- rstudioapi::askForPassword("Database password")
  }
  DBI::dbConnect(RMySQL::MySQL(),
                 host = host,
                 user = user,
                 port=port,
                 password = password,
                 dbname=dbname
  )
}

build_where_condition <- function (col_name, vals, existing_condition, existing_interpolate_list){
  #'Constructs where conditions for querying database.
  #'
  #'@param col_name The column name for there where condition.
  #'@param vals The value or values to include in the query.
  #'@param existing_condition An existing SQL query to append this where condition to.
  #'@param existing_interpolate_list Existing interpolation list to add this interpolation list to.
  #'@export
  if (length(vals)==0){
    warning("zero length value condition in build_where_condition. Query will return empty dataset.")
    return(list(condition= paste(existing_condition, " WHERE 1=2"), interpolate_list=existing_interpolate_list))
  }
  if (identical(vals, "all")|identical(vals, "%")){
    return(list(condition= existing_condition, interpolate_list=existing_interpolate_list))
  } else {
    where_pos<- stringr::str_locate(existing_condition, "WHERE")
    if (is.na(where_pos[1])){
      existing_condition <- paste0(existing_condition, " WHERE ")
    } else {
      existing_condition <- paste0(existing_condition, " AND ")
    }
  }
  expandout <- expand_out_ors (col_name, vals)
  condition <- paste0(existing_condition, expandout[["condition"]])
  interpolate_list <- c(existing_interpolate_list, expandout[["interpolate_list"]])

  list(condition=condition, interpolate_list=interpolate_list)
}
expand_out_ors <- function(col_name, vals){
  res<-list()
  interpolate_list <- list()
  safecol_name<-gsub("\\.", "", col_name)
  for (this_n in seq_along(vals)){
    if (this_n==1){
      this_condition = "("
    } else {
      this_condition <- paste0(this_condition, " OR ")
    }
    this_condition <- paste0(this_condition, col_name, "=?", safecol_name, this_n)
    interpolate_list[[paste0(safecol_name, this_n)]] <- vals[this_n]
  }
  this_condition <- paste0(this_condition, ")")
  res[["condition"]] <- this_condition
  res[["interpolate_list"]]<-interpolate_list
  res
}

get_allocation_connect_to_docs <- function(user_id = "all", allocation_type="all", document_id="all", user_doc_id="all", include_ocr = FALSE){
  #' Get the allocation of articles to users (the unit of coding), with connected information from documents table and search tables
  #' @param user_id The user_id to check in the database.
  #' @param document_id The set of document_ids to include in the allocation
  #' @param include_ocr Should the OCR of the document be included in the results.
  #' @return dataframe of the document allocations to the user.
  #' @export

  con <- manage_dbcons()
  this_sql <-"SELECT ud.id as user_doc_id, ud.allocation_date, ud.allocation_type, ud.coding_complete, ud.article_type, ud.geo_relevant, ud.time_relevant, ud.electoral_nature, ud.electoralviolence_nature, ud.violence_nature, ud.legibility, ud.comment_docinfo, ud.document_id, ud.allocated_by, ud.user_id, ud.status, ud.recommend_qualitative, ud.difficulty_ranking, ud.ideal_coding_comments, ud.score, ud.last_updated, d.doc_title, d.pdf_location, d.pdf_page_location, d.candidate_document_id, d.publication_title, d.publication_location, c.type, c.status as cand_doc_status, c.page, d.publication_date, d.word_count, c.g_status, c.status_writer, c.url, u.first_name as coder_first_name, u.last_name as coder_last_name, u.username as coder_username"

  if(include_ocr){
    this_sql <- paste0(this_sql, ", d.ocr")
  }
  this_sql <- paste(this_sql, "FROM `portal_userdocumentallocation` ud LEFT JOIN `portal_document` d ON ud.document_id = d.id LEFT JOIN `auth_user` u ON u.id=ud.user_id LEFT JOIN `portal_candidatedocument` c ON d.candidate_document_id=c.id") # base query

  res<-build_where_condition("user_id", user_id, this_sql, NULL)
  res<-build_where_condition("allocation_type", allocation_type, res[[1]], res[[2]])
  res<-build_where_condition("document_id", document_id, res[[1]], res[[2]])
  res<-build_where_condition("ud.id", user_doc_id, res[[1]], res[[2]])

  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]

  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)

  allocation<-DBI::dbGetQuery(con, this_safe_sql)


  allocation
}


get_allocation <- function(user_id = "all", allocation_type="all", document_id="all", user_doc_id="all"){
  #' Get the articles currently allocated to the user.
  #' @param user_id The user_id to check in the database.
  #' @param document_id The set of document_ids to include in the allocation
  #' @return dataframe of the document allocations to the user.
  #' @export

  con <- manage_dbcons()
  this_sql <-"SELECT * FROM portal_userdocumentallocation" # base query

  res<-build_where_condition("user_id", user_id, this_sql, NULL)
  res<-build_where_condition("allocation_type", allocation_type, res[[1]], res[[2]])
  res<-build_where_condition("document_id", document_id, res[[1]], res[[2]])
  res<-build_where_condition("id", user_doc_id, res[[1]], res[[2]])

  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)

  allocation<-DBI::dbGetQuery(con, this_safe_sql)


  allocation
}

user_allocated_fromset <- function(user_id, set, not_allocated=FALSE){
  #' Which items allocated (or not allocated) to user
  #'
  #' Returns the elements of a set of documents which are allocated (or not allocated) to a user.
  #' @param user_id The user id to check in the database.
  #' @param set The set of documents to check.
  #' @param not_allocated If FALSE (default) returns the items allocated to the user, if TRUE returns items NOT allocated to the user.
  #' @return A vector numbers representing the document ids from the set which are allocated to the user (or which are not allocated to the user if not_allocated=TRUE).
  #' @examples
  #' # returns the trainingset documents allocated to user 1
  #' user_allocated_fromset(1, set=define_trainingset())
  #'
  #' # returns the trainingset documents not allocated to user 1
  #' user_allocated_fromset(1, set=define_trainingset(), not_allocated=TRUE)
  #'
  #' @export


  user_allocation <- get_allocation(user_id)
  training_allocation <- dplyr::filter(user_allocation, allocation_type=="training")

  doc_ids <- set[set %in% user_allocation$document_id] # document ids which are in training set but not allocated to the user

  doc_ids_needed <- set[!set %in% user_allocation$document_id] # document ids which are in training set but not allocated to the user
   if (not_allocated){
    res<-doc_ids_needed
  } else {
    res<-doc_ids
  }
  res
}


get_user <- function(user_id){
  #' Gets user details.

  #' @param user_id The user id.
  #' @export

  con <- manage_dbcons()

  this_sql<-"SELECT * FROM auth_user"

  res<-build_where_condition("id", user_id, this_sql, NULL)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)

  users<-DBI::dbGetQuery(con, this_safe_sql)


  users
}

get_status<-function(user_id){
  con <- manage_dbcons()

  this_sql<-"SELECT * FROM auth_user WHERE id=?user_id ;"

  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql, user_id=user_id)
  users<-DBI::dbGetQuery(con, this_safe_sql)


  users

}

get_document<-function(document_id, url="all"){
  #' Returns the document table filtered by document id
  #'
  #' @param document_id Document id or ids to filter by.
  #' @param url url to filter by.
  #' @export

  con <- manage_dbcons()

  this_sql<-"SELECT * FROM portal_document" # base query

  res<-build_where_condition("id", document_id, this_sql, NULL)
  res<-build_where_condition("url", url, res$condition, res$interpolate_list)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  documents<-DBI::dbGetQuery(con, this_safe_sql)



  documents
}

get_candidate_documents<-function(cand_document_id="all", url="all", status="all", g_status="all", include_ocr=TRUE){
  #' Returns the candidate document table filtered by candidate document id
  #'
  #'Note that excluding the ocr from the download will significantly reduce the time for the query to execute.
  #' @param cand_document_id Candidate document id or ids to filter by.
  #' @param url document url or urls to filter by.
  #' @param status document classification status to filter by.
  #' @param include_ocr should the ocr be downloaded.
  #' @export

  con <- manage_dbcons()
  if(include_ocr){
    this_sql<-"SELECT * FROM portal_candidatedocument" # base query with ocr
  } else {

    this_sql<-"SELECT id, title, url, description, publication_title, publication_location, type, status, page, publication_date, word_count, g_status, status_writer FROM portal_candidatedocument" # base query without ocr

  }

  res<-build_where_condition("id", cand_document_id, this_sql, NULL)
  res<-build_where_condition("url", url, res$condition, res$interpolate_list)
  res<-build_where_condition("status", status, res$condition, res$interpolate_list)
  res<-build_where_condition("g_status", g_status, res$condition, res$interpolate_list)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  cand_documents<-DBI::dbGetQuery(con, this_safe_sql)

  cand_documents
}
get_archivesearchsummaryonly<-function(archivesearchsummaryonly_id="all"){
  #' Returns the archivesearchessummaryonly table filtered by id
  #'
  #' @param event_report_id Event report id or ids to filter by.
  #' @export

  con <- manage_dbcons()
  this_sql<-"SELECT * FROM portal_archivesearchsummaryonly" # base query

  res<-build_where_condition("id", archivesearchsummaryonly_id, this_sql, NULL)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  archivesearchessummaryonly<-DBI::dbGetQuery(con, this_safe_sql) %>%
    tibble::as.tibble()

  archivesearchessummaryonly
}
get_event_report<-function(event_report_id="all", user_doc_id="all"){
  #' Returns the event_report table filtered by event report id
  #'
  #' @param event_report_id Event report id or ids to filter by.
  #' @export

  con <- manage_dbcons()
  this_sql<-"SELECT id as event_report_id, event_type, environment, event_start, event_end, comment_events, event_id, user_doc_id, summary, meeting, election_point, event_timeframe_quantifier,  autodetected_cluster_id FROM portal_eventreport" # base query

  res<-build_where_condition("id", event_report_id, this_sql, NULL)
  res<-build_where_condition("user_doc_id", user_doc_id, res$condition, res$interpolate_list)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  eventreport<-DBI::dbGetQuery(con, this_safe_sql)

  eventreport
}

get_tag<-function(tag_id="all", event_report_id="all"){
  #' Returns the tag table filtered by either tag_id (primary key) or event report id
  #'
  #' @param tag_id Primary key from tag table
  #' @param event_report_id Event report id or ids to filter by.
  #' @export

  con <- manage_dbcons()
  this_sql<-"SELECT id as tag_id, tag_table, tag_variable, tag_value, event_report_id, comment as comment_tags, contested as contested_tags, proximity_relative FROM portal_tag" # base query

  res<-build_where_condition("id", tag_id, this_sql, NULL)
  res<-build_where_condition("event_report_id", event_report_id, res$condition, res$interpolate_list)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  tag<-DBI::dbGetQuery(con, this_safe_sql)

  tag
}

get_attribute<-function(attribute_id="all", tag_id="all"){
  #' Returns the tag table filtered by either attribute_id (primary key) or tag_id
  #'
  #' @param attribute_id attribute id or ids to filter by.
  #' @param tag_id Foreign key from tag table
  #' @export

  con <- manage_dbcons()
  this_sql<-"SELECT id as attribute_id, attribute, attribute_value, tag_id, contested as contested_attribute FROM portal_attribute" # base query

  res<-build_where_condition("id", attribute_id, this_sql, NULL)
  res<-build_where_condition("tag_id", tag_id, res$condition, res$interpolate_list)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  attribute<-DBI::dbGetQuery(con, this_safe_sql)

  attribute
}

users_to_actual<-function(user_id){
  #' Restrict a vector to user ids actually existing in the the database.
  #' @export

  users_from_db<-get_user(user_id)$id
  actual_users <- user_id[user_id %in% users_from_db]
  if (length(actual_users)<length(user_id)){
    warning(paste0("Some user ids not found in database and will not be used: ", paste0(user_id[!user_id %in% actual_users], collapse=", ")))
  }

  actual_users
}

documents_to_actual<-function(document_id){
  #' Restrict a vector to document ids actually existing in the database.
  #' @export

  docs_from_db<-get_document(document_id)$id
  actual_docs <- document_id[document_id %in% docs_from_db]
  if (length(actual_docs)<length(document_id)){
    warning(paste0("Some document ids not found in database and will not be used: ", paste0(document_id[!document_id %in% actual_docs], collapse=", ")))
  }

  actual_docs
}

get_coding <- function(include_ocr=FALSE){
  #' Download all the coding on the database
  #' @param include_ocr Should results include the full ocr of the documents (will slow the download).
  #' @export
  user_docs <- get_allocation_connect_to_docs(include_ocr=include_ocr)
  event_reports <- get_event_report()
  tags <- get_tag()
  attributes <- get_attribute()

  res <- list(user_docs=user_docs,
       event_reports = event_reports,
       tags=tags,
       attributes = attributes)
  class(res) <-"evp_coding_download"

  res
}

is_evp_coding_download <- function(coding_download){
  #' Check if object is an evp coding download.
  #' @param coding_download object to check
  #' @return logical
  #' @export
  if(class(coding_download)=="evp_coding_download"){
    res <- TRUE
  } else {
    res <- FALSE
  }
  res
}

download_to_superwide <- function (evp_coding_download, coding_mode_only=TRUE, coding_complete_only=TRUE){
  #' Transforms the downloaded coding to 'superwide' format
  #'
  #' Superwide format creates one row for every event_report. It then creates a separate variable for every table, variable, value combination and counts the number of instances associated with event report. For example the event report has one Conservative perpetrator then the superwide record variable will be actors_perpetrator_cons_tor_union and the superwide value for that variable will be 1.
  #' Currently the locations are processed with a hack - choosing the first location of each type associated with an event report.
  #'
  #' @param evp_coding_download The result of a \code{get_coding()}.
  #' @param coding_mode_only Whether to include only results from coding undertaken in by users in 'coding mode'.
  #' @param coding_complete_only Whether to include only results where coding is marked as complete.
  #'
  #' @export

  if(!is_evp_coding_download(evp_coding_download)){
    stop("not a valid coding results list")
  }

  user_docs <- evp_coding_download[["user_docs"]]
  event_reports <- evp_coding_download[["event_reports"]]

  tags <- evp_coding_download[["tags"]]

  location_tags <- tags %>%
    dplyr::filter(tag_table == "location")

  processed_locations <- process_locations(location_tags)

  non_location_tags <- tags %>%
    dplyr::filter(tag_table != "location") %>%
    tidyr::unite(var_complex, tag_table, tag_variable, tag_value)

  non_location_tags_wide <- non_location_tags %>%
    dplyr::group_by(event_report_id, var_complex) %>%
    dplyr::summarise(value=n()) %>%
    tidyr::spread(var_complex, value, fill = 0 )

  attributes <- evp_coding_download[["attributes"]]

  attributes_wide <- attributes %>%
    dplyr::left_join(dplyr::select(tags, event_report_id, tag_id, tag_table, tag_variable, tag_value), by=c("tag_id"="tag_id")) %>%
    tidyr::unite(attribute_complex, tag_table, tag_variable, tag_value, attribute, attribute_value) %>%
    dplyr::group_by(event_report_id, attribute_complex) %>%
    dplyr::summarize(value=n()) %>%
    tidyr::spread(attribute_complex, value, fill=0)

  superwide <- event_reports %>%
    dplyr::inner_join(user_docs, by="user_doc_id") %>%
    dplyr::left_join(non_location_tags_wide, by="event_report_id") %>%
    dplyr::left_join(attributes_wide, by="event_report_id") %>%
    dplyr::left_join(processed_locations, by="event_report_id") %>%
    tibble::as.tibble()

  if(coding_mode_only){
    superwide <- dplyr::filter(superwide, allocation_type=="coding")
  }

  if(coding_complete_only){
    superwide <- dplyr::filter(superwide, coding_complete==1)
  }

  superwide
}

is_location_tags <- function(location_tags){
  #' Checks if an object is a location_tags table
  #' @param location_tags object to check
  #' @export

  if(is.data.frame(location_tags)){
    right_names <- sum(names(location_tags) %in% c("tag_id", "tag_table", "tag_variable", "tag_value", "event_report_id", "comment_tags", "contested_tags", "proximity_relative"))==8

    if("tag_table" %in% names(location_tags)){
      all_locations <- sum(location_tags$tag_table=="location")==nrow(location_tags)
    } else {
      all_locations <- FALSE
    }

    res <- right_names & all_locations

  } else {
    res <- FALSE
  }


  res
}
process_locations <- function(location_tags){
  #' Creates a wide version of the locations associated with event reports. Currently based on a hack.
  #' @param location_tags a table of tags (filtered: table=="location) containing only locations.
  #' @export
  #' @importFrom magrittr %>%

  if(!is_location_tags(location_tags)){
    stop("Not a location_tag")
  }
  processed_locationa <- location_tags %>%
    dplyr::select(-comment_tags, -contested_tags, -tag_id, -tag_table) %>%
    dplyr::mutate(tag_variable=ifelse(tag_variable=="", "unspecified_location_level", tag_variable)) %>%
    dplyr::group_by(event_report_id, tag_variable) %>%
    dplyr::arrange(tag_value, proximity_relative, .by_group=TRUE) %>%
    dplyr::slice(1)

  locs<-processed_locationa%>%
    select(-proximity_relative) %>%
    tidyr::spread(tag_variable, tag_value)

  pr<-processed_locationa%>%
    ungroup() %>%
    select(-tag_value) %>%
    mutate(tag_variable=paste0(tag_variable, "_proximity_relative")) %>%
    tidyr::spread(tag_variable, proximity_relative, fill=0)
  processed_location <- left_join(locs, pr, by="event_report_id")

  processed_location
}

assign_coding_to_environment<- function(evp_coding_download, restrict_to_coding_complete = TRUE, restrict_to_coding_mode = TRUE){
  #' Assign the results of a get_coding download to the global environment
  #' @param evp_coding_download The result from executing the get_coding() command.
  #' @param restrict_to_coding_complete Should the data including only records where coding is tagged as complete?
  #' @param restrict_to_coding_mode Should the data include on the records where coders were in coding mode?
  #' @export
  #'

  if(is_evp_coding_download(evp_coding_download)){

    if(restrict_to_coding_complete){
      evp_data[["user_docs"]] <-  dplyr::filter(evp_data[["user_docs"]], coding_complete==1)
    }
    if(restrict_to_coding_mode){
      evp_data[["user_docs"]] <-  dplyr::filter(evp_data[["user_docs"]], allocation_type=="coding")
    }

    evp_data[["event_reports"]] <-  dplyr::filter(evp_data[["event_reports"]], user_doc_id %in% evp_data[["user_docs"]]$user_doc_id)
    evp_data[["tags"]] <-  dplyr::filter(evp_data[["tags"]], event_report_id %in% evp_data[["event_reports"]]$event_report_id)
    evp_data[["attributes"]] <-  dplyr::filter(evp_data[["attributes"]], tag_id %in% evp_data[["tags"]]$tag_id)


    location<- evp_coding_download[["tags"]]
    location <- dplyr::filter(location, tag_table == "location")
    location <- tibble::as.tibble(location)
    processed_locations <- process_locations(location)


    actors<- evp_coding_download[["tags"]]
    actors <- dplyr::filter(actors, tag_table == "actors")
    actors <- tibble::as.tibble(actors)

    evp_coding_download[["location"]] <- location
    evp_coding_download[["processed_locations"]] <- processed_locations

    for (i in 1:length(evp_coding_download)) {
      assign(names(evp_coding_download)[i], tibble::as.tibble(evp_coding_download[[i]]), envir=globalenv())
    }
  } else {
    stop("not a valid coding results list")
  }
}

get_user_mode <- function(user_id="all"){
  #' Returns table of current user modes
  #'
  #' Returns the table of current user modes.
  #' @param user_id Id of the user whose mode to check or a vector of user ids. The default ("all") selects all users.
  #' @export

  con <- manage_dbcons()
  this_sql<-"SELECT * FROM portal_userprofile"

  res<-build_where_condition("user_id", user_id, this_sql, NULL)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)

  profiles<-DBI::dbGetQuery(con, this_safe_sql)


  profiles
}

get_archivesearches<-function(archive_search_id="all"){
  #' Returns the table of searches which have been passed to the crawler.
  #'
  #' @param archive_search_id archive_search id or ids to filter by.
  #' @export

  con <- manage_dbcons()

  this_sql<-"SELECT * FROM portal_archivesearch" # base query

  res<-build_where_condition("id", archive_search_id, this_sql, NULL)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  archivesearch<-DBI::dbGetQuery(con, this_safe_sql)



  archivesearch
}

get_archivesearchresults<-function(archivesearchresults_id="all", archive_search_id="all"){
  #' Returns the crawler search table
  #'
  #' @param archivesearch_id archive_search id or ids to filter by.
  #' @export

  con <- manage_dbcons()

  this_sql<-"SELECT * FROM portal_archivesearchresult" # base query

  res<-build_where_condition("id", archivesearchresults_id, this_sql, NULL)
  res<-build_where_condition("archive_search_id", archive_search_id, res$condition, res$interpolate_list)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  archivesearch<-DBI::dbGetQuery(con, this_safe_sql)



  archivesearch
}
set_user_mode <- function (user_id, new_mode){
  #' Sets user mode
  #'
  #' Sets the users mode to a new value (training, testing, coding, checking, ideal).
  #' @param user_id Id of the user whose mode to set or a vector of user ids.
  #' @param new_mode The new mode (one of: training, testing, coding, checking, ideal) to set as the users mode(s). For multiple users either a single value should be given to be assigned to all identified users, or a vector of values should be given of the same length as the vector of user_ids.
  #' @export

  con <- manage_dbcons()
  #print(con)

  if(length(new_mode)==1) new_mode <- rep(new_mode, length(user_id))
  if(length(new_mode)!=length(user_id)) warning("length (mode) not equal to length(user_id))")

  current_modes <- get_user_mode(user_id) %>%
    mutate(new_mode=new_mode)
  for (this_user in user_id){
    this_data <- current_modes %>%
      filter(user_id==this_user) %>%
      top_n(1)
    this_id<-this_data$id
    this_mode<-this_data$new_mode

    this_sql<-"UPDATE portal_userprofile set mode=?new_mode WHERE id=?id"
    this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                       id = this_id,
                                       new_mode=this_mode)
    documents<-DBI::dbGetQuery(con, this_safe_sql)



  }
}

get_useractivitylogentry<-function (useractivitylogentry_id="all", action="all", user_id="all", allocation_id="all"){
  #' Get user logged activity
  #'
  #' Gets the log of user activity on the database relating to a user or an allocation or an action type.
  #' @param useractivitylogentry_id primary key of user activity table of interest.
  #' @param user_id Id of the user whose activity is of interest.
  #' @param action action types of interest.
  #' @param allocation_id allocation_ids of interest.
  #' @export

  con <- manage_dbcons()
  #print(con)


  this_sql<-"SELECT * FROM portal_useractivitylogentry" # base query

  res<-build_where_condition("id", useractivitylogentry_id, this_sql, NULL)
  res<-build_where_condition("action", action, this_sql, NULL)
  res<-build_where_condition("user_id", user_id, res$condition, res$interpolate_list)
  res<-build_where_condition("allocation_id", allocation_id, res$condition, res$interpolate_list)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  useractivity<-DBI::dbGetQuery(con, this_safe_sql)

  useractivity
  }


killDbConnections <- function () {
  #' Kills all current database connections
  #'
  #' @export

  all_cons <- DBI::dbListConnections(RMySQL::MySQL())

  print(all_cons)

  for(con in all_cons) DBI::dbDisconnect(con)

  print(paste(length(all_cons), " connections killed."))

}


show_evp_tables <- function(){
  #' Shows the tables in the database.
  #' @export
  con <- manage_dbcons()

  this_sql<-"SHOW TABLES;" # base query
  db_tables<-DBI::dbGetQuery(con, this_sql)

  db_tables
}
