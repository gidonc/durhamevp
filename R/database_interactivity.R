
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
  #' con <- evdb_connect(password_method="config")

  if (password_method=="keyring"){
    password <- keyring::key_get(dbname, user)
  } else if (password_method=="config"){
    password<-config::get()$datawarehouse$pwd
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
  for (this_n in seq_along(vals)){
    if (this_n==1){
      this_condition = "("
    } else {
      this_condition <- paste0(this_condition, " OR ")
    }
    this_condition <- paste0(this_condition, col_name, "=?", col_name, this_n)
    interpolate_list[[paste0(col_name, this_n)]] <- vals[this_n]
  }
  this_condition <- paste0(this_condition, ")")
  res[["condition"]] <- this_condition
  res[["interpolate_list"]]<-interpolate_list
  res
}

get_allocation <- function(con=connect, user_id = "all", allocation_type="all", document_id="all"){
  #' Get the articles currently allocated to the user.
  #' @param con The database connection to .
  #' @param user_id The user_id to check in the database.
  #' @param document_id The set of document_ids to include in the allocation
  #' @return dataframe of the document allocations to the user.
  #' @export

  this_sql <-"SELECT * FROM portal_userdocumentallocation" # base query

  res<-build_where_condition("user_id", user_id, this_sql, NULL)
  res<-build_where_condition("allocation_type", allocation_type, res[[1]], res[[2]])
  res<-build_where_condition("document_id", document_id, res[[1]], res[[2]])

  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)

  allocation<-DBI::dbGetQuery(con, this_safe_sql)

  allocation
}

user_allocated_fromset <- function(con, user_id, set, not_allocated=FALSE){
  #' Which items allocated (or not allocated) to user
  #'
  #' Returns the elements of a set of documents which are allocated (or not allocated) to a user.
  #' @param con The database connection to the election violence database.
  #' @param user_id The user id to check in the database.
  #' @param set The set of documents to check.
  #' @param not_allocated If FALSE (default) returns the items allocated to the user, if TRUE returns items NOT allocated to the user.
  #' @return A vector numbers representing the document ids from the set which are allocated to the user (or which are not allocated to the user if not_allocated=TRUE).
  #' @examples
  #' # returns the trainingset documents allocated to user 1
  #' user_allocated_fromset(con, 1, set=define_trainingset())
  #'
  #' # returns the trainingset documents not allocated to user 1
  #' user_allocated_fromset(con, 1, set=define_trainingset(), not_allocated=TRUE)
  #'
  #' @export
  user_allocation <- get_allocation(con, user_id)
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


get_user <- function(con, user_id){
  #' Gets user details.
  #' @param con The connection to the election violence database.
  #' @param user_id The user id.
  #' @export

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

get_status<-function(con, user_id){
  this_sql<-"SELECT * FROM auth_user WHERE id=?user_id ;"

  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql, user_id=user_id)
  users<-DBI::dbGetQuery(con, this_safe_sql)

  users

}

get_document<-function(con, document_id){
  #' Returns the document table filtered by document id
  #'
  #' @param con The connection to the election violence database.
  #' @param document_id Document id or ids to filter by.
  #' @export
  this_sql<-"SELECT * FROM portal_document" # base query

  res<-build_where_condition("id", document_id, this_sql, NULL)
  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)
  documents<-DBI::dbGetQuery(con, this_safe_sql)

  documents
}

users_to_actual<-function(con, user_id){

  users_from_db<-get_user(con, user_id)$id
  actual_users <- user_id[user_id %in% users_from_db]
  if (length(actual_users)<length(user_id)){
    warning(paste0("Some user ids not found in database and will not be used: ", paste0(user_id[!user_id %in% actual_users], collapse=", ")))
  }

  actual_users
}

documents_to_actual<-function(con, document_id){

  docs_from_db<-get_document(con, document_id)$id
  actual_docs <- document_id[document_id %in% docs_from_db]
  if (length(actual_docs)<length(document_id)){
    warning(paste0("Some document ids not found in database and will not be used: ", paste0(document_id[!document_id %in% actual_docs], collapse=", ")))
  }

  actual_docs
}
