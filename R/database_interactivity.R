
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
    password <- keyring::key_get(dbname)
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

get_allocation <- function(con=connect, user_id, allocation_type="%"){
  #' Get the articles currently allocated to the user.
  #' @param con The database connection to .
  #' @param user_id The userid to check in the database.
  #' @return dataframe of the document allocations to the user.
  #' @export
  #'
  #'
  this_sql<-"SELECT * FROM portal_userdocumentallocation WHERE user_id=?user_id AND allocation_type LIKE ?allocation_type ;"

  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql, user_id=user_id, allocation_type=allocation_type)
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

  this_sql<-"SELECT * FROM auth_user WHERE id=?user_id ;"

  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql, user_id=user_id)
  users<-DBI::dbGetQuery(con, this_safe_sql)

  users
}
