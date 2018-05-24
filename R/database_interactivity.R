
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
  #' con <- evbd_connect(password_method="config")

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
  #' Returns the elements of a set of documents allocated (or not allocated) to a user.
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


assign_article_to_user <- function (con, document_id, user_id, allocation_date, allocation_type, allocated_by, status, coding_complete=0){
  #' Assigns an article to a user.
  #'
  #' @param con The database connection to the election violence database.
  #' @param document_id Id of the document to be assigned.
  #' @param user_id Id of the user the document is to be assigned to.
  #' @param allocation_date Date allocation made (usually today).
  #' @param allocation_type Type of allocation (training, testing, coding, checking, ideal).
  #' @param status Status of document coding (generally 'New' for newly assigned documents).
  #' @export

  allocated_at <- as.character(Sys.time())
  this_sql<-"INSERT INTO portal_userdocumentallocation (document_id, user_id, allocation_date, allocation_type, allocated_by, status, coding_complete, article_type, geo_relevant, time_relevant, electoral_nature, violent_nature, violent_focus, legibility, comment, recommend_qualitative, difficulty_ranking, ideal_coding_comments, score, assigned_at, last_updated) VALUES (?document_id, ?user_id, ?allocation_date, ?allocation_type, ?allocated_by, ?status, ?coding_complete, '' , '', '', '', '', '', '', '', '', -1, '', -1, ?allocated_at, ?allocated_at) ;"

  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     document_id=document_id,
                                     user_id=user_id,
                                     allocation_date=allocation_date,
                                     allocation_type=allocation_type,
                                     allocated_by=allocated_by,
                                     status=status,
                                     coding_complete=coding_complete,
                                     allocated_at=allocated_at)
  DBI::dbSendStatement(con, this_safe_sql)
  return(NULL)
}

assign_set <- function(con, user_id, set, allocation_type, allocated_by = "assign_set", status="NEW"){
  #' Assigns a whole set of articles to a user (articles which are already allocated to a user are not allocated again).
  #' @param con The connection to the election violence database.
  #' @param user_id The user id.
  #' @param set A vector which contains the document ids of the articles to be allocated (generally the test set of articles and the training set of articles).
  #' @param allocation_type The type of the allocation in the database (one of testing, training, coding, checking and ideal)
  #' @param allocated_by The person (user_id) or the function which performed the allocation
  #' @export

  # restrict assignment of set to items not already allocated to the user
  items_needed <- user_allocated_fromset(con, user_id, set=set, not_allocated = TRUE)



  for (document_id in items_needed){
    assign_article_to_user(con,
                           document_id=document_id,
                           user_id=user_id,
                           allocation_date=as.character(Sys.Date()),
                           allocation_type=allocation_type,
                           allocated_by=allocated_by,
                           status=status)
  }
  completion_string <- paste0(allocated_by, " allocated ", length(items_needed), " items to user ", user_id, ".")
  if(length(items_needed)<length(set)){
    completion_string<-paste0(completion_string, " ", length(set)-length(items_needed), " items (of ", length(set), ") were already allocated ")
  }
  completion_string
}

assign_testset_to_user<-function(con, user_id, testset=define_testset(), allocation_type="testing", allocated_by="assign_testset_to_user") {
  assign_set(con=con, user_id=user_id, set=testset, allocation_type=allocation_type, allocated_by=allocated_by)
}


assign_trainingset_to_user <- function(con, user_id, trainingset=define_trainingset(),
                                       allocation_type="training", allocated_by="assign_trainingset_to_user"){
  #' Assigns the training set to user (articles which are already allocated to a user are not allocated again).
  #' @param con The connection to the election violence database.
  #' @param user_id The user id.
  assign_set(con=con, user_id=user_id, set=trainingset, allocation_type=allocation_type, allocated_by=allocated_by)
}

assign_initalsets_to_users <- function(con, user_ids){
  #' Assigns the training set and test sets to a user or set of users (articles which are already allocated to a user are not allocated again).
  #' @param con The connection to the election violence database.
  #' @param user_id Either a single user id or a vector of user ids.
  #' @usage
  #'
  #' # to assign training set and test set to user 3
  #' assign_initialsets_to_users(con, 3)
  #'
  #' # to asssing training set and test set to users 3,5, and 6
  #' assing_initalsets_to_users(con, c(3, 5, 6))
  #'
  #' @export

  for (user_id in user_ids){
    print(assign_testset_to_user(con, user_id))
    print(assign_trainingset_to_user(con, user_id))
  }
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
