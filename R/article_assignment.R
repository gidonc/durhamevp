
assign_article_to_user <- function (con, document_id, user_id, allocation_type, allocated_by, allocation_date=as.character(Sys.Date()), status="NEW", coding_complete=0){
  #' Assign article to a user.
  #'
  #' \code{assign_article_to_user} assigns a specific article or set of articles to a specific user or set of users. Other document assignment functions are convenience wrappers around this funciton with arguments set appropriately.
  #' @param con The database connection to the election violence database.
  #' @param document_id Id of the document to be assigned or a vector of document ids.
  #' @param user_id Id of the user the document is to be assigned to or a vector of user ids.
  #' @param allocation_type Type of allocation (training, testing, coding, checking, ideal).
  #' @param allocation_date Date allocation made (usually today).
  #' @param status Status of document coding (generally 'NEW' for newly assigned documents).
  #' @export


  allocated_at <- as.character(Sys.time())
  for (this_user_id in user_id){
    for (this_document_id in document_id){
      this_sql<-"INSERT INTO portal_userdocumentallocation (document_id, user_id, allocation_date, allocation_type, allocated_by, status, coding_complete, article_type, geo_relevant, time_relevant, electoral_nature, violent_nature, violent_focus, legibility, comment, recommend_qualitative, difficulty_ranking, ideal_coding_comments, score, assigned_at, last_updated) VALUES (?this_document_id, ?this_user_id, ?allocation_date, ?allocation_type, ?allocated_by, ?status, ?coding_complete, '' , '', '', '', '', '', '', '', '', -1, '', -1, ?allocated_at, ?allocated_at) ;"

      this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                         this_document_id=this_document_id,
                                         this_user_id=this_user_id,
                                         allocation_date=allocation_date,
                                         allocation_type=allocation_type,
                                         allocated_by=allocated_by,
                                         status=status,
                                         coding_complete=coding_complete,
                                         allocated_at=allocated_at)
      DBI::dbSendStatement(con, this_safe_sql)


    }
  }

  return(NULL)
}

assign_set <- function(con, user_id, set, allocation_type, allocated_by = "assign_set", status="NEW"){
  #' Assigns set of articles to user
  #'
  #' \code{assign_set} assigns a whole set of articles to a user (articles which are already allocated to a user are not allocated again).
  #' @export
  #' @describeIn assign_article_to_user Assign set of articles to user

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
  message(completion_string)
}

assign_testset_to_user<-function(con, user_id, allocation_type="testing", allocated_by="assign_testset_to_user") {
  #' Assign test set
  #'
  #' \code{assign_testset_to_user} assigns the test set of articles to a user specified by the user id. Articles which are already allocated to a user are not allocated again.
  #' @export
  #' @describeIn assign_article_to_user Assigns testset to user or users

  testset=durhamevp::define_testset()
  assign_set(con=con, user_id=user_id, set=testset, allocation_type=allocation_type, allocated_by=allocated_by)
}


assign_trainingset_to_user <- function(con, user_id, allocation_type="training", allocated_by="assign_trainingset_to_user"){
  #' Assign training set to user
  #'
  #' \code{assign_trainingset_to_user} assigns the training set to user (articles which are already allocated to a user are not allocated again).
  #' @export
  #' @describeIn assign_article_to_user Assigns training set to user

  trainingset=define_trainingset()
  assign_set(con=con, user_id=user_id, set=trainingset, allocation_type=allocation_type, allocated_by=allocated_by)
}

assign_initalsets_to_users <- function(con, user_ids){
  #' Assigns initial article sets to user
  #'
  #' \code{assign_initalsets_to_users} assigns the training set and test sets to a user or set of users (articles which are already allocated to a user are not allocated again).

  #' @export
  #' @describeIn assign_article_to_user Assigns training and testset set to user



  for (user_id in user_ids){
    assign_testset_to_user(con, user_id)
    assign_trainingset_to_user(con, user_id)
  }
}


# For regular article assignment
# Assign a set of articles (set) randomly to users
# with a recode rate (on average)
# recode rate 0 = no recoding
# recode rate .5 = half of thing recoded
# recode rate 1 = everything recoded
# recode rate 2 = everything triple coded
# articles should not be reassigned to coders who have already have the article assigned to them


regular_random_assignment <- function (con, user_ids, set, additional_coder_rate, min_coders=1, allocation_type="coding", allocated_by="regular_random_assignment"){
  # restrict to actual articles and actual users (both must already be in the database)
  set<-documents_to_actual(con, set)
  user_ids<-users_to_actual(con, user_ids)

  assign_dat <- data.frame(matrix(ncol=2, nrow=0))
  names(assign_dat) <- c("document_id", "user_id")

  for (this_article in set){
    n_assignments = min_coders + rpois(1, additional_coder_rate)
    users_already_coding <- get_allocation(con, document_id=this_article)$user_id
    available_users <- user_ids [!user_ids %in% users_already_coding]
    if (length(available_users)< n_assignments) {
      warning(paste0("insufficient available users: for document ", this_article, " aim to code document ", n_assignments, " times with only ", length(available_users), " coder(s) available. No assignments made for this document."))
    } else {
      print(paste0("article: ", this_article))
      these_users <- sample(available_users, n_assignments)
      print(paste0("users: ", these_users))
      for (this_user in these_users){
        assign_dat <- bind_rows(assign_dat,
                                data.frame(document_id=document_id, user_id=this_user))

      }
    }


  }
  assign_dat
}
