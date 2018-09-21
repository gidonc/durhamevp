
assign_article_to_user <- function (document_id, user_id, allocation_type, allocated_by, allocation_date=as.character(Sys.Date()), status="NEW", coding_complete=0){
  #' Assign article to a user.
  #'
  #' \code{assign_article_to_user} assigns a specific article or set of articles to a specific user or set of users. Other document assignment functions are convenience wrappers around this function with arguments set appropriately.
  #' @param document_id Id of the document to be assigned or a vector of document ids.
  #' @param user_id Id of the user the document is to be assigned to or a vector of user ids.
  #' @param allocation_type Type of allocation (training, testing, coding, checking, ideal).
  #' @param allocation_date Date allocation made (usually today).
  #' @param status Status of document coding (generally 'NEW' for newly assigned documents).

  #' @export

  con <- manage_dbcons()

  allocated_at <- as.character(Sys.time())
  for (this_user_id in user_id){
    for (this_document_id in document_id){
      this_sql<-"INSERT INTO portal_userdocumentallocation (document_id, user_id, allocation_date, allocation_type, allocated_by, status, coding_complete, article_type, geo_relevant, time_relevant, electoral_nature, electoralviolence_nature, violence_nature, legibility, comment_docinfo, recommend_qualitative, difficulty_ranking, ideal_coding_comments, score,last_updated) VALUES (?this_document_id, ?this_user_id, ?allocation_date, ?allocation_type, ?allocated_by, ?status, ?coding_complete, '' , '', '', '', '', '', '', '', '', -1, '', -1, ?allocated_at) ;"

      this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                         this_document_id=this_document_id,
                                         this_user_id=this_user_id,
                                         allocation_date=allocation_date,
                                         allocation_type=allocation_type,
                                         allocated_by=allocated_by,
                                         status=status,
                                         coding_complete=coding_complete,
                                         allocated_at=allocated_at)
      #DBI::dbSendStatement(con, this_safe_sql)
      DBI::dbExecute(con, this_safe_sql)

    }
  }

  return(NULL)
}

reassign_article_to_user <- function (user_doc_id, user_id, allocation_type, allocated_by, allocation_date=as.character(Sys.Date()), status="NEW", coding_complete=0){
  #' Reassign article allocation to a new user.
  #'
  #' \code{reassign_article_to_user} reassigns and otherwise modifies a specific existing document allocation to a specific user, and otherwise modifies the allocation parmeters (updating allocation_type, allocated_by, allocation_date, status and coding_complete fields.
  #' @param user_doc_id Id of the document to be assigned or a vector of document ids.
  #' @param user_id Id of the user the document is to be assigned to or a vector of user ids.
  #' @param allocation_type Type of allocation (training, testing, coding, checking, ideal).
  #' @param allocation_date Date allocation made (usually today).
  #' @param status Status of document coding (generally 'NEW' for newly reassigned documents).

  #' @export

  if(length(user_doc_id)>1|length(user_id)>1) stop("reassign_article_to_user requires single user_doc_id and user_id")
  con <- manage_dbcons()

  allocated_at <- as.character(Sys.time())

  this_sql<-"UPDATE portal_userdocumentallocation SET user_id=?user_id, allocation_date=?allocation_date, allocation_type=?allocation_type, allocated_by=?allocated_by, status=?status, coding_complete=?coding_complete, last_updated=?allocated_at WHERE id=?user_doc_id ;"

      this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                         user_id=user_id,
                                         allocation_date=allocation_date,
                                         allocation_type=allocation_type,
                                         allocated_by=allocated_by,
                                         status=status,
                                         coding_complete=coding_complete,
                                         allocated_at=allocated_at,
                                         user_doc_id=user_doc_id)

      DBI::dbExecute(con, this_safe_sql)
  return(NULL)
}

assign_set <- function(user_id, set, allocation_type, allocated_by = "assign_set", status="NEW"){
  #' Assigns set of articles to user
  #'
  #' \code{assign_set} assigns a whole set of articles to a user (articles which are already allocated to a user are not allocated again).
  #' @export
  #' @describeIn assign_article_to_user Assign set of articles to user

  # restrict assignment of set to items not already allocated to the user

  items_needed <- user_allocated_fromset(user_id, set=set, not_allocated = TRUE)



  for (document_id in items_needed){
    assign_article_to_user(document_id=document_id,
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

assign_testset_to_user<-function(user_id, allocation_type="testing", allocated_by="assign_testset_to_user") {
  #' Assign test set
  #'
  #' \code{assign_testset_to_user} assigns the test set of articles to a user specified by the user id. Articles which are already allocated to a user are not allocated again.
  #' @export
  #' @describeIn assign_article_to_user Assigns testset to user or users

  testset=durhamevp::define_testset()
  assign_set(user_id=user_id, set=testset, allocation_type=allocation_type, allocated_by=allocated_by)
}


assign_trainingset_to_user <- function(user_id, allocation_type="training", allocated_by="assign_trainingset_to_user"){
  #' Assign training set to user
  #'
  #' \code{assign_trainingset_to_user} assigns the training set to user (articles which are already allocated to a user are not allocated again).
  #' @export
  #' @describeIn assign_article_to_user Assigns training set to user

  trainingset=define_trainingset()
  assign_set(user_id=user_id, set=trainingset, allocation_type=allocation_type, allocated_by=allocated_by)
}

assign_initalsets_to_users <- function(user_ids){
  #' Assigns initial article sets to user
  #'
  #' \code{assign_initalsets_to_users} assigns the training set and test sets to a user or set of users (articles which are already allocated to a user are not allocated again).

  #' @export
  #' @describeIn assign_article_to_user Assigns training and testset set to user



  for (user_id in user_ids){
    assign_testset_to_user(user_id=user_id)
    assign_trainingset_to_user(user_id=user_id)
  }
}

split_allocation_randomly <- function (user_ids, set, allocation_type="coding", allocated_by="split_random_assignment", restrict_to_actual=TRUE, make_assignments=FALSE){
  #' Divide a set of N*P articles amongst P users, giving N articles each user for coding.
  #'
  #' \code{allocate_number_randomly} divides a set of articles amongst a set of users all users get the same number of articles to code, with the articles being divided randomly amongst users. Each article is coded exactly once, and all users receive the same number of articles for coding (the function checks that the number of articles is a multiple of the number of users).
  #' By default the articles and user ids are checked against the database and codes which do not correspond to existing documents and/or users are ignored.
  #' @param user_ids The users to allocate the articles amongst (vector with single or multiple user_ids).
  #' @param set The set of documents which are to be allocated (vector with single or multiple of document_ids).
  #' @param allocation_type The value to write to the allocation_type field in the database document_allocations table (training, testing, coding, checking, ideal).
  #' @param allocated_by The value to write to the allocated_by field in the database document_allocations table.
  #' @param restrict_to_actual Should the restriction to actual users and documents be enforced. Should only be set to FALSE for debugging purposes.
  #' @param make_assignments Actually make changes to the database (TRUE) or only create a proposed set of changes as a dataframe (FALSE)
  #' @export


  # restrict to actual articles and actual users (both must already be in the database)
  if(restrict_to_actual){
    set<-documents_to_actual(set)
    user_ids<-users_to_actual(user_ids)
  }

  n_articles_each <- length(set) %/% length(user_ids)

  if(n_articles_each * length(user_ids) != length(set)){
    phrase<-""
    if(restrict_to_actual){
      phrase <- "Possibly because restrict_to_actual=TRUE."
    }

    stop(paste("Number of articles is not a multiple of number of users.", phrase))
  }
  assign_dat <- data.frame(matrix(ncol=2, nrow=0))
  names(assign_dat) <- c("document_id", "user_id")
  left <- set
  for(this_user in user_ids){
    if(length(left) == 1){ # prevent single number behaviour of sample function
      these_articles <- left
    } else {
      these_articles <- sample(left, n_articles_each, replace = FALSE)
    }
    left <- left[!(left %in% these_articles)]
    for(this_article in these_articles)
    assign_dat <- dplyr::bind_rows(assign_dat,
                                   data.frame(document_id=this_article, user_id=this_user))
  }
  if(make_assignments){
    allocate_from_df(assign_dat, allocation_type, allocated_by)
  }
  assign_dat
}

allocate_randomly <- function (user_ids, set, coder_rate=1.1, allocation_type="coding", allocated_by="regular_random_assignment", restrict_to_actual=TRUE, make_assignments=TRUE){
  #' Randomly assign a set of articles amongst a set of users for coding
  #'
  #' \code{allocate_randomly} assigns a set of articles amongst a set of users for them to code in the election violence database.
  #' By default the articles and user ids are checked against the database and codes which do not correspond to existing documents and/or users are ignored.
  #' @param user_ids The users to allocate the articles amongst (vector with single or multiple user_ids).
  #' @param set The set of documents which are to be allocated (vector with single or multiple of document_ids).
  #' @param coder_rate The average number of coders to be allocated to a document.
  #' @param allocation_type The value to write to the allocation_type field in the database document_allocations table (training, testing, coding, checking, ideal).
  #' @param allocated_by The value to write to the allocated_by field in the database document_allocations table.
  #' @param restrict_to_actual Should the restriction to actual users and documents be enforced. Should only be set to FALSE for debugging purposes.
  #' @param make_assignments Actually make changes to the database (TRUE) or only create a proposed set of changes as a dataframe (FALSE)
  #' @export


  # restrict to actual articles and actual users (both must already be in the database)
  if(restrict_to_actual){
    set<-documents_to_actual(set)
    user_ids<-users_to_actual(user_ids)
  }
  min_coders <- floor(coder_rate)
  additional_coder_rate <- coder_rate - min_coders

  #print(set)
  #print(user_ids)

  assign_dat <- data.frame(matrix(ncol=2, nrow=0))
  names(assign_dat) <- c("document_id", "user_id")

  for (this_article in set){
    n_assignments = min_coders + stats::rpois(1, additional_coder_rate)
    users_already_coding <- get_allocation(document_id=this_article)$user_id
    available_users <- user_ids [!user_ids %in% users_already_coding]
    if (length(available_users)< n_assignments) {
      warning(paste0("insufficient available users: for document ", this_article, " aim to code document ", n_assignments, " times with only ", length(available_users), " coder(s) available. No assignments made for this document."))
    } else {
      if (length(available_users)==1){ # prevent single number behaviour of sample function
        these_users <- available_users
      } else{
        these_users <- sample(available_users, n_assignments)
      }
      for (this_user in these_users){
        assign_dat <- dplyr::bind_rows(assign_dat,
                                data.frame(document_id=this_article, user_id=this_user))

      }
    }
  }
  if(make_assignments){
    allocate_from_df(assign_dat, allocation_type, allocated_by)
  }
  assign_dat
}

allocate_randomly_with_some_double_assignment <- function (double_coding_user_ids, other_user_ids, set, n_double_coding, coder_rate=1.1, allocation_type="coding", allocated_by="irregular_random_assignment", restrict_to_actual=TRUE, make_assignments=TRUE) {
  #' Assigns articles to users with some double coding and some another coding rate. Some users have a specified number of articles also assigned to another user (double coding). Remaining assignments are randomly allocated across all users.
  #'
  #' \code{allocate_randomly_with_some_double_assignment} assigns articles to some users with a specified number of articles to be double coded by the other users. Articles which are not used in this double coding are allocated amongst all users at the specified coding rate.
  #' By default the articles and user ids are checked against the database and codes which do not correspond to existing documents and/or users are ignored.
  #' @param double_coding_user_ids The users who will have a specified number of their articles double coded. to allocate the articles amongst (vector with single or multiple user_ids).
  #' @param other_user_ids The other users to allocate the articles amongst (vector with single or multiple user_ids).
  #' @param set The set of documents which are to be allocated (vector with single or multiple of document_ids).
  #' @param n_double_coding Number of articles (per user) that should be doubled coded (by those users undertaking double coding).
  #' @param coder_rate The average number of coders to be allocated to a document.
  #' @param allocation_type The value to write to the allocation_type field in the database document_allocations table (training, testing, coding, checking, ideal).
  #' @param allocated_by The value to write to the allocated_by field in the database document_allocations table.
  #' @param restrict_to_actual Should the restriction to actual users and documents be enforced. Should only be set to FALSE for debugging purposes.
  #' @param make_assignments Actually make changes to the database (TRUE) or only create a proposed set of changes as a dataframe (FALSE)
  #' @export

  n_double_coding_users<-length(double_coding_user_ids)
  n_articles_required_for_double_coding <- n_double_coding_users * n_double_coding

  if(n_articles_required_for_double_coding > length(set)) {
    stop("Insufficient articles for double coding.")
  }
  if(length(other_user_ids)<1){
    stop("Insufficient other users to allow double coding.")
  }

  double_coding_articles <- sample(set, n_articles_required_for_double_coding, replace = FALSE)
  other_articles <- set[!(set %in% double_coding_articles)]

  # First split a subset of the documents randomly amongst the new users (these documents will all be allocated to another user for coding)
  df1<-split_allocation_randomly(user_ids = double_coding_user_ids, double_coding_articles, allocation_type = allocation_type, allocated_by=allocated_by, restrict_to_actual = restrict_to_actual, make_assignments = FALSE)

  # Now allocate the same subset of the documents randomly to amongst the other users (this ensures the double coding)
  df2 <- allocate_randomly(user_ids = other_user_ids, double_coding_articles, coder_rate=1, allocation_type = allocation_type, allocated_by=allocated_by, restrict_to_actual = restrict_to_actual, make_assignments = FALSE)

  # Then allocate the rest of the documents randomly at the specified rate
  df3 <- allocate_randomly(user_ids = c(double_coding_user_ids, other_user_ids), other_articles, coder_rate=coder_rate, allocation_type = allocation_type, allocated_by=allocated_by, restrict_to_actual = restrict_to_actual, make_assignments = FALSE)
  assign_dat <- dplyr::bind_rows(df1, df2, df3)
  if(make_assignments){
    allocate_from_df(assign_dat, allocation_type, allocated_by)
  }
  assign_dat
}

allocate_from_df<-function(assign_dat, allocation_type, allocated_by){
  #' Assign users to documents from a dataframe
  #'
  #' Assign users to documents from a dataframe where dataframe has a user_id column and and a document_id column

  #' @param assign_dat A dataframe (or tibble) with a user_id column and a document_id column
  #' @param allocation_type The allocation type to write to the database
  #' @param allocated_by The allocated by to write to the database
  #' @export


  apply(assign_dat, 1, function (x)
    assign_article_to_user(
                           x[["document_id"]],
                           x[["user_id"]],
                           allocation_type = allocation_type,
                           allocated_by = allocated_by
                           ))
}

reallocate_from_df<-function(reassign_dat, allocation_type, allocated_by){
  #' Assign users to documents from a dataframe
  #'
  #' Assign users to documents from a dataframe where dataframe has a user_id column and and a document_id column

  #' @param reassign_dat A dataframe (or tibble) with a user_id column and a document_id column
  #' @param allocation_type The allocation type to write to the database
  #' @param allocated_by The allocated by to write to the database
  #' @export


  apply(reassign_dat, 1, function (x)
    reassign_article_to_user(
      x[["user_doc_id"]],
      x[["user_id"]],
      allocation_type = allocation_type,
      allocated_by = allocated_by
    ))
}

reallocate_randomly<- function (user_ids, user_doc_ids, allocated_by="reallocate_randomly", allocation_type="coding", restrict_to_actual=TRUE, make_assignments=TRUE, force=FALSE){
  #' Randomly reassign a set of articles amongst a set of users for coding
  #'
  #' \code{reallocate_randomly} reassigns a set of already existing articles amongst a set of users for them to code in the election violence database.
  #' By default allocation_ids and user ids are checked against the database and codes which do not correspond to existing assignments and/or users are ignored.
  #' @param user_ids The users to allocate the articles amongst (vector with single or multiple user_ids).
  #' @param user_doc_ids The set of documents which are to be allocated (vector with single or multiple of document_ids).
  #' @param allocated_by The value to write to the allocated_by field in the database document_allocations table.
  #' @param restrict_to_actual Should the restriction to actual users and documents be enforced. Should only be set to FALSE for debugging purposes.
  #' @param make_assignments Actually make changes to the database (TRUE) or only create a proposed set of changes as a dataframe (FALSE)
  #' @param force Make changes even if usual conditions are violated (status=="NEW", allocation_type=="coding")
  #' @export


  # restrict to actual actual users (must already be in the database)
  if(restrict_to_actual){
    user_ids<-users_to_actual(user_ids)
  }

  reassign_dat <- data.frame(matrix(ncol=2, nrow=0))
  names(reassign_dat) <- c("user_doc_id", "user_id")
  n_assignments <- 1
  to_change<-get_allocation(user_doc_id=user_doc_ids)
  if(!force){
    to_change<-dplyr::filter(to_change, allocation_type=="coding", status=="NEW")
  }
  for (this_allocation in seq_along(1:nrow(to_change))){
    this_doc<-to_change[this_allocation, "document_id"]
    this_user_doc_id<-to_change[this_allocation, "id"]
    users_already_coding <- get_allocation(document_id=this_doc)$user_id
    available_users <- user_ids [!user_ids %in% users_already_coding]
    if (length(available_users)< 1) {
      warning(paste0("insufficient available users: for document ", this_doc, " in user_doc_id ", this_user_doc_id, ". ", length(available_users), " coder(s) available. No assignments made for this document."))
    } else {
      if (length(available_users)==1){ # prevent single number behaviour of sample function
        these_users <- available_users
      } else{
        these_users <- sample(available_users, n_assignments)
      }
      for (this_user in these_users){
        reassign_dat <- dplyr::bind_rows(reassign_dat,
                                       data.frame(user_doc_id=this_user_doc_id, user_id=this_user))

      }
    }
  }
  if(make_assignments){
    reallocate_from_df(reassign_dat, allocation_type, allocated_by)
  }
  reassign_dat
}
