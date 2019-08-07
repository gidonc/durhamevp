
assign_autocluster_to_user <- function(autodetected_cluster_id, user_id, allocation_date=as.character(Sys.Date()), last_updated=as.character(Sys.Date()), completed=0){
  #' Assign autodetected cluster to a user.
  #'
  #' \code{assign_article_to_user} assigns an autodetected cluster to a user for verification.
  #' @param autodetected_cluster_id Id of the autodetected cluster (generally newly created).
  #' @param user_id Id of the user the the cluster is to be assigned to.
  #' @param allocation_date Date allocation made (usually today).
  #' @param last_updated Date record last updated (usually today).
  #' @param completed Is the cluster verification complete (usually 0 [i.e. not complete])

  #' @export

  con <- manage_dbcons()

  this_sql <- "INSERT INTO  portal_userautodetectedclusterallocation (autodetected_cluster_id, user_id, allocation_date, last_updated, completed) VALUES (?autodetected_cluster_id, ?user_id, ?allocation_date, ?last_updated, ?completed) ;"
  this_safe_sql <- DBI::sqlInterpolate(DBI::ANSI(),
                                       this_sql,
                                       autodetected_cluster_id = autodetected_cluster_id,
                                       user_id = user_id,
                                       allocation_date = allocation_date,
                                       last_updated=last_updated,
                                       completed = completed)

  DBI::dbGetQuery(con, this_safe_sql)
}
