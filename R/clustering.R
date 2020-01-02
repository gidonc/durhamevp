
create_new_autocluster <- function(){
  #' \code{create_new_autocluster} Creates a new autodetected cluster on the data.
  #' @return the id on the database of the newly created autodetected cluster.
  #'
  #' @ export

  con <- manage_dbcons()

  sql1<-'insert into portal_autodetectedcluster () values ();'
  sql2<-'select id from portal_autodetectedcluster order by id desc limit 0,1;'
  DBI::dbGetQuery(con, sql1)
  this_cluster_id<-as.numeric(dplyr::pull(DBI::dbGetQuery(con, sql2), id))

  this_cluster_id
}


assign_eventreport_to_autocluster <- function(autodetected_cluster_id, event_report_id){
  #' Assign a single event report to an autodetected cluster on the database.
  #'
  #' \code{assign_eventreport_to_autocluster} assigns an event report to an autodetected cluster on the database.
  #'
  #' @param autodetected_cluster_id Id of the autodetected cluster (generally newly created).
  #' @param event_report_id Id of the event report to assign.

  #' @export

  con <- manage_dbcons()

  this_sql <- "UPDATE portal_eventreport SET autodetected_cluster_id=?autodetected_cluster_id WHERE id=?event_report_id ;"
  this_safe_sql <- DBI::sqlInterpolate(DBI::ANSI(),
                                       this_sql,
                                       autodetected_cluster_id = autodetected_cluster_id,
                                       event_report_id = event_report_id)
  DBI::dbGetQuery(con, this_safe_sql)
}


assign_autocluster_to_user <- function(autodetected_cluster_id, user_id, allocation_date=as.character(Sys.Date()), last_updated=as.character(Sys.time()), completed=0, needs_additional_checks=0){
  #' Assign autodetected cluster to a user.
  #'
  #' \code{assign_autocluster_to_user} assigns an autodetected cluster to a user for verification.
  #' @param autodetected_cluster_id Id of the autodetected cluster (generally newly created).
  #' @param user_id Id of the user the the cluster is to be assigned to.
  #' @param allocation_date Date allocation made (usually today).
  #' @param last_updated Date record last updated (usually today).
  #' @param completed Is the cluster verification complete (usually 0 [i.e. not complete])
  #' @para needs_additional_checks Does the cluster need additional checks (for a new cluster allocation should be zero - may be changed later on the platform as part of the process)

  #' @export

  con <- manage_dbcons()

  this_sql <- "INSERT INTO  portal_userautodetectedclusterallocation (autodetected_cluster_id, user_id, allocation_date, last_updated, completed, needs_additional_checks) VALUES (?autodetected_cluster_id, ?user_id, ?allocation_date, ?last_updated, ?completed, ?needs_additional_checks) ;"
  this_safe_sql <- DBI::sqlInterpolate(DBI::ANSI(),
                                       this_sql,
                                       autodetected_cluster_id = autodetected_cluster_id,
                                       user_id = user_id,
                                       allocation_date = allocation_date,
                                       last_updated=last_updated,
                                       completed = completed,
                                       needs_additional_checks= needs_additional_checks)

  DBI::dbGetQuery(con, this_safe_sql)
}

assign_clusters_from_df<-function(df, cluster_col=geodatecluster, event_report_id_col=event_report_id){
  #' Assigns event report to an autodetected cluster
  #'@param df a data frame or tibble indicating clustering to upload to the database.
  #'@param cluster_col the column of the data frame which indicates the clustering.
  #'@param event_report_id_col the column of the data frame which identifies the event report
  #'@export


  con <- manage_dbcons()

  ecluster_col <- dplyr::enquo(cluster_col)
  eevent_report_id_col <- dplyr::enquo(event_report_id_col)

  clusters<-unique(dplyr::pull(arrange(df, !!ecluster_col), !!ecluster_col))
  n_clusters<-length(clusters)
  autodetected_cluster_ids<-vector(length=n_clusters)


  for(n_cluster in seq_along(clusters)){
    cluster <-clusters[n_cluster]
    events_in_cluster<-filter(df, !!ecluster_col==cluster)
    # create new cluster
    sql1<-'insert into portal_autodetectedcluster () values ();'
    sql2<-'select id from portal_autodetectedcluster order by id desc limit 0,1;'
    DBI::dbGetQuery(con, sql1)
    autodetected_cluster_ids[n_cluster]<-as.numeric(pull(DBI::dbGetQuery(con, sql2), id))
    # autodetected_cluster_ids[n_cluster]<-clusters[n_cluster]
    for(er in seq_len(nrow(events_in_cluster))){
      this_er<-pull(events_in_cluster, !!eevent_report_id_col)[er]

      durhamevp::assign_eventreport_to_autocluster(autodetected_cluster_ids[n_cluster], this_er)
    }

  }


  autodetected_cluster_ids
}

assign_autoclusters_to_user <- function(autocluster_ids, user_id){
  #' Assigns mutiple auto detected clusters to a single user.
  #'
  #' @param autocluster_ids The autocluster_ids to allocate.
  #' @param user_id The user id to allocate to.
  #' @export

  for(n in seq_along(autocluster_ids)){
    assign_autocluster_to_user(autocluster_ids[n], user_id)
  }

}

assign_reallocation_to_user <- function(event_report_id, user_id, allocation_date=as.character(Sys.Date()), last_updated=as.character(Sys.time()), completed=0){
  #' Assign single events to user for reallocation to clusters.
  #'
  #' \code{assign_reallocation_to_user} assigns an autodetected cluster to a user for verification.
  #' @param event_report_id event_report_id of the single events.
  #' @param user_id Id of the user the the cluster is to be assigned to.
  #' @param allocation_date Date allocation made (usually today).
  #' @param last_updated Date record last updated (usually today).
  #' @param completed Is the cluster verification complete (usually 0 [i.e. not complete])

  #' @export

  con <- manage_dbcons()

  this_sql <- "INSERT INTO portal_userreallocationallocation (allocation_date, completed, last_updated, event_report_id, user_id) VALUES (?allocation_date, ?completed, ?last_updated, ?event_report_id, ?user_id) ;"
  this_safe_sql <- DBI::sqlInterpolate(DBI::ANSI(),
                                       this_sql,
                                       event_report_id = event_report_id,
                                       user_id = user_id,
                                       allocation_date = allocation_date,
                                       last_updated=last_updated,
                                       completed = completed)

  DBI::dbGetQuery(con, this_safe_sql)
}

assign_reallocations_to_user <- function(event_report_ids, user_id){
  #' Assigns mutiple single events for reallocation to clusters to a single user.
  #'
  #'@param event_report_ids The autocluster_ids to allocate.
  #'@param user_id The user id to allocate to.
  #'@export

  for(n in seq_along(event_report_ids)){
    assign_reallocation_to_user(event_report_ids[n], user_id)
  }

}

get_autodetected_cluster_allocations<-function(user_id="all", autodetected_cluster_id="all"){
  #' Get the autodetcted cluster allocations currently allocated to the user.
  #' @param user_id The user_id to check in the database.
  #' @param autodetected_cluster_id The set of autodetected cluster ids to check in the database
  #' @return dataframe of the autodetected cluster allocations to the user.
  #' @export

  con <- manage_dbcons()
  this_sql <-"SELECT * FROM portal_userautodetectedclusterallocation" # base query

  res<-build_where_condition("user_id", user_id, this_sql, NULL)
  res<-build_where_condition("autodetected_cluster_id", autodetected_cluster_id, res[[1]], res[[2]])

  res[["condition"]] <- paste(res[["condition"]], ";")
  this_sql<-res[["condition"]]
  interpolate_list <- res[["interpolate_list"]]
  this_safe_sql<-DBI::sqlInterpolate(DBI::ANSI(), this_sql,
                                     .dots = interpolate_list)

  allocation<-DBI::dbGetQuery(con, this_safe_sql)


  allocation
}

assign_verified_cluster_to_user <- function(verified_cluster_id, user_id, allocation_date=as.character(Sys.Date()), last_updated=as.character(Sys.time()), completed=0){
  #' Assign verified (and combined) clusters to user for reallocation to clusters.
  #'
  #' \code{assign_verfied_cluster_to_user} assigns a verfied cluster to a user for combination.
  #' @param verified_cluster_id verified_cluster_id of the verified cluster to be assigned.
  #' @param user_id Id of the user the the cluster is to be assigned to.
  #' @param allocation_date Date allocation made (usually today).
  #' @param last_updated Date record last updated (usually today).
  #' @param completed Is the cluster verification complete (usually 0 [i.e. not complete])

  #' @export

  con <- manage_dbcons()

  this_sql <- "INSERT INTO portal_usercombinationallocation (allocation_date, completed, last_updated, verified_cluster_id, user_id) VALUES (?allocation_date, ?completed, ?last_updated, ?verified_cluster_id, ?user_id) ;"
  this_safe_sql <- DBI::sqlInterpolate(DBI::ANSI(),
                                       this_sql,
                                       verified_cluster_id = verified_cluster_id,
                                       user_id = user_id,
                                       allocation_date = allocation_date,
                                       last_updated=last_updated,
                                       completed = completed)

  DBI::dbGetQuery(con, this_safe_sql)
}

assign_verified_clusters_to_user <- function(verified_cluster_ids, user_id){
  #' Assigns multiple verified clusters to a single user for combination.
  #'
  #'@param verified_cluster_ids The verified_cluster_ids to allocate.
  #'@param user_id The user id to allocate to.
  #'@export

  con <- manage_dbcons()

  for(n in seq_along(verified_cluster_ids)){
    assign_verified_cluster_to_user(verified_cluster_ids[n], user_id)
  }

}

get_verified_clusters <- function(){
  #' Finds verified clusters (and their existing size).
  #'
  #' \code{get_verified_clusters} reports the verified clusters and the event reports currently in that cluster.
  #' @export

  con <- manage_dbcons()

  this_sql<-this_sql<-"Select vc.id as verified_cluster_id, user_alloc_id, latitude, longitude, event_report_id
  FROM `portal_verifiedcluster` vc LEFT JOIN `portal_verifiedclusterentry` vce ON vc.id = vce.verified_cluster_id; " # base query

  DBI::dbGetQuery(con, this_sql)
}
