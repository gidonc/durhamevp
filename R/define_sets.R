define_trainingset<-function(trainingset=2:17){
  #' Returns the training set document ids.
  #' @param trainingset Vector of items which can replace the default training set (items 2 to 17).
  #' @export
  trainingset
}

define_testset<-function(testset=18:37){
  #' Returns the test set document ids.
  #' @param testset Vector of items which can replace the default test set (items 18 to 37).
  #' @export

  testset
}

date_to_election<-function(the_date, election_start_col="monthsearch_start", election_end_col="monthsearch_end", election_data=durhamevp::election_dates, return_col="election_name"){
  #' Finds the election relating to any specific date.
  #'
  #'
  #'@param the_date the date to classifiy
  #'@param election_start_col the column in the election data frame which identifies when the election starts.
  #'@param election_end_col the column in the election data frame which identifies when the election ends.
  #'@param election_data the data frame containing the election data.
  #'@param return_col the column in the data frame containing the name of the election (to be returned by the function).
  #'@export
  the_date<-lubridate::date(the_date)
  starts<-lubridate::date(election_data[, election_start_col, drop=TRUE])
  ends<-lubridate::date(election_data[, election_end_col, drop=TRUE])
  all_res<-election_data[,return_col, drop=TRUE]
  the_res<-unlist(sapply(the_date,
                        function(y){
                          k<-which(sapply(seq_along(all_res), function(x) dplyr::between(y, starts[x], ends[x])))
                          ifelse(identical(k, character(0)), NA_character_, k)
                        }
                  ))


  res<-all_res[the_res]

}

month_contains_election<-function(the_date, election_start_col="monthsearch_start", election_end_col="monthsearch_end", election_data=durhamevp::election_dates, return_col="election_name"){
  #' Finds the election relating to a month (where an election took place on any day that month).
  #'
  #'
  #'@param the_date the month to classifiy as yyyy-mm-dd where dd must be 01.
  #'@param election_start_col the column in the election data frame which identifies when the election starts.
  #'@param election_end_col the column in the election data frame which identifies when the election ends.
  #'@param election_data the data frame containing the election data.
  #'@param return_col the column in the data frame containing the name of the election (to be returned by the function).
  #'@export

  the_date<-lubridate::ymd(the_date)
  if(lubridate::day(the_date)!=1){
    stop("Day not first of the month")
  }
  n_days<-lubridate::days_in_month(the_date)
  dates_in_month <- 1:n_days
  dates_in_month <- rep(the_date, n_days)
  lubridate::day(dates_in_month)<-1:n_days
  res<-date_to_election(dates_in_month)
  if(sum(is.na(res)==length(res))){
    res<-NA
  } else {
    res<-res[!is.na(res)][1]
  }

  res
}


