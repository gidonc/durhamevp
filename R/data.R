#' Election Dates and Basic Information.
#'
#' A dataset containing the dates and other basic information about British General Elections between 1832 and 1910.
#'
#' @format A data frame with 20 rows and 11 variables, which are:
#' \describe{
#'   \item{election_name}{The standardized name of the election,used to link to other data sets in the election violence project.}
#'   \item{dissolution}{Date Parliament dissolved prior to the election.}
#'   \item{commencement}{Date Parliament commences after the election.}
#'   \item{duration_dissolution_commencement}{Number of days between dissolution and commencement.}
#'   \item{polling_start}{Date General Election polling started.}
#'   \item{polling_end}{Date General Election polling ended.}
#'   \item{polling_duration}{Number of days between the start and end of General Election polling.}
#'   \item{monthsearch_start}{First date in project's systematic search of one month of newspaper articles around the election.}
#'   \item{monthsearch_end}{Last date in project's systematic search of one month of newspaper articles around the election.}
#'   \item{monthsearch_duration}{Number of days in projects systematic search of newspaper articles.}
#'   \item{keywords_13_complete}{Indicates whether the projects has completed the systematic search of the British Newspaper Archive using its set of 13 keywords to detect election violence has been completed.}
#' }
#' @source Gary Hutchison collected the information from various sources
"election_dates"


#' Election Violence Event Level Data.
#'
#' An event level dataset containing violent events in British General Elections between 1832 and 1910. Note that information on the detail of events is not in this dataset, but instead is stored in event reports relating to these events. This data can be linked to the event report data using the event_id column.
#'
#' @format A data frame with 2971 rows and 10 variables, which are:
#' \describe{
#'   \item{event_id}{The unique identifier of the event (which corresponds to the event_id in the processed event_reports table and to the final_cluster_id in the clustering table (in cluster attempt 401 to 420).}
#'   \item{election_name}{Standardised name of election.}
#'   \item{event_constituency_g_name} {The most commonly associated constituency (Vision of Britain g_name) associated with this event in the election violence database.}
#'   \item{event_level}{Manual classification the event intensity into four categories: Riot, Disturbance, Incident, Individual. These categories are based on Wasserman and Jaggard (2006) 'Electoral violence in mid ninteenth-century England and Wales', *Historical Research*, 207:80, but with some additions (and an extra 'individual' category.).
#'   \cr **Riot** A serious and sustained outbreak of collective violence, involving the implicit or explicit use of force, intimidation or coercion, and which resulted in physical damage to persons or property - or the immediate fear that such would occur. Riots commonly evoked a magisterial response, such as the reading of the Riot Act proclamation and/or the forcible restoration of the peace by police officers or the military.
#'   \cr **Disturbance** A less serious breach of the peace than a riot, and involved episodic outbursts of crowd violence rather than the type of sustained disorder characteristic of a riot. A disturbance generated a degree of public alarm and usually elicited some measure of official response.
#'   \cr **Incident** A noisy or demonstrative action by a crowd of people that is caused by, interferes with, or disrupts, the proceedings of an election campaign. An incident was a relatively short-lived event that involved little overt violence and invoked a limited or no official response.
#'   \cr **Individual** An action or set of actions involving fewer than 3-4 people, there being no crowd presence or active involvement. Occurrences of individual violence tend to be brief, and often had no effect on the proceedings of an election campaign, but were nevertheless caused by the occurrence of an election.}
#'   \item{imap_constituency_g_name}{Vision of Britain g_name of the main constituency related to the event as found on the interactive map.}
#'   \item{imap_county_g_name}{Vision of Britain g_name of county of the event as found on the interactive map.}
#'   \item{imap_townvillage}{The name of the town or village where the event took place, as found on the interactive map.}
#'   \item{event_longitude}{Event longitude taken initially from the interactive map and supplemented by further cleaning.}
#'   \item{event_latitude}{Event latitude taken initially from the interactive map and supplemented by further cleaning.}
#'   \item{summary_event}{An edited brief description of the event from the database.}
#'   }
#' @source Processed Data from Durham Election Violence Dataset created by Gary Hutchison for the interactive map and supplemented by further cleaning. The interactive map can be found at <https://coders.victorianelectionviolence.uk/interactive_map>.
"ev_events"


#' Mitchell Newspaper Partisan Affiliation for Each user_doc.
#'
#' The partisan information comes from the Mitchell Advertiser handbooks for 1847, 1856 (linked to the 1857 election), 1868, 1885, and 1910. The partisan leaning coding is described by the newspapers themselves in the Mitchell and was translated by Dr Gary Hutchison into two variables. Sources from our dataset were matched to the Mitchell data based on (1) publication title, (2) publication location, and (3) publication weekday/frequency. This ensures that newspapers with the same name are matched correctly and allows weekday and weekend editions to have different partisan leanings. This process was done by hand by a trained and supervised RA. The quality of the match is recorded in match_quality, where 3 means perfect match on all three criteria, 2 means there is good match on two criteria, and 1 means there is a match, but only on one criteria (generally title). Where match_quality==NA, no match could be found. The Mitchell data is added to the document_level data and missing years have been inter- and extrapolated using forward and backward panel completion. There are purely interpolated variables (only partisan and leaning_simple), where “lof” and “bw” indicates whether the last observation is brought forward or backward, and fully inter- and extrapolated variables. Those partisan and leaning_simple variables are named accordingly in the data.
#'
#' @format A data frame with 11667 rows and 14 variables, which are:
#' \describe{
#'   \item{user_doc_id}{The user_doc_id the record relates to (links to user_doc table in the database).}
#'   \item{mitchell_title}{The title of the publication as recorded in the Mitchell handbook.}
#'   \item{mitchell_partisan} {The partisan leaning as recorded in the Mitchell handbook.}
#'   \item{leaning} {A simplified version of the Mitchell partisan variable. Values: liberal, lib-union, lib-lab, lib-con, radical, con-lib, conservative, religious-conservative, repeal, netural, UNKNOWN, NA}
#'   \item{partisan} {Records whether a newspaper has a partisan leaning or not. The variable can take there values “Y”=yes, “N”=no, “UNKNOWN”= unknown, i.e., the information given does not allow us to make a judgement if and whether a paper has a specific political leaning. NAs mean that no partisan information was recorded (if a paper could be matched) or no paper in the Mitchell list could be matched to the newspaper source.}
#'   \item{partisan_interpol_f} {Interpolated partisan variable using last observation forwards. No extrapolation. Values: Y (yes), N (no), UNKNOWN, NA}
#'   \item{partisan_interpol_b} {Interpolated partisan variable using last observation backwards. No extrapolation.. Values: Y (yes), N (no), UNKNOWN, NA}
#'   \item{partisan_full_f} {Interpolated and extrapolated partisan variable using last observation forwards if possible, and last observation backwards if necessary.. Values: Y (yes), N (no), UNKNOWN, NA}
#'   \item{partisan_full_b}{Interpolated and extrapolated partisan variable using last observation backwards if possible, and backwards if necessary.. Values: Y (yes), N (no), UNKNOWN, NA}
#'   \item{leaning_simple} {Records the leaning of the paper. The variable can take three values: “conservative-right”, “liberal-left”, or “neutral”. “neutral” is only possible is partisan==N. NA means a leaning could not be determined if partisan==Y or partisan==NA if there was a match or simply that there was not Mitchell match.}
#'   \item{leaning_simple_interpol_f}{Interpolated leaning_simple variable using last observation forwards. No extrapolation.}
#'   \item{leaning_simple_interpol_b}{Interpolated leaning_simple variable using last observation backwards. No extrapolation.}
#'   \item{leaning_simple_full_f}{Interpolated and extrapolated leaning_simple variable using last observation forwards if possible, and last observation backwards if necessary}
#'   \item{leaning_simple_full_b}{Interpolated and extrapolated leaning_simple variable using last observation backwards if possible, and last observation forwards if necessary}
#'   }
#' @source .
"user_docs_partisanmerger"

