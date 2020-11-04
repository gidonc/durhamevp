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
#' An event level dataset containing violent events in British General Elections between 1832 and 1910.Note that information on the detail of events is not in this dataset, but instead is stored in event reports relating to these events. This data can be linked to the event report data using the event_id column.
#'
#' @format A data frame with 2915 rows and 9 variables, which are:
#' \describe{
#'   \item{event_id}{The unique identifier of the event (which corresponds to the event_id in the processed event_reports table and to the final_cluster_id in the clustering table (in cluster attempt 401 to 420).}
#'   \item{election_name}{Standardised name of election.}
#'   \item{event_level}{Manual classification the event intensity into four categories: Riot, Disturbance, Incident, Individual. These categories are based on Wasserman and Jaggard (2006) 'Electoral violence in mid ninteenth-century England and Wales', *Historical Research*, 207:80, but with some additions (and an extra 'individual' category.).
#'   \cr **Riot** A serious and sustained outbreak of collective violence, involving the implicit or explicit use of force, intimidation or coercion, and which resulted in physical damage to persons or property - or the immediate fear that such would occur. Riots commonly evoked a magisterial response, such as the reading of the Riot Act proclamation and/or the forcible restoration of the peace by police officers or the military.
#'   \cr **Disturbance** A less serious breach of the peace than a riot, and involved episodic outbursts of crowd violence rather than the type of sustained disorder characteristic of a riot. A disturbance generated a degree of public alarm and usually elicited some measure of official response.
#'   \cr **Incident** A noisy or demonstrative action by a crowd of people that is caused by, interferes with, or disrupts, the proceedings of an election campaign. An incident was a relatively short-lived event that involved little overt violence and invoked a limited or no official response.
#'   \cr **Individual** An action or set of actions involving fewer than 3-4 people, there being no crowd presence or active involvement. Occurrences of individual violence tend to be brief, and often had no effect on the proceedings of an election campaign, but were nevertheless caused by the occurrence of an election.}
#'   \item{imap_constituency_g_name}{Vision of Britain g_name of the main constituency related to the event as found on the interactive map.}
#'   \item{imap_county_g_name}{Vision of Britain g_name of county of the event as found on the interactive map.}
#'   \item{imap_townvillage}{The name of the town or village where the event took place, as found on the interactive map.}
#'   \item{imap_longitude}{Event longitude on the interactive map.}
#'   \item{imap_latitude}{Event latitude on the interactive map.}
#'   \item{imap_event_summary}{An edited brief description of the event from the interactive map (taking all the cluster best descriptions into account).}
#'   }
#' @source Processed Data from Durham Election Violence Dataset created by Gary Hutchison for the interactive map. The interactive map can be found at <https://coders.victorianelectionviolence.uk/interactive_map>.
"ev_events"
