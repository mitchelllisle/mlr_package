jv_events <- function(start_date = "2017-01-01"){
  #'
  #' Important: Must set env variables with apprpriate values for PostgresDB:
  #' Sys.setenv("POSTGRES_HOST" = "")
  #' Sys.setenv("POSTGRES_PORT" = "")
  #' Sys.setenv("POSTGRES_USER" = "")
  #' Sys.setenv("POSTGRES_PASS" = "")
  #' Sys.setenv("POSTGRES_DB" = "")
  #'
  #' @example
  #' jv_events()
  
  events_query <- paste(
    "SELECT
    tracks.user_id,
    tracks.timestamp AT TIME ZONE 'Australia/Melbourne' as timestamp,
    event,
    email
    FROM jarvis_production.tracks
    join jarvis_production.users on user_id = jarvis_production.users.id
    where tracks.timestamp AT TIME ZONE 'Australia/Melbourne' >= '", start_date ,"'  
    and email not like '%bigdatr%'
    and email not like '%blueflag%'
    and email not like '%getjarvis%'
    order by tracks.user_id,tracks.timestamp asc"
  )
  
  jv_events <- mlr::mlr_getPostgres(Sys.getenv("POSTGRES_HOST"),
                                          Sys.getenv("POSTGRES_PORT"),
                                          Sys.getenv("POSTGRES_USER"),
                                          Sys.getenv("POSTGRES_PASS"),
                                          Sys.getenv("POSTGRES_DB"),
                                          events_query)
  jv_events
}