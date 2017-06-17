jv_intercom <- function() {
  intercom_query <- (
    "select name,
    last_request_at,
    email,
    created_at,
    signed_up_at,
    session_count
    from jarvis_intercom.users
    where email not like '%bigdatr%'
    and email not like '%blueflag%'
    and email not like '%getjarvis%'")

  jv_intercom_raw <- mlr::mlr_getPostgres(Sys.getenv("POSTGRES_HOST"),
                                          Sys.getenv("POSTGRES_PORT"),
                                          Sys.getenv("POSTGRES_USER"),
                                          Sys.getenv("POSTGRES_PASS"),
                                          Sys.getenv("POSTGRES_DB"),
                                          intercom_query)
  
  jv_intercom_raw <- jv_intercom_raw %>%
    mutate(since_last_request = ifelse(last_request_at != '', as.Date(today())-as.Date(last_request_at), 0))
  
  jv_intercom_raw
}
