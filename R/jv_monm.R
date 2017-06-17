jv_monm <- function(){
    #'
    #' Important: Must set env variables with apprpriate values for PostgresDB:
    #' Sys.setenv("POSTGRES_HOST" = "")
    #' Sys.setenv("POSTGRES_PORT" = "")
    #' Sys.setenv("POSTGRES_USER" = "")
    #' Sys.setenv("POSTGRES_PASS" = "")
    #' Sys.setenv("POSTGRES_DB" = "")
    #'
    #' @example
    #' jv_monm()
    #' 
    
    jv_monm <- jv_events() %>% 
    mutate(timestamp_month = month(timestamp)) %>%
    mutate(timestamp_year = year(timestamp)) %>%
    mutate(thismonth = month(today(tzone="Australia/Melbourne"))) %>%
    filter(timestamp_month >= (thismonth-1)) %>%
    group_by(timestamp_year,timestamp_month, user_id, timestamp) %>%
    mutate(idle_time = c(0,as.numeric(diff(timestamp), units="mins"))) %>%
    mutate(session_start = if_else(idle_time > 30 | idle_time == 0, 1, 0)) %>%
    left_join(jv_intercom(), by = c("email" = "email")) %>%
    filter(!is.na(signed_up_at)) %>%
    mutate(user_days_on_platform = difftime(today(),as.Date(created_at),  units="days")) %>%
    dplyr::filter(session_start == 1) %>%
    ungroup() %>%
    group_by(email, timestamp_month, timestamp_year, signed_up_at) %>%
    summarise(mlr_sessions = sum(session_start)) %>%
    arrange(desc(email)) %>%
    mutate(daysinmonth = days_in_month(timestamp_month)) %>%
    mutate(signed_up_at = as.Date(signed_up_at), signed_up_at_month = month(signed_up_at), signed_up_at_day = day(signed_up_at)) %>%
    mutate(adjusted_daysinmonth = if_else(timestamp_month == signed_up_at_month, daysinmonth - signed_up_at_day, daysinmonth)) %>%
    mutate(adjusted_daysinmonth = adjusted_daysinmonth+1) %>%
    mutate(avg_sessions_bymonth = mlr_sessions/adjusted_daysinmonth) %>%
    mutate(usage = if_else(avg_sessions_bymonth >= 0.5,'DAU', if_else(avg_sessions_bymonth < 0.5 & avg_sessions_bymonth > 0.1, 'WAU', if_else(avg_sessions_bymonth <= 0.1, 'MAU', 'None')))) %>%
    mutate(avg_sessions_bymonth = round(avg_sessions_bymonth, digits = 2))
    
    jv_monm
  }
  