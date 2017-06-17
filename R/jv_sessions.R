jv_sessions <- function(start_date = "2017-01-01"){
  jv_sessions <- jarvispack::jv_events() %>%
    # dplyr::filter(email == "mitchell.lisle@bigdatr.com") %>%
    distinct(user_id,timestamp,email)
    group_by(user_id) %>%
    mutate(timestamp = as.POSIXct(timestamp)) %>%
    mutate(idle_time = c(0,as.numeric(diff(timestamp), units="mins"))) %>%
    mutate(session_start = if_else(idle_time > 30 | idle_time == 0, 1, 0)) %>%
    left_join(jarvispack::jv_intercom(), by = "email") %>%
    mutate(user_days_on_platform = difftime(today(),as.Date(created_at),  units="days")) %>%
    mutate(is_weekend = wday(timestamp) %in% c(1,7)) %>%
    dplyr::filter(session_start == 1) %>%
    mutate(times = ifelse(session_start == 1, strftime(timestamp, format="%H"), NA)) %>%
    group_by(email, session_count, user_days_on_platform, last_request_at, timestamp) %>%
    summarise(mlr_sessions = sum(session_start), common_time = names(which.max(table(times)))) %>%
    mutate(session_per_day = mlr_sessions/as.numeric(user_days_on_platform)) %>%
    mutate(usage = if_else(session_per_day >= 0.5,'DAU', if_else(session_per_day < 0.5 & session_per_day > 0.1, 'WAU', 'MAU'))) %>%
    mutate(usage = factor(usage))


  jv_sessions

}
