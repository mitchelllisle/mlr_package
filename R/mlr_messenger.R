

mlr_messenger <- function(app = "Hipchat", success_flag = "status", channel = "AppChat", success_value = TRUE, success_message = "Success!", fail_message = "Failed!"){
  
  # Function for executing Slack message
  slack <- function(){
    slackr::slackr_setup(channel=paste0("#", channel), incoming_webhook_url="https://hooks.slack.com/services/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")
    
    if(success_flag == success_value){
      message <- as.character(success_message)
      slackr::slackr_bot(message)
    } else {
      message <- as.character(fail_message)
      slackr::slackr_bot(message)
    }
  }
  
  # Function for executing Hipchat message
  hipchat <- function(){
    if(status == TRUE){
      hipchat_send('room', 'JARVIS', 'notification',
                   color = 'green', message = as.character(paste0("@ShielMunroe @FarzanaHaroon @MitchellLisle @AtridgeDCosta So far we've invited ", current_progress$invited,
                                                                  " and signed up ", current_progress$signed, ". ",
                                                                  "To hit our target of ", target_results$signupgoal, " we need ",
                                                                  target_results$invites_per_day, " invites and ",
                                                                  target_results$signups_per_day, " sign ups per day.")),
                   notify = TRUE, message_format = 'text')
      
    } else {
      hipchat_send('room', 'JARVIS', 'notification',
                   color = 'green', message = as.character('Jarvis sign up goal script failed to run @MitchellLisle'),
                   notify = TRUE, message_format = 'text')
      
    }
  }
  
  if(app == "Hipchat"){
    hipchat()
  } else if(app == "Slack"){
    slack()
  } else {
    message("Selected App not available. Please choose from either Slack or Hipchat")
  }
  
}