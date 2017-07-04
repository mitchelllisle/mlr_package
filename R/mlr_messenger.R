#' Simple function for sending messages to Hipchat and Slack dependant on success criteria.
#' 
#' @description These functions make it simple to send a message to and from either Slack
#' using the https://github.com/hrbrmstr/slackr package, or for Hipchat using the 
#' https://github.com/robertzk/hipchat package. You can set the criteria for whether the 
#' function should send a Success or Error message to a room of your choosing. No extra
#' env variables are needed as it works of the existing config for the packages mentioned
#' above. 
#'
#' @param app - Can be either 'Hipchat' or 'Slack'
#' #' @param channel - the room to send to. No need for # prefix on room name if 
#' using Slack
#' @param success_flag - Can be a variable already pre-stored in your script. To use an
#' existing value, make sure to leave it unquoted when passing to the function
#' @param success_value - How will the fuction know to send a Success or Error message? Uses
#' this value and evaluates it against success_flag.
#' @param success_message - The message to send if success_flag == success_value
#' @param fail_message - The message to send if success_flag != success_value
#'
#' @example mlr_messenger(app = "Hipchat", channel = "AppChat", success_flag = status, success_value = TRUE, success_message = "Success!", fail_message = "Fail!")

mlr_messenger <- function(app = "Hipchat", channel = "AppChat", success_flag = status, success_value = TRUE, success_message = "Success!", fail_message = "Failed!"){
  
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
    
    token <- Sys.getenv("HIPCHAT_API_TOKEN")
    
    if(token == ""){
      message("Please set a HIPCHAT_API_TOKEN in Environment variables before sending Hipchat message")
    } else {
      if(success_flag == success_value){
      hipchat::hipchat_send('room', channel, 'notification',
                     color = 'green', message = as.character(success_message),
                     notify = TRUE, message_format = 'text')
        
      } else {
        hipchat::hipchat_send('room', channel, 'notification',
                     color = 'red', message = as.character(fail_message),
                     notify = TRUE, message_format = 'text')
        
      }
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