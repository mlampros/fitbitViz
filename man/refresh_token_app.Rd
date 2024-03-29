% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fitbit_utils.R
\name{refresh_token_app}
\alias{refresh_token_app}
\title{Refresh Token of an existing application}
\usage{
refresh_token_app(client_id, client_secret, refresh_token)
}
\arguments{
\item{client_id}{a character string specifying the 'client_id' of the registered (existing) Fitbit application}

\item{client_secret}{a character string specifying the 'client_secret' of the registered (existing) Fitbit application}

\item{refresh_token}{a character string specifying the 'refresh_token' of the registered (existing) Fitbit application}
}
\value{
a named list that includes access_token, expires_in, refresh_token, scope, token_type, user_id
}
\description{
Refresh Token of an existing application
}
\details{
A registered Fitbit application has a time limit of 8 hours. Therefore, the user has to refresh the token after the expiration using the 'client_id', 'client_secret' and 'refresh_token' that it's available for the registered application.
Based on the Fitbit API Documentation "After the Access Token expiration time has passed your requests will receive a 401 HTTP error. When this happens, your app should use the Refresh Token to get a new pair of tokens"
}
\examples{

\dontrun{

require(fitbitViz)

# client id, client secret and refresh token of
# the existing Fitbit Application
Client_ID = 'xxxxxx'
Client_SECRET = 'xxxxxxxxxxxxxxxxxx'
Refresh_TOKEN = 'xxxxxxxxxxxxxxxxxxxxxxxx'

# refresh the token
res_token = refresh_token_app(client_id = Client_ID,
                              client_secret = Client_SECRET,
                              refresh_token = Refresh_TOKEN)

res_token

# use the updated token to a function

USER_ID = '99xxxx'
new_TOKEN = res_token$access_token,

res_type = fitbit_data_type_by_date(user_id = USER_ID,
                                    token = new_TOKEN,
                                    date = '2022-10-12',
                                    type = 'spo2',
                                    show_nchar_case_error = 135)
}
}
