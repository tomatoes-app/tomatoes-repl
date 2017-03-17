:set -XOverloadedStrings
import Network.HTTP.Client
manager <- newManager defaultManagerSettings
response <- createSession manager "XXX"

tomato <- createTomato manager "XXX" "one, two"

let request = initRequest {method = "POST", requestBody = RequestBodyBS $ "token=XXX&tomato[tag_list]=one, two"}


-- example github access token XXX
-- % auth github XXX
