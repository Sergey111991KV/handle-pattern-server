#! /bin/bash



# curl -b  'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -X DELETE http://localhost:3000/api/delete/draft/2 
# error as not access

curl -b  'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -X DELETE http://localhost:3000/draft/2 
# good