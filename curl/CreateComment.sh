#! /bin/bash


curl -b  'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -d  '{"usersIdComments":{"userIdRaw":1},"newsIdComments":1,"idComment":0,"dataCreateComments":"2011-11-19T18:28:52.607875Z","textComments":"test comment4"}'  -X POST http://localhost:3000/comment  -H «Content-Type:application/json» 


