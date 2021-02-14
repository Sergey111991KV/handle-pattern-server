#! /bin/bash


curl -b  'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -d "{\"userIsAdmin\":true,\"userLogin\":{\"loginRaw\":\"Test Login\"},\"lastName\":\"Test lastName\",\"userPassword\":{\"passwordRaw\":\"Test Password\"},\"nameUser\":\"Test Name\",\"idUser\":{\"userIdRaw\":2},\"dataCreate\":\"2015-09-01T13:34:02Z\",\"userIsAuthor\":false,\"avatar\":\"Test Avatar\"}"  -X PUT http://localhost:3000/user  -H «Content-Type:application/json» 

