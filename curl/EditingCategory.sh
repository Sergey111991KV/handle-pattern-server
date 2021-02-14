#! /bin/bash


curl -b  'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -d '{"idCategory":3,"nameCategory":"test2","parentCategory":{"idCategory":4,"nameCategory":"test1!!!","parentCategory":null}}'  -X PUT http://localhost:3000/category  -H «Content-Type:application/json» 


