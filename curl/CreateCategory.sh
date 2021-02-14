#! /bin/bash


curl -b  'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -d '{"idCategory":0,"nameCategory":"test2","parentCategory":{"idCategory":1,"nameCategory":"test1","parentCategory":null}}'  -X POST http://localhost:3000/category  -H «Content-Type:application/json» 


# '{"idCategory":0,"nameCategory":"test","parentCategory":{"idCategory":1,"nameCategory":"test2","parentCategory":null}}'