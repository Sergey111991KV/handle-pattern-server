#! /bin/bash


curl -b  'sId=OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v' -d '{"tagsId":{"fromPGArray":[1,2]},"idAuthorDraft":1,"mainPhotoUrl":"Test mainPhotoUrl","newsIdDraft":null,"textDraft":"Test textDraft","shortNameDraft":"jjj","otherPhotoUrl":{"fromPGArray":["Test Avatar"]},"dataCreateDraft":"2015-09-01T13:34:02Z","idDraft":1}'  -X POST http://localhost:3000/draft  -H «Content-Type:application/json» 


