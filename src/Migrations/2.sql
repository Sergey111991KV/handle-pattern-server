INSERT INTO usernews (name_user, lastname, login_user, password_user , avatar_user, datacreate_user, admin, authoris)
VALUES ('Pasha','Dragon','pasha@test.com','3456ABCDefgh','https://nlotv.com/ru/news/view/6554-novye-kadry-iz-avatar-2-predstavili-druguyu-lokaciyu-pandory','2001-09-29 00:00:00',true,true);

INSERT INTO usernews (name_user, lastname, login_user, password_user , avatar_user, datacreate_user, admin, authoris)
VALUES ('Igor','Kram','kram777@test.com','3456ABCDesdfds','emodgi','2001-09-29 00:00:00',false,true);

INSERT INTO usernews (name_user, lastname, login_user, password_user , avatar_user, datacreate_user, admin, authoris)
VALUES ('Oleg','Ax','oleg@test.com','5678ABCDefgh','some avatar','2001-09-29 00:00:00',true,false);

INSERT INTO usernews (name_user, lastname, login_user, password_user , avatar_user, datacreate_user, admin, authoris)
VALUES ('Fedor','Nikolaev','nikolaevXXX@mailtest.com','4678dBCJefgh','non pictures','2001-09-29 00:00:00',false,false);


INSERT INTO author (description, id_link_user) VALUES ('Советский шпион',1);
INSERT INTO author (description, id_link_user) VALUES ('Европейский маргинал',2);


INSERT INTO news (data_creat_news, authors_id_news, category_id_news, text_news, main_photo_news, other_photo_news,short_name_news) 
	VALUES ('2011-08-01 00:00:00',1,1,'some description for 1 news','main photo 1 news ','{" 1 photo", " 2 photo"}','news 1');
INSERT INTO news (data_creat_news, authors_id_news, category_id_news, text_news, main_photo_news, other_photo_news,short_name_news) 
	VALUES ('2011-09-19 00:00:00',1,2,'some description for 2 news','main photo 2 news ','{" 1 photo", " 2 photo"}','news 2');
INSERT INTO news (data_creat_news, authors_id_news, category_id_news, text_news, main_photo_news, other_photo_news,short_name_news) 
	VALUES ('2012-01-10 00:00:00',1,3,'some description for 3 news','main photo 3 news ','{" 1 photo", " 2 photo"}','news 3');
INSERT INTO news (data_creat_news, authors_id_news, category_id_news, text_news, main_photo_news, other_photo_news,short_name_news) 
	VALUES ('2013-09-11 00:00:00',1,4,'some description for 4 news','main photo 4 news ','{" 1 photo", " 2 photo"}','news 4');
INSERT INTO news (data_creat_news, authors_id_news, category_id_news, text_news, main_photo_news, other_photo_news,short_name_news) 
	VALUES ('2014-04-19 00:00:00',2,1,'some description for 5 news','main photo 5 news ','{" 1 photo", " 2 photo"}','news 5');
INSERT INTO news (data_creat_news, authors_id_news, category_id_news, text_news, main_photo_news, other_photo_news,short_name_news) 
	VALUES ('2014-05-22 00:00:00',2,2,'some description for 6 news','main photo 6 news ','{" 1 photo", " 2 photo"}','news 6');
INSERT INTO news (data_creat_news, authors_id_news, category_id_news, text_news, main_photo_news, other_photo_news,short_name_news) 
	VALUES ('2020-03-18 00:00:00',2,3,'some description for 7 news','main photo 7 news ','{" 1 photo", " 2 photo"}','news 7');
INSERT INTO news (data_creat_news, authors_id_news, category_id_news, text_news, main_photo_news, other_photo_news,short_name_news) 
	VALUES ('2001-04-09 00:00:00',2,4,'some description for 8 news','main photo 8 news ','{" 1 photo", " 2 photo"}','news 8');


INSERT INTO category (id_category,name_category,parent_category) VALUES ( 0,'Global News',DEFAULT);
INSERT INTO category (name_category,parent_category) VALUES ('America News',0);
INSERT INTO category (name_category,parent_category) VALUES ('Ney York News',1);
INSERT INTO category (name_category,parent_category) VALUES ('Europe News',2);
INSERT INTO category (name_category,parent_category) VALUES ('Germany',3);


INSERT INTO comment (text_comment, data_create_comment, news_id_comment, user_id_comment) 
	VALUES('дожились','2005-09-29 00:00:00',1,2); 
INSERT INTO comment (text_comment, data_create_comment, news_id_comment, user_id_comment) 
	VALUES('напрашивается анекдот','2011-09-19 00:00:00',1,3); 
INSERT INTO comment (text_comment, data_create_comment, news_id_comment, user_id_comment) 
	VALUES('так держать','2004-09-29 00:00:00',2,3); 
INSERT INTO comment (text_comment, data_create_comment, news_id_comment, user_id_comment) 
	VALUES('о боже мой','2002-09-29 00:00:00',2,3); 


INSERT INTO draft (text_draft, data_create_draft, news_id_draft, main_photo_draft, short_name_draft, other_photo_draft, tags_id, id_author_draft) 
	VALUES('some text draft for 1 news','2011-09-19 00:00:00',1,'main photo 1 news draft','draft1','{"draft 1 photo", "draft 2 photo"}','{1,2}',1);
INSERT INTO draft (text_draft, data_create_draft, news_id_draft, main_photo_draft, short_name_draft, other_photo_draft, tags_id, id_author_draft) 
	VALUES('some text draft for 2 news','2011-09-19 00:00:00',1,'main photo 1 news draft','draft1','{"draft 1 photo", "draft 2 photo"}','{2,3}',2);
INSERT INTO draft (text_draft, data_create_draft, news_id_draft, main_photo_draft, short_name_draft, other_photo_draft, tags_id, id_author_draft) 
	VALUES('some text draft for 1 news','2011-09-19 00:00:00',null,'main photo 1 news draft','draft1','{"draft 1 photo", "draft 2 photo"}','{4,5}',2);



INSERT INTO tag (name_tag) VALUES('Россия');
INSERT INTO tag (name_tag) VALUES('Франция');
INSERT INTO tag (name_tag) VALUES('Утрешние');
INSERT INTO tag (name_tag) VALUES('Звезды');
INSERT INTO tag (name_tag) VALUES('America');
INSERT INTO tag (name_tag) VALUES('Europe');
INSERT INTO tag (name_tag) VALUES('South America');
INSERT INTO tag (name_tag) VALUES('England');



INSERT INTO tags_news (tags_id,news_id) VALUES(1,1);
INSERT INTO tags_news (tags_id,news_id) VALUES(2,2);
INSERT INTO tags_news (tags_id,news_id) VALUES(2,3);
INSERT INTO tags_news (tags_id,news_id) VALUES(3,1);
INSERT INTO tags_news (tags_id,news_id) VALUES(3,3);
INSERT INTO tags_news (tags_id,news_id) VALUES(4,1);
INSERT INTO tags_news (tags_id,news_id) VALUES(4,2);
INSERT INTO tags_news (tags_id,news_id) VALUES(4,3);
INSERT INTO tags_news (tags_id,news_id) VALUES(5,1);
INSERT INTO tags_news (tags_id,news_id) VALUES(5,2);
INSERT INTO tags_news (tags_id,news_id) VALUES(6,1);
INSERT INTO tags_news (tags_id,news_id) VALUES(7,3);
INSERT INTO tags_news (tags_id,news_id) VALUES(7,3);
INSERT INTO tags_news (tags_id,news_id) VALUES(8,4);


INSERT INTO session (key, user_news_id) values ('OvSvZjTyT3E8F4cBhggjYjDEnOJnFU6v',1);


