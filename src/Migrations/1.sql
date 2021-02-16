CREATE TABLE IF NOT EXISTS usernews (
    id_user SERIAL PRIMARY KEY,
    name_user text,
    lastname text,
    login_user text,
    password_user text,
    avatar_user text,
    datacreate_user timestamp with time zone,
    admin boolean,
    authoris boolean
);


CREATE TABLE IF NOT EXISTS author (
    id_author SERIAL PRIMARY KEY,
    id_link_user integer REFERENCES usernews(id_user) ON DELETE CASCADE,
    description text
);




CREATE TABLE IF NOT EXISTS news (
    id_news SERIAL PRIMARY KEY,
    data_creat_news timestamp with time zone,
    authors_id_news integer REFERENCES author(id_author),
    category_id_news integer,
    text_news text,
    main_photo_news text,
    other_photo_news text[],
    short_name_news text
);






CREATE TABLE IF NOT EXISTS category (
    id_category SERIAL PRIMARY KEY,
    parent_category integer DEFAULT 0 ,
    name_category character varying(100),
	FOREIGN KEY (parent_category) REFERENCES category (id_category) ON DELETE SET NULL
);




CREATE TABLE IF NOT EXISTS comment (
    id_comment SERIAL PRIMARY KEY,
    text_comment text,
    data_create_comment timestamp with time zone,
    news_id_comment integer REFERENCES news(id_news) ON DELETE CASCADE,
    user_id_comment integer REFERENCES usernews(id_user)
);



CREATE TABLE IF NOT EXISTS draft (
    id_draft SERIAL PRIMARY KEY,
    text_draft text,
    data_create_draft timestamp with time zone,
    news_id_draft integer REFERENCES news(id_news),
    main_photo_draft text,
    short_name_draft text,
    other_photo_draft text[],
    tags_id integer[],
    id_author_draft integer REFERENCES author(id_author)
);




CREATE TABLE IF NOT EXISTS tag (
    id_tag SERIAL PRIMARY KEY,
    name_tag text
);




CREATE TABLE IF NOT EXISTS tags_news (
    tags_id integer,
    news_id integer
);



CREATE TABLE IF NOT EXISTS session (
    id SERIAL PRIMARY KEY,
    key text,
    user_news_id integer,
    CONSTRAINT "IDENTITY(1,1)" CHECK (NULL::boolean)
);




CREATE OR REPLACE FUNCTION updeite_news(auth INT, dr INT) RETURNS Int AS
$$
BEGIN
					if count(*) <> 0 from draft where id_author_draft=auth and id_draft=dr and news_id_draft IS NULL  
						then
							WITH draft_temp as (SELECT * from draft where id_author_draft=auth and id_draft = dr)
							INSERT INTO  	news (data_creat_news, authors_id_news, category_id_news, text_news, main_photo_news, other_photo_news, short_name_news)
							SELECT 		data_create_draft, id_author_draft, 3,text_draft, main_photo_draft, other_photo_draft, short_name_draft   from draft_temp;
							return 1;
						else 
							if count(*) <> 0 from draft where id_author_draft=auth and id_draft=dr 
								then
									WITH draft_temp as (SELECT * from draft where id_author_draft=auth and id_draft = dr)
									UPDATE 	news SET data_creat_news = data_create_draft
													, authors_id_news = id_author_draft
													, category_id_news = 1
													, text_news = text_draft
													, main_photo_news = main_photo_draft
													, other_photo_news = other_photo_draft
													, short_name_news = short_name_draft FROM draft_temp where id_news= news_id_draft;
													return 1;
								else return 0;
							END IF;
					END IF;	
					
			
END;
$$
LANGUAGE plpgsql;

			
