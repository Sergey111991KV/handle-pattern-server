module Database.DatabaseCommon where

import Database.ImportDatabase
import ClassyPrelude 


data Config = Config
    { configUrl ::  ByteString
    , configStripeCount :: Int
    , configMaxOpenConnPerStripe :: Int
    , configIdleConnTimeout :: NominalDiffTime
    } deriving (Show)


withPool :: Config -> (Pool Connection -> IO a)  -> IO a
withPool cfg  = Database.ImportDatabase.bracket initPool cleanPool 
  where
    initPool =
      createPool
        openConn
        closeConn
        (configStripeCount cfg)
        (configIdleConnTimeout cfg)
        (configMaxOpenConnPerStripe cfg)
    cleanPool = destroyAllResources
    openConn = connectPostgreSQL (configUrl cfg)
    closeConn = close



data Handle = Handle
    { hConfig :: Config
    , hPool   :: Pool Connection
    }

withHandle :: Config -> (Database.DatabaseCommon.Handle -> IO a) -> IO a
withHandle cfg action = do
     withPool cfg $ \pool -> do
        -- migrate state
        action $ Handle cfg pool


type PG  m
   = (Monad m, MonadIO m, MonadError ErrorServer m)

withConn :: PG  m => Pool Connection -> (Connection -> IO a) -> m a
withConn pool action = do
  liftIO . withResource pool $ \conn -> action conn

requestForPost :: Query
requestForPost =
  " select    endNews.id_news \
				                        \ , endNews.data_creat_news \
				                        \ , endNews.id_author \
				                        \ , endNews.id_link_user \
				                        \ , endNews.description \
				                        \ , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( \
                                \  select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path \
                                \  from news, category t1 where t1.id_category = endnews.category_id_news \
                                \ union \
                                \ select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) \
                                \ from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) \
                                \ select distinct (id_category, name_category, parent_category) from temp1) \
                                \ , endNews.text_news \
				                        \ , endNews.main_photo_news \
				                        \ , endNews.other_photo_news \
				                        \ , endNews.short_name_news \
                                \ , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) \
				                        \ , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL) as t) \
				                        \ from (select * from news left join author on author.id_author = news.authors_id_news ) as endNews "


requestForPostFilterTag :: Query
requestForPostFilterTag =
  " select  distinct  endNews.id_news \
				                        \ , endNews.data_creat_news \
				                        \ , endNews.id_author \
				                        \ , endNews.id_link_user \
				                        \ , endNews.description \
				                        \ , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( \
                                \  select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path \
                                \  from news, category t1 where t1.id_category = endnews.category_id_news \
                                \ union \
                                \ select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) \
                                \ from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) \
                                \ select distinct (id_category, name_category, parent_category) from temp1) \
                                \ , endNews.text_news \
				                        \ , endNews.main_photo_news \
				                        \ , endNews.other_photo_news \
				                        \ , endNews.short_name_news \
                                \ , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) \
				                        \ , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL) as t) \
				                        \ from tags_news, (select * from news left join author on author.id_author = news.authors_id_news ) as endNews "


requestForPostAllFilterTag :: Query
requestForPostAllFilterTag =
  " select  distinct  endNews.id_news \
				                        \ , endNews.data_creat_news \
				                        \ , endNews.id_author \
				                        \ , endNews.id_link_user \
				                        \ , endNews.description \
				                        \ , ARRAY(with recursive temp1 (id_category, parent_category, name_category) as ( \
                                \  select t1.id_category, t1.parent_category, t1.name_category, cast (t1.name_category as varchar (50)) as path \
                                \  from news, category t1 where t1.id_category = endnews.category_id_news \
                                \ union \
                                \ select t2.id_category, t2.parent_category, t2.name_category, cast (temp1.path || '->'|| t2.name_category as varchar(50)) \
                                \ from category t2 inner join temp1 on (temp1.parent_category = t2.id_category)) \
                                \ select distinct (id_category, name_category, parent_category) from temp1) \
                                \ , endNews.text_news \
				                        \ , endNews.main_photo_news \
				                        \ , endNews.other_photo_news \
				                        \ , endNews.short_name_news \
                                \ , ARRAY(select ( id_comment, text_comment,data_create_comment,news_id_comment,user_id_comment) from comment where endNews.id_news = comment.news_id_comment) \
				                        \ , ARRAY(select ( id_tag, name_tag) from (select * from tags_news left join  tag on tag.id_tag = tags_news.tags_id and tags_news.news_id = endNews.id_news   WHERE tag.id_tag IS not NULL) as t) \
				                        \ from  (select * from (select news_id,  row(array_agg(distinct tags_id)) as d from \
 				                        \ tags_news  GROUP BY news_id  ) as e  right join (select * from news left join author on author.id_author = news.authors_id_news ) as n ON e.news_id = n.id_news) as endNews \
                                \ where (endNews.d :: text) LIKE ((?) :: text) limit 20;"






