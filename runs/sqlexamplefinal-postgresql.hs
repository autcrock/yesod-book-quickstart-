{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
import           Control.Monad.Logger
import           Data.Text               (Text)
import qualified Database.Esqueleto      as E
import           Database.Esqueleto      ((^.))
import           Database.Persist.Postgresql
import           Yesod
import qualified Data.Conduit.List as CL
import Data.Conduit (($=))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Author
    name Text
Blog
    author AuthorId
    title Text
    content Html
|]

data App = App
    { persistConfig :: PostgresConf
    , connPool      :: E.ConnectionPool
    }
instance Yesod App
instance YesodPersist App where
    type YesodPersistBackend App = E.SqlBackend
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

mkYesod "App" [parseRoutes|
/ HomeR GET
/blog/#BlogId BlogR GET
|]

getHomeR :: Handler TypedContent
getHomeR = do
    let blogsSrc =
             E.selectSource
           $ E.from $ \(blog `E.InnerJoin` author) -> do
                E.on $ blog ^. BlogAuthor E.==. author ^. AuthorId
                return
                    ( blog   ^. BlogId
                    , blog   ^. BlogTitle
                    , author ^. AuthorName
                    )

    render <- getUrlRenderParams
    respondSourceDB typeHtml $ do
        sendChunkText "<html><head><title>Blog posts</title></head><body><ul>"
        blogsSrc $= CL.map (\(E.Value blogid, E.Value title, E.Value name) ->
            toFlushBuilder $
            [hamlet|
                <li>
                    <a href=@{BlogR blogid}>#{title} by #{name}
            |] render
            )
        sendChunkText "</ul></body></html>"

getBlogR :: BlogId -> Handler Html
getBlogR _ = error "Implementation left as exercise to reader"

main :: IO ()
main = do
    let conf = PostgresConf "host=localhost port=5432 user=postgres dbname=postgres password=postgres" 10
    pool <- createPoolConfig conf
    flip runSqlPersistMPool pool $ do
        runMigration migrateAll

        -- Fill in some testing data
        alice <- insert $ Author "Alice"
        bob   <- insert $ Author "Bob"

        insert_ $ Blog alice "Alice's first post" "Hello World!"
        insert_ $ Blog bob "Bob's first post" "Hello World!!!"
        insert_ $ Blog alice "Alice's second post" "Goodbye World!"

    warp 3000 App
        { persistConfig = conf
        , connPool      = pool
        }
