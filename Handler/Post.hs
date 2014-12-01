module Handler.Post where

import Import
import qualified Data.Text as T
import Text.Hamlet (hamletFile)
import           Yesod.Auth
import           Yesod.Auth.Email


adminLayout widget = do
        master <- getYesod
        mmsg   <- getMessage 
        pc     <- widgetToPageContent $ do
            $(widgetFile "admin-layout")
        withUrlRenderer $(hamletFile "templates/admin-layout-wrapper.hamlet")

getPostR :: Handler Html
getPostR = adminLayout $ do
             setTitle "Write New Post"
             $(widgetFile "new_post")
           
-- instance Show Post where
--     show (Post title content) =  T.unpack $ T.concat ["标题:"::Text, title, "  内容"::Text, content]

postPostR :: Handler Html
postPostR = do 
             post <- runInputPost $ Post
                                    <$> ireq textField "title"
                                    <*> ireq textField "content"

             postId <- runDB $ insert post

             redirect $ PostsR

getPostsR :: Handler Html
getPostsR = do
             posts <- runDB $ selectList [] [Desc PostId]
             adminLayout $ do 
               $(widgetFile "posts")

getPostShowR :: PostId -> Handler Html
getPostShowR postId = do
                  maybePost <- runDB $ get postId
                  case maybePost of
                    Nothing -> defaultLayout $ do
                                   [whamlet| Nothing |]
                    Just post -> defaultLayout $ do
                                   [whamlet| #{postContent post} |]

deletePostShowR :: PostId -> Handler Value
deletePostShowR postId = do
   runDB $ delete postId
   return $ object [ "name" .= ("asd"::String), "age"  .= ("1"::String)]

{-------------------------------------------------------------------------------------------------------}
{-                                       授权                                                          -}
{-------------------------------------------------------------------------------------------------------}

