module Handler.Post where

import Import
import qualified Data.Text as T

getPostR :: Handler Html
getPostR = defaultLayout $ do
             setTitle "Write New Post"
             $(widgetFile "new_post")
           
instance Show Post where
    show (Post title content) =  T.unpack $ T.concat ["标题"::Text, title, "内容"::Text, content]

postPostR :: Handler Html
postPostR = do 
             post <- runInputPost $ Post
                                    <$> ireq textField "title"
                                    <*> ireq textField "content"

             defaultLayout [whamlet|<p> #{show post}|]
