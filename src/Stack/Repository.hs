-- |
-- Module      :  Stack.Repository
-- Copyright   :  Joshua Simmons <joshua.simmons@emptypath.com> 2015
-- License     :  BSD3
--
-- An abstraction of a repository of files that may be versioned and can be
-- usefully locally cached.
--
module Stack.Repository where

import           Control.Monad.Catch (MonadCatch, MonadThrow, Exception, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text (Text)
import qualified Data.Text.IO as T
import           Path

class Repository r where
    cacheRepo :: (MonadIO m, MonadThrow m, MonadCatch m)
              => CacheLoc -> r -> m ()
    cacheRepo _ _ = throwM (NotSupported FullCache)
    cacheRepoFile :: (MonadIO m, MonadThrow m, MonadCatch m)
                  => CacheLoc -> r -> Path Rel File -> m Text
    cacheRepoFile cache repo file = do cacheRepo cache repo
                                       dir <- cacheTo cache repo
                                       liftIO $ T.readFile
                                                     (toFilePath $ dir </> file)
    repoType :: r -> String
    fullUri :: r -> String
    parseRepo :: (MonadThrow m)
              => String -> Maybe String -> m r

cacheTo' :: (MonadThrow m, Repository r) => r -> m (Path Rel Dir)
cacheTo' = undefined

cacheTo :: (MonadThrow m, Repository r) => CacheLoc -> r -> m (Path Abs Dir)
cacheTo (DirectlyIn a) _ = return a
cacheTo (OrganizeIn a) r = (a </>) <$> cacheTo' r

data CacheLoc = DirectlyIn (Path Abs Dir) | OrganizeIn (Path Abs Dir)
    deriving (Eq, Ord, Show)

data RepositoryException
        = NotSupported Feature
        | RemoteNotCorrectType String
        | ToolNotAvailable
    deriving (Eq, Ord, Show)

instance Exception RepositoryException

data Feature = FullCache
    deriving (Eq, Ord, Show)

data GitRepo = GitRepo String         -- ^ URL of the remote repository
                       (Maybe String) -- ^ Version to use, if not the default
    deriving (Eq, Ord, Show)

data HttpRepo = HttpRepo String
    deriving (Eq, Ord, Show)

instance Repository HttpRepo where
    repoType _ = "rawhttp"
    fullUri (HttpRepo s) = s
    parseRepo url _ = return $ HttpRepo url

instance Repository GitRepo where
    repoType _ = "git"
    fullUri (GitRepo s _) = s
    parseRepo url ver = return $ GitRepo url ver
