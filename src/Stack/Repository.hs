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
import           Path
import           Path.IO (createTree)

class Repository r where
    cacheRepo :: (MonadIO m, MonadThrow m, MonadCatch m)
              => CacheLoc -> UseNetwork -> r -> m ()
    cacheRepo _ _ _ = throwM (NotSupported FullCache)
    cacheRepoFile :: (MonadIO m, MonadThrow m, MonadCatch m)
                  => CacheLoc -> UseNetwork -> r -> Path Rel File -> m ()
    cacheRepoFile cache online repo _ = cacheRepo cache online repo
    repoType :: r -> String
    -- ^ A short, one-word, lowercase word describing the repo type.
    -- It should not contain anything but lowercase ASCII letters
    fullUri :: r -> String
    -- ^ Whatever String best uniquely represents the URL of the remote
    -- repository that's being referred to. It should not include
    -- information about what specific version is being referred to.
    parseRepo :: (MonadThrow m)
              => String
              -- ^ The URL, in whatever format the repository likes
              -> Maybe String
              -- ^ A version, if a specific one is desired other than
              -- whatever default makes sense with the repository
              -> m r
    {-# MINIMAL repoType
              , fullUri
              , parseRepo
              , (cacheRepo | cacheRepoFile) #-}

cacheTo' :: (MonadThrow m, Repository r) => r -> m (Path Rel Dir)
cacheTo' = undefined

cacheTo :: (MonadThrow m, Repository r) => CacheLoc -> r -> m (Path Abs Dir)
cacheTo (DirectlyIn a) _ = return a
cacheTo (OrganizeIn a) r = (a </>) <$> cacheTo' r

data CacheLoc = DirectlyIn (Path Abs Dir) | OrganizeIn (Path Abs Dir)
    deriving (Eq, Ord, Show)

data UseNetwork = Online | Offline
    deriving (Eq, Ord, Show, Enum, Bounded)

data RepositoryException
        = NotSupported Feature
        | RemoteNotCorrectType String
        | ToolNotAvailable
        | NotCached
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
    cacheRepo cache _ repo =
      do createCacheTree cache repo

createCacheTree :: (MonadIO m, MonadThrow m, Repository r)
                => CacheLoc -> r -> m ()
createCacheTree cache repo = cacheTo cache repo >>= createTree
