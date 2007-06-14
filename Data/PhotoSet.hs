{- PhotoSet Component
Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
Please see COPYRIGHT for more details
-}

{- |
   Module     : Data.PhotoSet
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with strings.

Written by John Goerzen, jgoerzen\@complete.org
-}
{- |
   Module     : Data.PhotoSet
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org
-}

module Data.PhotoSet where

{- | The main class that defines a storage of photos. -}
class PhotoSet a where
    -- | Obtain a list of albums
    getAlbums :: Album b => a -> IO [b]

    -- | The location of the top of the store.  Probably a filesystem
    -- path for local items or a URL for remote protocols.
    psLocation :: a -> String

    -- | The driver for this store.
    psDriver :: a -> String

{- | The class to hold a collection of 'Photo's.  This may be a directory on
a filesystem, a set on Flickr, an album in digikam or gallery, etc. -}
class Album a where
    -- | The unique id for the album.  Implementation-specific.
    albumId :: a -> String
    -- | The short descriptive title for the album.
    albumTitle :: a -> String
    -- | The longer description of the album.
    albumDescription :: a -> String
    -- | The location of the album.  Probably a filesystem path for local
    -- items or a URL for remote protocols.
    albumLocation :: a -> String
    -- | Obtain a list of photos stored directly in this album.
    albumGetPhotos :: Photo b => a -> IO [b]

    -- | Update this album.  The id is to remain constant; the other
    -- items are updated to the data store to match as defined.

    albumUpdate :: a -> IO ()

data BasicAlbum = BasicAlbum {
    balbumId :: String,
    balbumTitle :: String,
    balbumDescription :: String,
    balbumLocation :: String,
    balbumGetPhotos :: IO [BasicPhoto],
    balbumUpdate :: IO () }

instance Album BasicAlbum where
    albumId = balbumId
    albumTitle = balbumTitle
    albumDescription = balbumDescription
    albumLocation = balbumLocation
    albumGetPhotos = balbumGetPhotos
    albumUpdate = balbumUpdate

instance Show BasicAlbum where
    show (BasicAlbum a b c d _ _) = show [a, b, c, d]

class Photo b where
    photoId :: b -> String
    photoLocation :: b -> String
    photoTitle :: b -> String

data BasicPhoto = BasicPhoto {
    bphotoId :: String,
    bphotoLocation :: String,
    bphotoTitle :: String}

instance Photo BasicPhoto where
    photoId = bphotoId
    photoLocation = bphotoLocation
    photoTitle = bphotoTitle

