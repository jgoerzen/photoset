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

class (Photo b) => Album a b where
    albumId :: a -> String
    albumTitle :: a -> String
    albumDescription :: a -> String
    albumLocation :: a -> String
    albumGetPhotos :: a -> IO [b]

    -- | Update this album.  The id is to remain constant; the other
    -- items are updated to the data store to match as defined.

    albumUpdate :: a -> IO ()

class Photo b where
    photoId :: b -> String


