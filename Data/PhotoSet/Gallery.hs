{- PhotoSet Component
Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
Please see COPYRIGHT for more details
-}

{- |
   Module     : Data.PhotoSet.Gallery
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

This module provides various helpful utilities for dealing with strings.

Written by John Goerzen, jgoerzen\@complete.org
-}
{- |
   Module     : Data.PhotoSet.Gallery
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : GNU GPL, version 2 or above

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org
-}

module Data.PhotoSet.Gallery where

import Network.URI
import Network.HTTP
import Data.String
import Data.List
import Data.Maybe.Utils
import Data.PhotoSet

data GalleryRemote = GalleryRemote {
        baseURL :: String
        }
        deriving (Eq, Show)

data GalleryAlbum = GalleryAlbum [(String, String)]
        deriving (Eq, Show)

instance PhotoSet GalleryRemote BasicAlbum BasicPhoto where
    psLocation = baseURL
    psDriver _ = "GalleryRemote"
    getAlbums gr =
        do r <- sendRequest gr [("cmd", "fetch-albums")]
           let albumcount = read (forceLookup "album_count" r)
           let results = (map (convalbum r) [1..albumcount])
           print r
           print results
           return results
        where convalbum r i =
                  BasicAlbum {
                      balbumId = getalb r "name" i,
                      balbumTitle = getalb r "title" i,
                      balbumDescription = getalb r "summary" i,
                      balbumGetPhotos = fail "not implemented",
                      balbumUpdate = fail "not implemented",
                      balbumLocation = baseURL gr ++ "?g2_itemId=" ++ getalb r "name" i}

              getalb r key i = forceLookup ("album." ++ key ++ "." ++ show i) r
                  

forceLookup key list =
    forceMaybeMsg (show key) (lookup key list)

contentType = "application/x-www-form-urlencoded"
protoVer = "2.3"

createGR :: String -> GalleryRemote
createGR = GalleryRemote

url2uri uri = 
    case parseURI uri of
         Nothing -> error $ "Invalid remote URL: " ++ uri
         Just x -> x

sendRequest :: GalleryRemote -> [(String, String)] -> IO [(String, String)]
sendRequest gr params =
    do r <- simpleHTTP $ Request url POST 
                         [Header HdrContentType contentType,
                          Header HdrContentLength (show (length formdata))]
                         formdata
       case r of
            Left ce -> fail (show ce)
            Right resp ->
                   case rspCode resp of
                     (2, _, _) -> validateStatus (parseResult $ rspBody resp)
                     _ -> fail $ "Bad HTTP result: " ++ show (rspCode resp) ++
                                  ": " ++ rspReason resp
                         
    where formdata = urlEncodeVars . 
                     (++) [("g2_controller", "remote:GalleryRemote")] .
                     map (\(x, y) -> ("g2_form[" ++ x ++ "]", y)) $
                     (params ++ [("protocol_version", "2.3")])
          url = url2uri (baseURL gr)
          validateStatus res =
              case lookup "status" res of
                   Nothing -> fail $ "Missing status in result: " ++ show res
                   Just s -> if s == "0"
                                then return res
                                else fail $ "Bad status: " ++ s ++ ", result packet: " ++ show res
          parseResult = 
              map convline .
              filter (/= "") .
              map strip .
              tail . dropWhile (\l -> not (isPrefixOf "#__GR2PROTO__" l)) .
              lines
          convline l = 
              case elemIndex '=' l of
                   Nothing -> error $ "Invalid response line: " ++ (show l)
                   Just i -> (take i l, drop (i + 1) l)

