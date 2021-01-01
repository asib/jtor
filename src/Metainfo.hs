module Metainfo where

import BEncode
import BEncode.Internal
import qualified Data.Map as M
import Path
import Network.URI (URI, parseURI)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import qualified Data.ByteString as BS
import Data.ByteString.Char8 (ByteString, unpack)
import Control.Lens
import Control.Applicative

data Metainfo = Metainfo { _info :: Info
                         , _announce :: Either URI [[URI]]
                         } deriving (Show)

data Info = Info { _pieceLength :: Integer
                 , _pieces :: ByteString
                 , _fileModeInfo :: FileModeInfo
                 } deriving (Show)

data FileModeInfo = SingleFileMode { _name :: ByteString
                                   , _length :: Integer
                                   }
                  | MultiFileMode { _name :: ByteString
                                  , _files :: [FileInfo]
                                  }
                  deriving (Show)

data FileInfo = FileInfo { _lengthMulti :: Integer
                         , _path :: [ByteString]
                         } deriving (Show)

$(makeLenses ''Metainfo)
$(makeLenses ''Info)
$(makePrisms ''FileModeInfo)
$(makeLenses ''FileModeInfo)
$(makeLenses ''FileInfo)

parseSingleFileMode :: BValue -> Maybe FileModeInfo
parseSingleFileMode v = SingleFileMode <$> n <*> l
  where f = fetch v
        n = f "name" _BString
        l = f "length" _BInt

parseMultiFileMode :: BValue -> Maybe FileModeInfo
parseMultiFileMode v = MultiFileMode <$> n <*> fs
  where f = fetch v
        n = f "name" _BString
        fs = f "files" _BList >>= mapM parseFile

parseFile :: BValue -> Maybe FileInfo
parseFile v = FileInfo <$> l <*> p
  where f = fetch v
        l = f "length" _BInt
        p = f "path" _BList >>= mapM (^? _BString)

parseInfo :: BValue -> Maybe Info
parseInfo v = Info <$> pl <*> ps <*> fmi
  where f = fetch v
        pl = f "piece length" _BInt
        ps = f "pieces" _BString
        fmi = (parseSingleFileMode v) <|> (parseMultiFileMode v)

parseMetainfo :: BValue -> Maybe Metainfo
parseMetainfo v = Metainfo <$> i <*> (Left <$> a <|> Right <$> al)
  where f = fetch v
        i = f "info" _BDict >>= parseInfo . (_BDict #)
        a = f "announce" _BString >>= parseURI . unpack
        al = f "announce-list" _BList >>= mapM (^? _BList) >>= mapM (mapM ((parseURI =<<) . fmap unpack . (^? _BString)))

readTorrentFile :: Path Abs File -> IO (Maybe Metainfo)
readTorrentFile = (return . decodeAndParse =<<) . BS.readFile . toFilePath
  where decodeAndParse = (parseMetainfo =<<) . decodeMaybe
