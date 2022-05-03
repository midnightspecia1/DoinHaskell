{-#LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe

type Html = T.Text
type Author = T.Text
type Title = T.Text

data Book = Book {
    author :: Author,
    title :: Title } deriving Show

-- little collection of books
book1 :: Book
book1 = Book {
    title = "The Conspiracy Against the Human Race",
    author = "Ligotti, Thomas" }

book2 :: Book
book2 = Book {
    title = "A Short History of Decay",
    author = "Cioran, Emil" }

book3 :: Book
book3 = Book {
    title = "The Tears of Eros",
    author = "Bataille, Georges" }

books :: [Book]
books = [book1, book2, book3]


-- this function creates an individual snippet fo the html from a book
bookToHtml :: Book -> Html
bookToHtml book = mconcat [ "<p>\n",
                            titleInTags,
                            authorInTags,
                            "<p>\n"]
    where titleInTags = mconcat ["<strong>", title book, "</strong>\n"]
          authorInTags = mconcat ["<em>", author book, "</em>\n"]

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat [ "<html>\n",
                              "<head><title>books</title>",
                              "<meta charset='utf-8'/>",
                              "/head>\n",
                              "<body>\n",
                              booksHtml,
                              "\n</body>\n",
                              "</html>"]
    where booksHtml = (mconcat . map bookToHtml) books

-- MARC files consist of three elements
-- leader - contains info about record itself
-- directory - tells where to find info in the record
-- base record - has the info but to navigate thru it we need directory

--creating type synonims for the record and leader to being able to iterate thru records
type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString
type MarcDirectoryRaw = B.ByteString
type MarcDirectoryEntryRaw = B.ByteString

-- the leader of the record is first 24 bytes so declaring constant length of the leader and making getter
leaderLen :: Int
leaderLen = 24

dirEntryLength :: Int
dirEntryLength = 12

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader record = B.take leaderLen record

-- first 5 bytes of the leader contain a lengths of the record
rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength leader = rawToInt (B.take 5 leader)

-- function that would be separating input bytestring to record and rest bytestring 
-- to iterate thru a stream of records we recursively call this func
nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
            where recordLength = getRecordLength marcStream

-- converting stream of raw data into list of records 
allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == B.empty
                        then []
                        else next : allRecords rest
        where (next, rest) = nextAndRest marcStream

-- reading the directory
-- to know how long is directory we need to know where base starts (directory start is the ledaer end)
getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress leader = rawToInt (B.take 5 remainder)
        where remainder = B.drop 12 leader

-- calculating directory length
getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLen + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLen afterLeader
        where directoryLen = getDirectoryLength record
              afterLeader = B.drop leaderLen record

-- now when we have directory we can lookup for the fields (each 12 bytes)
-- need to split up directory to entrys
splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory = if directory == B.empty
                           then []
                           else nextEntry : splitDirectory rest
        where (nextEntry, rest) = B.splitAt 12 directory

-- each field in directory stores metadata consisting of 
-- tag of the field (3 chars)
-- length of the field ( next 4 chars)
-- where the field start relative to the base address (rest)

data FieldMetadata = FieldMetadata {tag         :: T.Text,
                                    fieldLength :: Int,
                                    fieldStart  :: Int } deriving Show

-- processing MarcDirectoryEntryRaw into a list of FieldMetaData
makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag theLength theStart
        where (theTag, rest) = B.splitAt 3 entry
              textTag = E.decodeUtf8 theTag
              (rawLength, rawStart) = B.splitAt 4 rest
              theLength = rawToInt rawLength
              theStart = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata rawEntries = map makeFieldMetadata rawEntries

-- now we can write function that lets us to lookup the field itself
type FieldText = T.Text

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record fieldMeta = E.decodeUtf8 byteStringValue
        where baseAddress = getBaseAddress record
              baseRecord = B.drop baseAddress record
              baseAtEntry = B.drop (fieldStart fieldMeta) baseRecord
              byteStringValue = B.take (fieldLength fieldMeta) baseAtEntry

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

--tags and subfield codes for author and title
titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

-- check either our fields exist with Maybe
lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record = if null results
                                  then Nothing
                                  else Just (head results)
        where metadata = (getFieldMetadata . splitDirectory . getDirectory) record
              results = filter ((== aTag) . tag) metadata

lookupSubfield :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing _ _ = Nothing 
lookupSubfield (Just fieldMetadata) subfield record = if null results
                                                      then Nothing
                                                      else Just ((T.drop 1 . head) results)
        where rawField = getTextField record fieldMetadata
              subfields = T.split ( == fieldDelimiter) rawField
              results = filter ((== subfield) . T.head) subfields

-- next is the general lookupValue function for looking up tag-subfield code pairs
lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text 
lookupValue aTag subfield record = lookupSubfield entryMetadata subfield record
        where entryMetadata = lookupFieldMetadata aTag record

-- making two wraper function for author and tittle 
lookupTitle :: MarcRecordRaw -> Maybe Title
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe Author
lookupAuthor = lookupValue authorTag authorSubfield

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
        where records = allRecords marcStream
              titles = map lookupTitle records
              authors = map lookupAuthor records

-- converting Maybe pairs into books
pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map (\(title, author) -> Book { title = fromJust title,
                                                     author = fromJust author}) justPairs
        where justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

-- putting all together
processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs

--temp IO
main :: IO ()
main = do
    marcData <- B.readFile "sample.mrc"
    let processed = processRecords 500 marcData
    TIO.writeFile "books.html" processed
