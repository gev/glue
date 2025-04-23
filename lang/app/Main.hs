import Data.ByteString (readFile)
import Data.Foldable
import Data.IORef
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import Database.SQLite.Simple
import Database.SQLite.Simple.QQ
import Lang
import Xeno.SAX
import Prelude hiding (id, readFile)

createLemmasTable :: Query
createLemmasTable =
    [sql|
        CREATE TABLE lemmas
            (id INTEGER PRIMARY KEY, text TEXT)
    |]

createLemmasTableTextIndex :: Query
createLemmasTableTextIndex =
    [sql|
        CREATE INDEX lemmas_text_idx 
            ON lemmas(text)
    |]

createFormsTable :: Query
createFormsTable =
    [sql|
        CREATE TABLE forms
            (lemma INTEGER, text TEXT)
    |]

createFormsLemmaIndex :: Query
createFormsLemmaIndex =
    [sql|
        CREATE INDEX forms_lemma_idx 
            ON forms(lemma)
    |]

createLinksTable :: Query
createLinksTable =
    [sql|
        CREATE TABLE links
            (from_lemma INTEGER, to_lemma INTEGER)
    |]

createLinksFromLemmaIndex :: Query
createLinksFromLemmaIndex =
    [sql|
        CREATE INDEX links_from_lemma_idx 
            ON links(from_lemma)
    |]

createLinksToLemmaIndex :: Query
createLinksToLemmaIndex =
    [sql|
        CREATE INDEX links_to_lemma_idx 
            ON links(to_lemma)
    |]

insertLemma :: Query
insertLemma =
    [sql|
        INSERT INTO lemmas (id, text)
        VALUES (?, ?)
    |]

insertForm :: Query
insertForm =
    [sql|
        INSERT INTO forms (lemma, text)
        VALUES (?, ?)
    |]

insertLink :: Query
insertLink =
    [sql|
        INSERT INTO links (from_lemma, to_lemma)
        VALUES (?, ?)
    |]

storeLemma :: Connection -> Lemma -> IO ()
storeLemma db lemma = do
    execute db insertLemma (lemma.id, lemma.lemma.text)
    traverse_ (storeForm db lemma.id) lemma.forms

storeForm :: Connection -> Int -> Form -> IO ()
storeForm db id form = execute db insertForm (id, form.text)

storeLink :: Connection -> Link -> IO ()
storeLink db link = execute db insertLink (link.from, link.to)

main :: IO ()
main = do
    db <- open "./var/lang/ru.db"
    traverse_
        (execute_ db)
        [ createLemmasTable
        , createLemmasTableTextIndex
        , createFormsTable
        , createFormsLemmaIndex
        , createLinksTable
        , createLinksFromLemmaIndex
        , createLinksToLemmaIndex
        ]
    id <- newIORef Nothing
    text <- newIORef Nothing
    forms <- newIORef []
    grammemes <- newIORef []
    from <- newIORef Nothing
    to <- newIORef Nothing
    type_ <- newIORef Nothing
    input <- readFile "./tmp/dict.opcorpora.xml"
    withTransaction db $
        process
            Process
                { openF = \name ->
                    if
                        | name == "lemma" -> do
                            writeIORef id Nothing
                            writeIORef forms []
                        | name == "l" || name == "f" -> do
                            writeIORef text Nothing
                            writeIORef grammemes []
                        | name == "link" -> do
                            writeIORef id Nothing
                            writeIORef from Nothing
                            writeIORef to Nothing
                            writeIORef type_ Nothing
                        | otherwise -> pure ()
                , attrF = \key value ->
                    if
                        | key == "id" ->
                            writeIORef id . Just . read . unpack $ decodeUtf8 value
                        | key == "from" ->
                            writeIORef from . Just . read . unpack $ decodeUtf8 value
                        | key == "to" ->
                            writeIORef to . Just . read . unpack $ decodeUtf8 value
                        | key == "type" ->
                            writeIORef type_ . Just . read . unpack $ decodeUtf8 value
                        | key == "t" ->
                            writeIORef text $ Just value
                        | key == "v" -> do
                            grammemes' <- readIORef grammemes
                            writeIORef grammemes $ grammemes' <> [value]
                        | otherwise -> pure ()
                , endOpenF = skip
                , textF = skip
                , closeF = \name ->
                    if
                        | name == "l" || name == "f" -> do
                            text' <- fromJust <$> readIORef text
                            grammemes' <- readIORef grammemes
                            forms' <- readIORef forms
                            let form =
                                    Form
                                        { text = decodeUtf8 text'
                                        , grammemes = decodeUtf8 <$> grammemes'
                                        }
                            writeIORef forms $ forms' <> [form]
                        | name == "lemma" -> do
                            id' <- fromJust <$> readIORef id
                            lemma' : forms' <- readIORef forms
                            storeLemma
                                db
                                Lemma
                                    { id = id'
                                    , lemma = lemma'
                                    , forms = forms'
                                    }
                        | name == "link" -> do
                            id' <- fromJust <$> readIORef id
                            from' <- fromJust <$> readIORef from
                            to' <- fromJust <$> readIORef to
                            type' <- fromJust <$> readIORef type_
                            storeLink
                                db
                                Link
                                    { id = id'
                                    , from = from'
                                    , to = to'
                                    , type_ = type'
                                    }
                        | otherwise -> pure ()
                , cdataF = skip
                }
            input
  where
    skip = const $ pure ()
