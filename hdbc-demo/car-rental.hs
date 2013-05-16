import Graphics.UI.Gtk
import Database.HDBC.Sqlite3
import Database.HDBC


data Client = Client { clientId :: Int
                     , clientName :: String
                     , clientPhone :: Integer
                     , clientEmail :: String
                     , clientAddress :: String
                     }

clientFromSqlRow :: [SqlValue] -> Client
clientFromSqlRow [cid, cname, cphone, cemail, caddr] =
    Client { clientId = fromSql cid
           , clientName = fromSql cname
           , clientPhone = fromSql cphone
           , clientEmail = fromSql cemail
           , clientAddress = fromSql caddr
           }

clientToSqlRow :: Client -> [SqlValue]
clientToSqlRow client = [ toSql $ clientId client
                        , toSql $ clientName client
                        , toSql $ clientPhone client
                        , toSql $ clientEmail client
                        , toSql $ clientAddress client
                        ]


data Car = Car { carId :: Int
               , carBrand :: String
               , carCapacity :: Int
               , carEngineType :: String
               , carMaxPeople :: Int
               }

carFromSqlRow :: [SqlValue] -> Car
carFromSqlRow [cid, cbrand, ccap, cetype, cmaxp] =
    Car { carId = fromSql cid
        , carBrand = fromSql cbrand
        , carCapacity = fromSql ccap
        , carEngineType = fromSql cetype
        , carMaxPeople = fromSql cmaxp
        }

carToSqlRow :: Car -> [SqlValue]
carToSqlRow car = [ toSql $ carId car
                  , toSql $ carBrand car
                  , toSql $ carCapacity car
                  , toSql $ carEngineType car
                  , toSql $ carMaxPeople car
                  ]


loginClient :: IConnection conn => conn
                                -> String -> String
                                -> IO (Maybe Client)
loginClient conn login pass = do
    stmt <- prepare conn query
    execute stmt [toSql login, toSql pass]
    
    row <- fetchRow stmt
    return $ row >>= (Just . clientFromSqlRow)
  where
    -- W naszym świecie loginem i hasłem są email i numer telefonu.
    query = "SELECT * FROM client WHERE email = ? AND phone = ?"


addClient :: IConnection conn => conn -> Client -> IO ()
addClient conn client = do
    stmt <- prepare conn query
    execute stmt . tail . clientToSqlRow $ client
    return ()
  where
    query = "INSERT INTO client (name, phone, email, adress) " ++
            "VALUES (?, ?, ?, ?)"

addCar :: IConnection conn => conn -> Car -> IO ()
addCar conn car = do
    stmt <- prepare conn query
    execute stmt . tail . carToSqlRow $ car
    return ()
  where
    query = "INSERT INTO car (brand, capacity, type, maxp) " ++
            "VALUES (?, ?, ?, ?)"


showStartScreen :: IConnection conn => conn -> IO ()
showStartScreen conn = do
    window <- windowNew
    vbox   <- vBoxNew True 10
    
    -- Do wybierania rodzaju logowania.
    loginType <- comboBoxEntryNewText
    store     <- comboBoxEntrySetModelText loginType
    mapM_ (listStoreAppend store) ["admin", "klient", "gość", "recepcja"]

    -- Dane do logowania (tylko w przypadku admina i klienta).
    loginEntry <- entryNew
    passEntry  <- entryNew

    set passEntry [ entryVisibility := False ] -- Kontrolka "zahaślona".

    -- Przycisk do zalogowania.
    loginButton <- buttonNewWithLabel "Zaloguj"

    -- Ustawiamy obsługę zdarzeń.
    onDestroy window mainQuit

    onClicked loginButton $ do
        value <- comboBoxGetActiveText loginType
        login <- entryGetText loginEntry
        pass  <- entryGetText passEntry
        case value of
            Just "admin"    ->
                if login == "admin" && pass == "admin" then do
                    widgetHideAll window
                    showAdminScreen conn
                else
                    return ()
            Just "klient"   -> do
                mclient <- loginClient conn login pass
                case mclient of
                    Just client -> showClientScreen conn client
                    _         -> return ()
            Just "gość"     ->
                showGuestScreen conn
            Just "recepcja" ->
                showReceptionScreen conn
            _ ->
                return ()

    let setEditability val = do
        set loginEntry [ entryEditable := val ]
        set passEntry [ entryEditable := val ]

    -- Domyślnie zablokuj kontrolki.
    setEditability False
    -- Zablokuj lub odblokuj kontrolki w zależności od typu logowania.
    on loginType changed $ do
        value <- comboBoxGetActiveText loginType
        case value of
            Just "admin"  -> setEditability True
            Just "klient" -> setEditability True
            _             -> setEditability False

    -- Ładujemy wszystko do okna i wyświetlamy
    boxPackStart vbox loginType PackGrow 0
    boxPackStart vbox loginEntry PackGrow 0
    boxPackStart vbox passEntry PackGrow 0
    boxPackStart vbox loginButton PackGrow 0
    containerAdd window vbox
    widgetShowAll window


showClientScreen :: IConnection conn => conn -> Client -> IO ()
showClientScreen conn user = do
    return ()


showAdminScreen :: IConnection conn => conn -> IO ()
showAdminScreen conn = do
    -- Tworzymy okienka i zakładki.
    window <- windowNew
    notebook <- notebookNew

    clientsTab <- hBoxNew True 10
    carsTab <- hBoxNew True 10

    onDestroy window $ do
        commit conn
        mainQuit

    -- Lista klientów.
    clientsList <- getClients
    clientsStore <- listStoreNew clientsList
    clientsView <- treeViewNewWithModel clientsStore
    renderer <- cellRendererTextNew
    
    colNames <- treeViewColumnNew
    treeViewColumnSetTitle colNames "Nazwisko"
    colMails <- treeViewColumnNew
    treeViewColumnSetTitle colMails "E-mail"

    cellLayoutSetAttributes colNames renderer clientsStore
        (\client -> [ cellText := clientName client ] )
    cellLayoutSetAttributes colMails renderer clientsStore
        (\client -> [ cellText := clientEmail client ])
    
    let clientColumns = [colNames, colMails]
    mapM_ (\col -> cellLayoutPackStart col renderer False) clientColumns
    mapM_ (treeViewAppendColumn clientsView) clientColumns

    -- Lista samochodów.
    carsList <- getCars
    carsStore <- listStoreNew carsList
    carsView <- treeViewNewWithModel carsStore
    
    colBrands <- treeViewColumnNew
    treeViewColumnSetTitle colBrands "Marka"

    colCapacity <- treeViewColumnNew
    treeViewColumnSetTitle colCapacity "Pojemność"

    colType <- treeViewColumnNew
    treeViewColumnSetTitle colType "Typ silnika"

    colMaxP <- treeViewColumnNew
    treeViewColumnSetTitle colMaxP "Pasażerów"

    cellLayoutSetAttributes colBrands renderer carsStore
        (\car -> [ cellText := carBrand car ])
    cellLayoutSetAttributes colCapacity renderer carsStore
        (\car -> [ cellText := show $ carBrand car ])
    cellLayoutSetAttributes colType renderer carsStore
        (\car -> [ cellText := carEngineType car ])
    cellLayoutSetAttributes colMaxP renderer carsStore
        (\car -> [ cellText := show $ carMaxPeople car ] )

    let carColumns = [colBrands, colCapacity, colType, colMaxP]
    mapM_ (\col -> cellLayoutPackStart col renderer False) carColumns
    mapM_ (treeViewAppendColumn carsView) carColumns

    -- Dodawanie klientów.
    addClientBox <- vBoxNew True 5
    nameEntry <- entryNew
    phoneEntry <- entryNew
    emailEntry <- entryNew
    adressEntry <- entryNew
    addClientButton <- buttonNewWithLabel "Dodaj klienta"

    onClicked addClientButton $ do
        -- Stwórz nowego klienta w oparciu o wpisane dane.
        name <- entryGetText nameEntry
        phone <- entryGetText phoneEntry
        email <- entryGetText emailEntry
        address <- entryGetText adressEntry
        let newClient = Client { clientId = undefined
                               , clientName = name
                               , clientPhone = read phone
                               , clientEmail = email
                               , clientAddress = address
                               }
        -- Dodaj kilenta do listy oraz do bazy.
        listStoreAppend clientsStore newClient 
        addClient conn newClient

    -- Ładnie wszystko sklejamy i wyświetlamy.
    boxPackStart addClientBox nameEntry PackGrow 0
    boxPackStart addClientBox phoneEntry PackGrow 0
    boxPackStart addClientBox emailEntry PackGrow 0
    boxPackStart addClientBox adressEntry PackGrow 0
    boxPackStart addClientBox addClientButton PackGrow 0

    boxPackStart clientsTab clientsView PackGrow 0
    boxPackStart clientsTab addClientBox PackGrow 0
    boxPackStart carsTab carsView PackGrow 0

    notebookAppendPage notebook clientsTab "Klienci"
    notebookAppendPage notebook carsTab "Samochody"
    containerAdd window notebook
    widgetShowAll window
  where
    getClients :: IO [Client]
    getClients = do
        stmt <- prepare conn "SELECT * FROM client"
        execute stmt []

        rows <- fetchAllRows stmt
        return $ map clientFromSqlRow rows
    getCars :: IO [Car]
    getCars = do
        stmt <- prepare conn "SELECT * FROM car"
        execute stmt []

        rows <- fetchAllRows stmt
        return $ map carFromSqlRow rows


showGuestScreen :: IConnection conn => conn -> IO ()
showGuestScreen conn = do
    return ()


showReceptionScreen :: IConnection conn => conn -> IO ()
showReceptionScreen conn = do
    return ()


main :: IO ()
main = do
    conn <- connectSqlite3 "db.sqlite"
    initGUI

    showStartScreen conn
    --showAdminScreen conn

    mainGUI
