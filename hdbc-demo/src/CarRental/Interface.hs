module CarRental.Interface where

import Database.HDBC
import Graphics.UI.Gtk

import CarRental.Models
import CarRental.Queries


-- Ekran startowy: tutaj wybieramy rodzaj logowania i przechodzimy do innych.
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
                    _           -> return ()
            Just "gość"     ->
                showGuestScreen conn
            Just "recepcja" ->
                showReceptionScreen conn
            _ ->
                return ()

    let setEditability val = do
        set loginEntry [ entryEditable := val ]
        set passEntry  [ entryEditable := val ]

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


-- Ekran admina: możemy dodawać i usuwać samochody.
showAdminScreen :: IConnection conn => conn -> IO ()
showAdminScreen conn = do
    -- Tworzymy okienka i zakładki.
    window   <- windowNew
    notebook <- notebookNew

    onDestroy window $ do
        commit conn
        mainQuit

    clientsTab <- createClientsTab
    carsTab    <- createCarsTab
    
    notebookAppendPage notebook clientsTab "Klienci"
    notebookAppendPage notebook carsTab "Samochody"
    containerAdd window notebook
    
    widgetShowAll window
  where
    -- Tworzy zakładkę z listą klientów.
    createClientsTab :: IO HBox
    createClientsTab = do
        clientsTab <- hBoxNew True 10

        clientsList  <- getClients conn
        clientsStore <- listStoreNew clientsList
        clientsView  <- treeViewNewWithModel clientsStore
        renderer     <- cellRendererTextNew
        
        colNames <- treeViewColumnNew
        colMails <- treeViewColumnNew

        let clientColumns = [colNames, colMails]
        mapM_ (uncurry treeViewColumnSetTitle) $
            zip clientColumns ["Nazwisko", "E-mail"]
        mapM_ (\col -> cellLayoutPackStart col renderer False) clientColumns
        mapM_ (treeViewAppendColumn clientsView) clientColumns

        cellLayoutSetAttributes colNames renderer clientsStore $
            (\client -> [ cellText := clientName client ] )
        cellLayoutSetAttributes colMails renderer clientsStore $
            (\client -> [ cellText := clientEmail client ])

        addClientBox <- createAddClientBox clientsStore

        boxPackStart clientsTab clientsView PackGrow 0
        boxPackStart clientsTab addClientBox PackGrow 0

        return clientsTab
      where
        -- Tworzy kontrolki do dodawania klientów.
        createAddClientBox :: ListStore Client -> IO VBox
        createAddClientBox clientsStore = do
            addClientBox <- vBoxNew True 5

            nameEntry       <- entryNew
            phoneEntry      <- entryNew
            emailEntry      <- entryNew
            adressEntry     <- entryNew
            addClientButton <- buttonNewWithLabel "Dodaj klienta"

            onClicked addClientButton $ do
                -- Stwórz nowego klienta w oparciu o wpisane dane.
                name    <- entryGetText nameEntry
                phone   <- entryGetText phoneEntry
                email   <- entryGetText emailEntry
                address <- entryGetText adressEntry
                let newClient = Client { clientId      = undefined
                                       , clientName    = name
                                       , clientPhone   = read phone
                                       , clientEmail   = email
                                       , clientAddress = address
                                       }
                -- Dodaj kilenta do listy oraz do bazy.
                listStoreAppend clientsStore newClient 
                addClient conn newClient

            boxPackStart addClientBox nameEntry PackGrow 0
            boxPackStart addClientBox phoneEntry PackGrow 0
            boxPackStart addClientBox emailEntry PackGrow 0
            boxPackStart addClientBox adressEntry PackGrow 0
            boxPackStart addClientBox addClientButton PackGrow 0

            return addClientBox

    -- Tworzy zakładkę z listą samochodów.
    createCarsTab :: IO HBox
    createCarsTab = do
        carsTab <- hBoxNew True 10

        carsList  <- getCars conn
        carsStore <- listStoreNew carsList
        carsView  <- treeViewNewWithModel carsStore
        renderer  <- cellRendererTextNew
        
        colBrands   <- treeViewColumnNew
        colCapacity <- treeViewColumnNew
        colType     <- treeViewColumnNew
        colMaxP     <- treeViewColumnNew

        let carColumns = [colBrands, colCapacity, colType, colMaxP]
        mapM_ (uncurry treeViewColumnSetTitle) $
            zip carColumns ["Marka", "Pojemność", "Typ silnika", "Pasażerów"]
        mapM_ (\col -> cellLayoutPackStart col renderer False) carColumns
        mapM_ (treeViewAppendColumn carsView) carColumns
        
        cellLayoutSetAttributes colBrands renderer carsStore $
            (\car -> [ cellText := carBrand car ])
        cellLayoutSetAttributes colCapacity renderer carsStore $
            (\car -> [ cellText := show $ carBrand car ])
        cellLayoutSetAttributes colType renderer carsStore $
            (\car -> [ cellText := carEngineType car ])
        cellLayoutSetAttributes colMaxP renderer carsStore $
            (\car -> [ cellText := show $ carMaxPeople car ] )

        boxPackStart carsTab carsView PackGrow 0

        return carsTab


-- TODO
showClientScreen :: IConnection conn => conn -> Client -> IO ()
showClientScreen conn user = do
    return ()


-- TODO
showGuestScreen :: IConnection conn => conn -> IO ()
showGuestScreen conn = do
    return ()


-- TODO
showReceptionScreen :: IConnection conn => conn -> IO ()
showReceptionScreen conn = do
    return ()
