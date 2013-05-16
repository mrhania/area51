module CarRental.Queries where

import Database.HDBC

import CarRental.Models


-- Zwraca klienta odpowiadającego zadanemu loginowi i hasłu (haśle?).
loginClient :: IConnection conn => conn
                                -> String -> String
                                -> IO (Maybe Client)
loginClient conn login pass = do
    stmt <- prepare conn query
    execute stmt [toSql login, toSql pass]
    
    row <- fetchRow stmt
    return $ fmap clientFromSqlRow row
  where
    -- W naszym świecie loginem i hasłem są email i numer telefonu.
    query = "SELECT * FROM client WHERE email = ? AND phone = ?"


-- Dodaje nowego klienta do bazy danych. Atrybut `clientId` jest ignorowany.
addClient :: IConnection conn => conn -> Client -> IO ()
addClient conn client = do
    stmt <- prepare conn query
    execute stmt . tail . clientToSqlRow $ client
    return ()
  where
    query = "INSERT INTO client (name, phone, email, adress) " ++
            "VALUES (?, ?, ?, ?)"

-- Dodaje nowy samochód do bazy danych. Atrybut `carId` jest ignorowany.
addCar :: IConnection conn => conn -> Car -> IO ()
addCar conn car = do
    stmt <- prepare conn query
    execute stmt . tail . carToSqlRow $ car
    return ()
  where
    query = "INSERT INTO car (brand, capacity, type, maxp) " ++
            "VALUES (?, ?, ?, ?)"

-- Ściąga z bazy listę klientów.
getClients :: IConnection conn => conn -> IO [Client]
getClients conn = do
    stmt <- prepare conn "SELECT * FROM client"
    execute stmt []

    rows <- fetchAllRows stmt
    return $ map clientFromSqlRow rows


-- Ściąga z bazy listę samochodów.
getCars :: IConnection conn => conn -> IO [Car]
getCars conn = do
    stmt <- prepare conn "SELECT * FROM car"
    execute stmt []

    rows <- fetchAllRows stmt
    return $ map carFromSqlRow rows
