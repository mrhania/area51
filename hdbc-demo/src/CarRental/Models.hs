module CarRental.Models where

import Database.HDBC.SqlValue


-- Typ repezentujący klienta, zgodny z tym co siedzi w bazie danych.
data Client = Client { clientId :: Int
                     , clientName :: String
                     , clientPhone :: Integer
                     , clientEmail :: String
                     , clientAddress :: String
                     } deriving Show

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


-- Typ reprezentujący samochód, zgodny z tym co siedzi w bazie danych.
data Car = Car { carId :: Int
               , carBrand :: String
               , carCapacity :: Int
               , carEngineType :: String
               , carMaxPeople :: Int
               } deriving Show

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
