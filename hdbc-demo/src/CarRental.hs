module Main where

import Graphics.UI.Gtk
import Database.HDBC.Sqlite3

import CarRental.Models
import CarRental.Queries
import CarRental.Interface

main :: IO ()
main = do
    conn <- connectSqlite3 "db.sqlite"
    initGUI
    showStartScreen conn
    mainGUI
