import Graphics.UI.Gtk
import Database.HDBC.Sqlite3

import Models
import Queries
import Interface

main :: IO ()
main = do
    conn <- connectSqlite3 "db.sqlite"
    initGUI
    showStartScreen conn
    mainGUI
