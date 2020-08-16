module Main where

import Lib
import System.Cmd

main :: IO ()
main =
  do
    -- create database and database schema if necessary
    system "/bin/sh /app/wait-for database:5432"
    system "psql -U postgres -h database -f /app/schema.sql"
    startApp
