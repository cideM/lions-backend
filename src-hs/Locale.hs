{-# LANGUAGE OverloadedStrings #-}

module Locale (german) where

import qualified Data.Time as Time

german :: Time.TimeLocale
german =
  Time.TimeLocale
    { Time.wDays =
        [ ("Sonntag", "So"),
          ("Montag", "Mo"),
          ("Dienstag", "Di"),
          ("Mittwoch", "Mi"),
          ("Donnerstag", "Do"),
          ("Freitag", "Fr"),
          ("Samstag", "Sa")
        ],
      Time.months =
        [ ("Januar", "Jan"),
          ("Februar", "Feb"),
          ("März", "Mär"),
          ("April", "Apr"),
          ("Mai", "Mai"),
          ("Juni", "Jun"),
          ("Juli", "Jul"),
          ("August", "Aug"),
          ("September", "Sep"),
          ("Oktober", "Okt"),
          ("November", "Nov"),
          ("Dezember", "Dez")
        ],
      Time.amPm = ("Uhr", "Uhr"),
      Time.dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y",
      Time.dateFmt = "%d.%m.%y",
      Time.timeFmt = "%H:%M:%S",
      Time.time12Fmt = "%I:%M:%S %p",
      Time.knownTimeZones = Time.knownTimeZones Time.defaultTimeLocale
    }

