module Utils.Time exposing (formatDate,formatDecoder, formatTime)

import DateFormat as DF
import Time
import Task

formatDate : Time.Posix -> String
formatDate =
    DF.format
        [ DF.dayOfMonthNumber
        , DF.text " "
        , DF.monthNameFull
        , DF.text " "
        , DF.yearNumber
        ]
        Time.utc
formatTime : Time.Posix -> String
formatTime =
    DF.format
        [ DF.hourMilitaryFromOneFixed
        , DF.text ":"
        , DF.minuteFixed
        ]
        Time.utc
formatDatePost : Time.Posix -> String
formatDatePost =
    DF.format
        [ DF.dayOfMonthNumber
        , DF.text " "
        , DF.monthNameFull
        , DF.text " "
        , DF.yearNumber
        ]
        Time.utc

formatDecoder : Time.Posix -> String
formatDecoder =
    DF.format
        [ DF.yearNumber
        , DF.text "-"
        , DF.monthNumber
        , DF.text "-"
        , DF.dayOfMonthNumber
        , DF.text "T"
        , DF.hourNumber
        , DF.text ":"
        , DF.minuteNumber
        , DF.text ":"
        , DF.secondNumber
        ]
        Time.utc
