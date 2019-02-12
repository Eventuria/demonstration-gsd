module System.MyPrelude where

unlift :: Either a a -> a
unlift either = case either of
                  Right a -> a
                  Left a -> a