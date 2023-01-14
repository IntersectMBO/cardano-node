{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Test.Unlog.Org where

import Cardano.Prelude

import Hedgehog

import Cardano.Org
import Cardano.Util


sho :: Show a => a -> String
sho = show

out :: String -> PropertyT IO ()
out = liftIO . putStrLn

prop_Org_render_simple_table = property $ render
  Table
  { tColHeaders = ["foo", "woot", "quuxinator"]
  , tExtended = False
  , tApexHeader = Nothing
  , tRowHeaders = ["one", "two", "three"]
  , tColumns = [ ["1.0", "a", "......"]
               , ["b",   "11.0", ""]
               , ["--", "a", "111.0"]
               ]
  , tSummaryHeaders = []
  , tSummaryValues = []
  , tFormula = []
  , tConstants = []
  }
  ===
  [ "|-------+--------+------+------------|"
  , "|       |    foo | woot | quuxinator |"
  , "|-------+--------+------+------------|"
  , "|   one |    1.0 |    b |         -- |"
  , "|   two |      a | 11.0 |          a |"
  , "| three | ...... |      |      111.0 |"
  ]

prop_Org_render_summarised_simple_table = property $ render
  Table
  { tColHeaders = ["foo", "woot", "quuxinator"]
  , tExtended = False
  , tApexHeader = Nothing
  , tRowHeaders = ["one", "two", "three"]
  , tColumns = [ ["1.0", "a", "......"]
               , ["b",   "11.0", ""]
               , ["--", "a", "111.0"]
               ]
  , tSummaryHeaders = ["aaaveragee", "q"]
  , tSummaryValues = [ ["0000000", ""]
                     , ["", "0000000"]
                     , ["1", "2"]
                     ]
  , tFormula = []
  , tConstants = []
  }
  ===
  [ "|------------+---------+---------+------------|"
  , "|            |     foo |    woot | quuxinator |"
  , "|------------+---------+---------+------------|"
  , "|        one |     1.0 |       b |         -- |"
  , "|        two |       a |    11.0 |          a |"
  , "|      three |  ...... |         |      111.0 |"
  , "|------------+---------+---------+------------|"
  , "| aaaveragee | 0000000 |         |          1 |"
  , "|          q |         | 0000000 |          2 |"
  ]

prop_Org_render_extended_table = property $ render
  Props
  { oProps = [("DATE", "now")]
  , oConstants = [("pi", "3.141592653"), ("e", "2.718281828")]
  , oBody =
    [ Table
      { tColHeaders = ["foo", "woot", "quuxinator"]
      , tExtended = True
      , tApexHeader = Just "centile"
      , tRowHeaders = ["one", "two", "three"]
      , tColumns = [ ["1.0", "a", "......"]
                   , ["b",   "11.0", ""]
                   , ["--", "a", "111.0"]
                   ]
      , tSummaryHeaders = []
      , tSummaryValues = []
      , tFormula = []
      , tConstants = []
      }
    ]
  }
  ===
  [ "#+DATE: now"
  , "#+CONSTANTS: pi=3.141592653 e=2.718281828"
  , "|---+---------+--------+------+------------|"
  , "| ! | centile |    foo | woot | quuxinator |"
  , "|---+---------+--------+------+------------|"
  , "| # |     one |    1.0 |    b |         -- |"
  , "| # |     two |      a | 11.0 |          a |"
  , "| # |   three | ...... |      |      111.0 |"
  ]

prop_Org_render_extended_summarised_table = property $ render
  Props
  { oProps = [("DATE", "now")]
  , oConstants = [("pi", "3.141592653"), ("e", "2.718281828")]
  , oBody =
    [ Table
      { tColHeaders = ["foo", "woot", "quuxinator"]
      , tExtended = True
      , tApexHeader = Just "centile"
      , tRowHeaders = ["one", "two", "three"]
      , tColumns = [ ["1.0", "a", "......"]
                   , ["b",   "11.0", ""]
                   , ["--", "a", "111.0"]
                   ]
      , tSummaryHeaders = ["aaaveragee", "q"]
      , tSummaryValues = [ ["0000000", ""]
                         , ["", "0000000"]
                         , ["1", "2"]
                         ]
      , tFormula = []
      , tConstants = [ ("count",    "0")
                     , ("all",      "1")
                     , ("the",      "2")
                     , ("things",   "3")
                     , ("properly", "4")
                     ]
      }
    ]
  }
  ===
  [ "#+DATE: now"
  , "#+CONSTANTS: pi=3.141592653 e=2.718281828"
  , "|---+------------+---------+---------+------------|"
  , "| ! |    centile |     foo |    woot | quuxinator |"
  , "|---+------------+---------+---------+------------|"
  , "| # |        one |     1.0 |       b |         -- |"
  , "| # |        two |       a |    11.0 |          a |"
  , "| # |      three |  ...... |         |      111.0 |"
  , "|---+------------+---------+---------+------------|"
  , "| # | aaaveragee | 0000000 |         |          1 |"
  , "| # |          q |         | 0000000 |          2 |"
  , "|---+------------+---------+---------+------------|"
  , "| _ |      count |     all |     the |     things |"
  , "| # |          0 |       1 |       2 |          3 |"
  , "| _ |   properly |         |         |            |"
  , "| # |          4 |         |         |            |"
  ]

tests :: IO Bool
tests =
  checkSequential $$discover
