module Pangraph.AlgebraicGraphs
    ( toAlga
    ) where

import qualified Algebra.Graph as G
import Pangraph

toAlga :: Pangraph -> G.Graph Vertex
toAlga p = G.graph (vertices p) (map edgeEndpoints $ edges p)
