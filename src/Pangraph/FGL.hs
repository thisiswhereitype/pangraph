{-
Module          : Pangraph.FGL
Description     : Provides `convert` and `revert` to a `FGL` form.

The function provides an conversion to FGL in the datatypes `Pangraph` uses.
Users should convert the types as the see fit for example, convert `ByteString` to `Int`.
-}
{-# LANGUAGE OverloadedStrings #-}

module Pangraph.FGL (convert, revert) where

-- External Imports
-- ByteString
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)

-- FGL
import qualified Data.Graph.Inductive.Graph as FGL

-- Containers
import Data.Set (Set)
import qualified Data.Set as Set

-- Prelude
import Data.Maybe (fromJust)
import Data.Monoid ((<>))

-- Local
import Pangraph
import Pangraph.Internal.ProtoGraph

-- | Convert a Pangraph to Fgl types.
convert :: Pangraph -> ([FGL.LNode ByteString], [FGL.LEdge Int])
convert p = let
    -- Create the set of VertexIDs for crossreference when generating FGL.LEdge
    vertexSet :: Set VertexID
    vertexSet = (Set.fromList . map vertexID . vertexList) p
    -- The list of labelled vertices
    fglVertices :: [(Int, VertexID)]
    fglVertices = zip [0..] (Set.toAscList vertexSet)
    -- A helper function for cross referencing a Pangraph Vertex in its order in the set. This index forms a key in FGL.
    findIndexOfVertex :: VertexID -> Int
    findIndexOfVertex v = Set.findIndex v vertexSet
    -- Find the FGL.Node of the Endpoints, using the Set in VertexID.
    -- Safely fromJust the edgeID as its emergence from a Pangraph type enforces it Just.
    -- The id is formed from the order in the list provided by `vertices` which are guaranteed to be unique by the pangraph type.
    fglEdges :: [(FGL.Node, FGL.Node, Int)]
    fglEdges = let
        in map ((\(e, (a,b)) ->
        (findIndexOfVertex a, findIndexOfVertex b, e)) .
        (\e -> ((fromJust . edgeID) e, edgeEndpoints e))) (edgeList p)
    in (fglVertices, fglEdges)

-- (Int, ByteString) -> (Int, Int, Int)
-- | Revert FGL types into Pangraph. 
revert :: ([FGL.LNode ByteString], [FGL.LEdge Int]) -> Maybe Pangraph
revert t = let
    vf :: ProtoVertex -> VertexID
    vf v = (fromJust . lookup "id") (protoVertexAttributes v)
    ef :: ProtoEdge -> (VertexID, VertexID)
    ef e = let
        lookup' :: Value -> VertexID
        lookup' value = (fromJust . lookup value) (protoEdgeAttributes e)
        in (lookup' "source", lookup' "target")
    in buildPangraph (FGL t) vf ef

newtype FGL = FGL ([FGL.LNode ByteString], [FGL.LEdge Int])

instance BuildPangraph FGL where
    getProtoVertex (FGL (ns, _)) = map (\n -> makeProtoVertex [("id", snd n)]) ns
    getProtoEdge (FGL (_, es)) = let
        ps :: Show a => a -> ByteString
        ps = pack . show
        -- Take the source and destination and construct the protoEdge
        in map (\(src, dst, _) -> makeProtoEdge [("source", "n" <> ps src), ("target", "n" <> ps dst)]) es
