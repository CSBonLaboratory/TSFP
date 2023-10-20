module NestedList where

import Classes
import List ()

{-
    Define the 'NestedList' type, for handling lists with an arbitrary
    level of intricateness, which would be impossible to specify
    using plain Haskell lists. For instance, the String representation
    of such a nested list might be "[1, [2, 3], 4, 5]".
    
    (ConsList 1 (InnerList (ConsList 2 (ConsList 3 ListEnd)) (ConsList 4 (ConsList 5 ListEnd ))))

    Instantiate the following classes with the nested list type:
    * 'Show'
    * 'Functor'
    * 'Container'
    * 'Invertible'
    
    The inversion should be performed DEEPLY i.e., for the elements as well.
-}

data NestedList a = ListEnd | ConsList a (NestedList a) | InnerList (NestedList a) (NestedList a)

instance Show a => (Show (NestedList a)) where
    show ListEnd = "[]"
    show nested = "[" ++ (showHelper nested) ++ "]" where
        showHelper ListEnd = "[]"
        showHelper (ConsList atom ListEnd) = (show atom)
        showHelper (ConsList atom tailList) = (show atom) ++ ", " ++ (showHelper tailList)
        showHelper (InnerList brickedList ListEnd) = "[" ++ (showHelper brickedList) ++ "]"
        showHelper (InnerList brickedList tailList) = "[" ++ (showHelper brickedList) ++ "], " ++ (showHelper tailList)

instance Functor NestedList where
    fmap _ ListEnd = ListEnd
    fmap f (ConsList atom tailList) = (ConsList (f atom) (fmap f tailList))
    fmap f (InnerList brickedList tailList) = (InnerList (fmap f brickedList) (fmap f tailList))

instance Container NestedList where
    contents ListEnd = []
    contents (ConsList atom tailList) = atom : (contents tailList)
    contents (InnerList brickedList tailList) = (contents brickedList) ++ (contents tailList)