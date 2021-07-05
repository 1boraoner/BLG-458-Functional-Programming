
module Heap(
   Heap,          --
   empty',        -- Heap a
   insert',       -- insert 
   fromList',     --
   lookup',       -- 
   maxElement',   --
   delete',       --
   isValidMinHeap --
)where

exlist::[Int]
exlist = [5,1,2,4,3,6]

-- data structure definition
data Heap n = Leaf n | Branch (Maybe n, (Maybe(Heap n), Maybe(Heap n))) 
              deriving Show

empty'::Heap a    -- creates a empty heap
empty' = Branch(Nothing,(Nothing,Nothing))

backpropl :: Ord a => Heap a -> Heap a         -- left side comparison while back propagating during insertion
backpropl h@(Branch(Just hv, (Just left, right ))) = case left of
  Leaf b                  -> if b < hv then Branch(Just b, (Just (Leaf hv), right)) else h
  Branch(Just b, (rr,ll)) -> if b < hv then Branch(Just b, (Just (Branch(Just hv, (rr,ll))), right)) else h

backpropr :: Ord a => Heap a -> Heap a         -- right side comparison while back propagating during insertion
backpropr h@(Branch(Just hv, (left, Just right ))) = case right of
  Leaf b                  -> if b < hv then Branch(Just b, (left, Just (Leaf hv))) else h
  Branch(Just b, (rr,ll)) -> if b < hv then Branch(Just b, (left, Just (Branch(Just hv, (rr,ll))))) else h


depth_h:: Heap a -> Int                        -- calculates the depth of the argument sub tree used in insertion to find the proper palce to insert new element
depth_h h = depth_iter h 0        
 where 
  depth_iter::Heap a -> Int -> Int             -- finding the depth local function with accumulator (tail recursive)
  depth_iter (Leaf t) _  =  0                  -- 
  depth_iter h@(Branch(head, (left, right))) acc = case (left,right) of 
   (Just (Leaf a), Nothing) -> acc-1
   (Just (Leaf a), Just (Leaf b)) -> acc
   (Just (Branch(a,(b,c))), Just(Leaf k)) -> depth_iter (Branch(a,(b,c))) (acc) 
   (Just (Branch(_,(_,_))), Just (Branch(a,(b,c)))) -> depth_iter (Branch(a,(b,c))) (acc+1) 
    


insert' :: Ord a => Heap a -> a -> Heap a
insert' (Leaf t) x = backpropl (Branch (Just t, (Just (Leaf x),Nothing))) 
insert' (Branch (Nothing, (Nothing,Nothing))) x = Branch (Just x, (Nothing,Nothing))
insert' (Branch (head , (Nothing,Nothing))) x   = backpropl (Branch (head, (Just (Leaf x), Nothing)))
insert' (Branch (head , (left,Nothing))) x      = backpropr (Branch (head, (left, Just (Leaf x))))
insert' (Branch (head, (Just left, Just right))) x = case (left, right) of 
    (Leaf a, Leaf b)  -> backpropl (Branch (head, (Just (insert' left x), Just right)))
    (Branch (a,(b,c)), Leaf k) -> if (depth_h left) == -1 then backpropl (Branch (head, (Just (insert' left x), Just right))) 
                                  else backpropr (Branch (head, (Just left, Just (insert' right x))))
    (Branch (_,(_,_) ), Branch (_,(_,_) )) -> if (depth_h left) > (depth_h right) then backpropr (Branch (head, (Just left, Just (insert' right x))))
                                             else backpropl (Branch (head, (Just (insert' left x), Just right)))


fromList':: Ord a => [a] ->  Heap a
fromList' xs = fromListIter xs empty'
 where 
  fromListIter:: Ord a => [a] -> Heap a -> Heap a
  fromListIter [] h = h
  fromListIter (x:xs) h = fromListIter xs (insert' h x) 

lookup' :: Ord a => a -> Heap a -> Int
lookup' t (Leaf m) = if m == t then 1 else 0  
lookup' t (Branch(Nothing, (_,_)))   = 0
lookup' t h@(Branch(Just node_value, (l, r))) = case (l, r) of
    (Just left,Nothing)    -> if node_value == t then 1 else (lookup' t left) 
    (Just left,Just right) -> if node_value == t then 1 else (lookup' t left) + (lookup' t right) 
 

maxElement':: Ord a => Heap a -> Maybe a
maxElement' (Branch(Nothing, (_,_))) = Nothing
maxElement' h = Just (maxElementIter h)
 where
 maxElementIter::Ord a => Heap a -> a
 maxElementIter (Leaf t) = t
 maxElementIter h@(Branch(Just head, (left, right))) = case (left,right) of  
  (Nothing,Nothing) -> head
  (Just (Leaf b), Nothing) -> b
  (Just l, Just r)  -> (max (maxElementIter l)(maxElementIter r))



isValidMinHeap :: Ord a => Heap a -> Int
isValidMinHeap (Branch(Nothing, (Nothing,Nothing))) = 1
isValidMinHeap (Branch(Just a, (Nothing,_))) = 1
isValidMinHeap (Branch(Just a, (Just (Leaf b),Nothing))) = if a < b then 1 else 0
isValidMinHeap h@(Branch(Just nv, (Just left, Just right))) = case (left,right) of
  (Leaf a , Leaf b)                               -> if min nv (min a b) == nv then 1 else 0
  (Branch(Just x, (_,_)) , Leaf y)                -> if min nv (min x y) == nv then 1*isValidMinHeap(left) else 0
  (Branch(Just x, (_,_)) , Branch(Just y, (_,_))) -> if min nv (min x y) == nv then 1*isValidMinHeap(left)*isValidMinHeap(right) else 0



delete' ::Ord a => a -> Heap a -> Heap a
delete' target h
  | lookup' target h  == 0  = h
  | otherwise = delete_proc target h 
    where
    delete_proc :: Ord a => a -> Heap a -> Heap a 
    delete_proc target (Leaf k) = Leaf k
    delete_proc target h@(Branch(Just hv, (left,right))) = case (left,right) of
      (Just (Leaf a), Nothing)    ->  if a == target then Leaf hv else if hv == target then Leaf a  else h
      (Just(Leaf a), Just(Leaf b))-> if a == target then Branch(Just hv, (right, Nothing)) 
                                     else if b == target then Branch(Just hv, (left, Nothing)) 
                                     else if hv == target then if a < b then Branch(Just a, (right,Nothing)) else Branch(Just b, (left,Nothing))
                                     else h
      (Just (Branch(Just a, (b,c))), Just(Branch(Just k , (l,m)))) -> if hv == target then if a < k then Branch(Just a, (Just(delete_proc a (Branch(Just a, (b,c)))), right)) else Branch(Just k, (left, Just(delete_proc k (Branch(Just k, (l,m))))))
        else (Branch(Just hv, (Just (delete_proc target (Branch(Just a,(b,c))) ), Just(delete_proc target (Branch(Just k ,(l,m)))))))
      

-- kk = fromList' exlist

t1 = empty'
t2 = insert' t1 5
-- t3 = insert t2 3
-- t4 = insert t3 2
-- t5 = insert t4 1
-- t6 = insert t5 7
-- t7 = insert t6 8
-- t8 = insert t7 0
-- t9= insert t8 11
-- t10= insert t9 12
-- t11= insert t10 6
-- t12= insert t11 17
-- t13= insert t12 91
-- t14= insert t13 19
-- t15= insert t14 13
-- t16= insert t15 16


-- t = Branch (Just 21, (Just (Branch (Just 22, (Just (Leaf 25), Just (Leaf 4)))), Just (Branch (Just 12, (Just (Leaf 63), Nothing)))))

