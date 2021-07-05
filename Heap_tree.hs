----------------------------------------------------
--     BLG458E FUNCTIONAL PROGRAMMING HOMEWORK#2   -
--     Name Surname  = Bora Oner                   -
--     Student Id    = 150170301                   -
----------------------------------------------------


-- implemented empty', insert', fromList', lookup', maxElement', delete', isValidMinHeap' function

-- also helper functions backpropl, backpropr and depth_h are implemented to be used during the insertion task


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


depth_h:: Heap a -> Int                        -- calculates the depth of the argument sub tree used in insertion to find the proper place to insert new element
depth_h h = depth_iter h 0        
 where 
  depth_iter::Heap a -> Int -> Int             -- finding the depth local function with accumulator (tail recursive)
  depth_iter (Leaf t) _  =  0                  -- if a leaf node the depth contribution is 0
  depth_iter h@(Branch(head, (left, right))) acc = case (left,right) of  -- pattern mathing on the left and right sub trees
   (Just (Leaf a), Nothing) -> acc-1           -- if a nothing is encountered the depth is decreased by 1
   (Just (Leaf a), Just (Leaf b)) -> acc       -- leaf leaf combination outputs the acc itself
   (Just (Branch(a,(b,c))), Just(Leaf k)) -> depth_iter (Branch(a,(b,c))) (acc) -- Branch, Leaf combanation encountered, the left sub tree must be selected
   (Just (Branch(_,(_,_))), Just (Branch(a,(b,c)))) -> depth_iter (Branch(a,(b,c))) (acc+1) -- always go towards the right sub tree because Nothing is in the right
    


insert' :: Ord a => Heap a -> a -> Heap a       -- inserts a new value to the heap
insert' (Leaf t) x = backpropl (Branch (Just t, (Just (Leaf x),Nothing)))            -- if a leaf is reached the insertion is made and backprople is called
insert' (Branch (Nothing, (Nothing,Nothing))) x = Branch (Just x, (Nothing,Nothing)) -- inserting to empty heap
insert' (Branch (head , (Nothing,Nothing))) x   = backpropl (Branch (head, (Just (Leaf x), Nothing))) -- if now leaves
insert' (Branch (head , (left,Nothing))) x      = backpropr (Branch (head, (left, Just (Leaf x))))    -- if one leaf
insert' (Branch (head, (Just left, Just right))) x = case (left, right) of                            -- pattern matching of the two subtrees right and left
    (Leaf a, Leaf b)  -> backpropl (Branch (head, (Just (insert' left x), Just right)))               -- the last level is reached with 2 child nodes
    (Branch (a,(b,c)), Leaf k) -> if (depth_h left) == -1 then backpropl (Branch (head, (Just (insert' left x), Just right))) -- left child is branch and right is leaf --> if Branch has Nothing in it go there else insert left
                                  else backpropr (Branch (head, (Just left, Just (insert' right x))))
    (Branch (_,(_,_) ), Branch (_,(_,_) )) -> if (depth_h left) > (depth_h right) then backpropr (Branch (head, (Just left, Just (insert' right x))))
                                             else backpropl (Branch (head, (Just (insert' left x), Just right)))
    -- Branch Branch pattern -> compare the depths of the right and left subtree if the depth of left is more than traverse towards the right subtree else go left  



fromList':: Ord a => [a] ->  Heap a      -- constructs a heap from a given list
fromList' xs = fromListIter xs empty'    -- fromListIter starts from an empty Heap
 where 
  fromListIter:: Ord a => [a] -> Heap a -> Heap a --local defined creating a heap recursively
  fromListIter [] h = h                           -- if the list is exhausted return the final heap
  fromListIter (x:xs) h = fromListIter xs (insert' h x) -- insert each element to the heap

lookup' :: Ord a => a -> Heap a -> Int    -- search the heap in order to identify whether the target element is in the heap
lookup' t (Leaf m) = if m == t then 1 else 0  -- if the Leaf of the heap is reached check if the node value equals the target
lookup' t (Branch(Nothing, (_,_)))   = 0      -- if the empty heap --> return 0 
lookup' t h@(Branch(Just node_value, (l, r))) = case (l, r) of 
    (Just left,Nothing)    -> if node_value == t then 1 else (lookup' t left)  -- if one node branch is encountered 
    (Just left,Just right) -> if node_value == t then 1 else (lookup' t left) + (lookup' t right) -- if recursively visit each node 
 

maxElement':: Ord a => Heap a -> Maybe a       -- returns the max element of the heap -->
maxElement' (Branch(Nothing, (_,_))) = Nothing -- if the empty heap -> Nothing
maxElement' h = Just (maxElementIter h)        
 where
 maxElementIter::Ord a => Heap a -> a          -- maxElementIter is a local function that outputs the max element
 maxElementIter (Leaf t) = t                   -- if the Leaf is reached return the node value
 maxElementIter h@(Branch(Just head, (left, right))) = case (left,right) of  -- patter matching applied on the children of the heap
  (Nothing,Nothing) -> head                    -- a heap with only the root node
  (Just (Leaf b), Nothing) -> b                -- a node with one leaf 
  (Just l, Just r)  -> (max (maxElementIter l)(maxElementIter r))  -- the left branch and right branch is called and their max outof them are compared



delete' ::Ord a => a -> Heap a -> Heap a     -- deletes a target node from the heap
delete' target h                             -- pattern mathching applied
  | lookup' target h  == 0  = h              -- check the heap if the element exists in the heap 
  | otherwise = delete_proc target h         -- if the element in the heap visit each node and find the value to delete
    where
    delete_proc :: Ord a => a -> Heap a -> Heap a -- local defined function to traverse the tree and delete the value 
    delete_proc target (Leaf k) = Leaf k          -- the Leaf node is reached bounce back
    delete_proc target h@(Branch(Just hv, (left,right))) = case (left,right) of  --pattern matching applied on the left and right sub-tree
      (Just (Leaf a), Nothing)    -> if a == target then Leaf hv else if hv == target then Leaf a  else h  -- one leaf node in the tree 
      
      (Just(Leaf a), Just(Leaf b))-> if a == target then Branch(Just hv, (right, Nothing))      -- delete the left child 
                                     else if b == target then Branch(Just hv, (left, Nothing))  -- delete the right child
                                     else if hv == target then if a < b then Branch(Just a, (right,Nothing)) else Branch(Just b, (left,Nothing)) -- delete the parent value and replace by the min child
                                     else h  -- if nothing is equal return the branch as it is
      (Just(Branch(Just a, (b,c))), Just(Leaf k)) -> if k == target then (Branch(Just hv, (Just(Branch(Just a, (b,Nothing))) ,c))) 
                                                   else (Branch(Just hv, (Just (delete_proc target (Branch(Just a,(b,c)))) , Just(Leaf k))))
      
      (Just (Branch(Just a, (b,c))), Just(Branch(Just k , (l,m)))) -> if hv == target then if a < k then Branch(Just a, (Just(delete_proc a (Branch(Just a, (b,c)))), right)) else Branch(Just k, (left, Just(delete_proc k (Branch(Just k, (l,m))))))
        else (Branch(Just hv, (Just (delete_proc target (Branch(Just a,(b,c))) ), Just(delete_proc target (Branch(Just k ,(l,m)))))))
        
        --Branch Branch pattern -> if target delete is one of the branches' value then recursively delete the sub tree if the upper branches value
        --is wanted to be deleted then the minimum valued child replaces it and the deleting of the replaced node is done recursively        


isValidMinHeap' :: Ord a => Heap a -> Int                   -- checks if the heap is a proper_valid heap
isValidMinHeap' (Branch(Nothing, (Nothing,Nothing))) = 1    -- empty heap is a valid heap tree
isValidMinHeap' (Branch(Just a, (Nothing,_))) = 1           -- a heap with only root node is a valid heap
isValidMinHeap' (Branch(Just a, (Just (Leaf b),Nothing))) = if a < b then 1 else 0 --a node with only one child compare with the child and parent
isValidMinHeap' h@(Branch(Just nv, (Just left, Just right))) = case (left,right) of
  (Leaf a , Leaf b)                               -> if min nv (min a b) == nv then 1 else 0    -- a node with two children the min element of three must be the head node's value
  (Branch(Just x, (_,_)) , Leaf y)                -> if min nv (min x y) == nv then 1*isValidMinHeap'(left) else 0  -- compare the three value min should be the parent
  (Branch(Just x, (_,_)) , Branch(Just y, (_,_))) -> if min nv (min x y) == nv then 1*isValidMinHeap'(left)*isValidMinHeap'(right) else 0  -- compare the three value min should be the parent
