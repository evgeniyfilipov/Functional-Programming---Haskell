type Node = Int
type Treetd = [(Node, [Node])]
type Path = [Node]

assoc :: Node -> [(Node, [Node])] -> (Node, [Node])
assoc key [] = (key, [])
assoc key (x:xs)
  | fst x == key = x
  | otherwise    = assoc key xs

successors :: Node -> Treetd -> [Node]
successors node tree = snd (assoc node tree)

parent :: Node -> Treetd -> Node
parent node []                             = 0
parent node tree
  | elem node (successors first_node tree) = first_node
  | otherwise                              = parent node (tail tree)
  where                         first_node = fst (head tree)


tree1 :: Treetd
tree1 = [(10,[3,7,12]), (3,[5,8,9]), (7,[11,13]), (12,[6,4]), (8,[1,2])]

-- Task 01
maxValueFromLstFunctions :: (Num a, Ord a) => [(a -> a)] -> (a -> a)
maxValueFromLstFunctions fs = (\x -> maximum [ f x | f <- fs ]) 

-- Task 02

parentEqSucc :: Node -> Treetd -> Bool
parentEqSucc x xs = if parent x xs == sum(successors x xs) then True else False

takeSnd :: [(a, b)] -> [b]
takeSnd list = [ y | (x, y) <- list ]

numOfNodes :: Treetd -> Int
numOfNodes tree = length [x | x <- concat(takeSnd tree), parentEqSucc x tree]

-- Task 03
data BTree = Empty | Node Int BTree BTree

treeNodesAtLevel :: BTree -> Int -> [Int]
treeNodesAtLevel Empty _               = []
treeNodesAtLevel (Node x _ _) 0        = [x]
treeNodesAtLevel (Node _ left right) n = treeNodesAtLevel left (n - 1) ++ treeNodesAtLevel right (n - 1)

grandHelper :: BTree -> Bool
grandHelper (Node val left right) = if all (> val + 1) (treeNodesAtLevel left 1) && all (> val + 1) (treeNodesAtLevel right 1) then True else False

grandChildrenIncreased :: BTree -> Bool
grandChildrenIncreased Empty                 = True
grandChildrenIncreased (Node _ Empty Empty)  = True
grandChildrenIncreased (Node val left right) = grandHelper (Node val left right) && grandChildrenIncreased left && grandChildrenIncreased right

 {- 
                                tree2: 

                                  5
                                /   \
                               2     7
                              / \   / \
                             7   6     8
                            / \ / \   / \
                                         9
    -}
tree2 :: BTree
tree2 = (Node 5 (Node 2 (Node 7 Empty Empty) (Node 6 Empty Empty)) (Node 7 Empty (Node 8 Empty (Node 9 Empty Empty)))) -- False, защото при проверка на първите внуци с корена имаме 6 > 6.

main :: IO()
main = do
  print $ maxValueFromLstFunctions [(\x -> x), (\x -> x * x), (\x -> x * x * x), (\x -> x * x * x * x)] 2
  
  print $ parent 8 tree1
  print $ parentEqSucc 1 tree1
  print $ numOfNodes tree1

  print $ grandChildrenIncreased tree2


