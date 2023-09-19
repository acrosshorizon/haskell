-- | Implementation of integer valued binary trees.
--
-- Some more practise with recursion and working with Haskell syntax.
module BinTree (BinTree (..), pretty) where

-- | A BinTree is a type of tree which has two children: a left and a right.
--
-- The children can be either
-- - a Leaf with no value, or
-- - another BinTree
--
-- See https://tgdwyer.github.io/haskell2/#algebraic-data-types
data BinTree a = Leaf | Node a (BinTree a) (BinTree a)
  deriving (Show)

-- $setup
-- >>> one = Node 1 Leaf Leaf :: BinTree Int
-- >>> tree = Node 16 (Node 23 Leaf (Node 73 Leaf Leaf)) (Node 42 Leaf Leaf) :: BinTree Int
-- >>> bigTree = (Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) (Node 4 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf))) :: BinTree Int

-- | Pretty print a binary tree.
--
-- /Hint/: An auxiliary function is useful here.
--
-- /Hint 2/: Have a look at the week 2 solution below, you can use that as reference!
--
-- /Challenge/: Write this using foldTree and mapTree from Week 6.
--   Is this better than the recursive implementation?
--
-- >>> putStr $ pretty Leaf
--
--
-- >>> putStr $ pretty one
-- 1
--
-- >>> putStr $ pretty tree
-- 16
-- |- 23
-- |- |- 73
-- |- 42
--
-- >>> putStr $ pretty bigTree
-- 1
-- |- 2
-- |- |- 3
-- |- 4
-- |- |- 5
-- |- |- 6
pretty :: Show a => BinTree a -> String
pretty n = pretty' n ""
  where
    pretty' :: Show a => BinTree a -> String -> String
    pretty' Leaf _ = ""
    pretty' (Node v l r) p = p ++ show v ++ "\n" ++ left ++ right
      where
        left = pretty' l (p ++ "|- ")
        right = pretty' r (p ++ "|- ")

-- pretty :: Show a => BinTree a -> String
-- pretty n = pretty' n "|- "
--   where
--     pretty' :: Show a => BinTree a -> String -> String
--     pretty' Leaf _ = ""
--     pretty' (Node v l r) p = concat [show v, left l, right r]
--       where
--         left Leaf = ""
--         left t = "\n" ++ p ++ pretty' t (p ++ "|- ")
--         right Leaf = ""
--         right t = "\n" ++ p ++ pretty' t (p ++ "|- ")

-- pretty :: Show a => BinTree a -> String
-- pretty = trim . foldTree concat' "" . addPrefixes . mapTree show
--   where
--     trim = dropWhile (== '\n')

--     concat' acc x = acc ++ "\n" ++ x

--     addPrefixes Leaf = Leaf
--     addPrefixes (Node x l r) = Node x (addPrefixes' l) (addPrefixes' r)
--     addPrefixes' = addPrefixes . mapTree ("|- " ++)

{-

function prettifyBinaryTree<T extends Object>(node: BinaryTree<T>): string {
  function _prettifyBinaryTree(node: BinaryTree<T>, prefix: string): string {
    const current = node.data.toString();

    const left = node.left
      ? "\n" + prefix + _prettifyBinaryTree(node.left, prefix + "|- ")
      : "";
    const right = node.right
      ? "\n" + prefix + _prettifyBinaryTree(node.right, prefix + "|- ")
      : "";

    return [current, left, right].join("");
  }

  return _prettifyBinaryTree(node, "|- ");
}

-}
