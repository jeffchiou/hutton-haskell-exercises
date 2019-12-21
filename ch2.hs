-- Exercise 5
revInit xs = reverse (tail (reverse xs))
takeInit xs = take (length xs - 1) xs
