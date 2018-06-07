lastButOne :: [a] -> a
lastButOne lst = if (length lst) > 1
                 then head (drop ((length lst) - 2) lst)
                 else error "The list is too short!"
