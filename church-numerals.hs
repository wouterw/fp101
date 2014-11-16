type Church = (a -> a) -> a -> a

curchToInteger x = x (+1) 0
curchToInteger two = (\s z -> s (s z)) (+1) 0

curchToString x = x ('*':) ""
curchToString two = (\s z -> s (s z)) ('*':) ""

add x y = \s z -> x s (y s z)
mul x y = \s z -> x (y s) z
