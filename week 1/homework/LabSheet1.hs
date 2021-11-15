square x = x * x

pyth x y = x * x + y * y

isTriple x y z = if pyth x y == square z 
    then True
    else False

isTripleAny x y z = if pyth x y == square z
    then True
    else if pyth y z == square x
        then True
    else if pyth x z == square y
        then True
    else False