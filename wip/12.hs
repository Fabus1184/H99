data Single = Single Char
data Multiple = Multiple Int Char

main = do
    let x = [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
    return ()
