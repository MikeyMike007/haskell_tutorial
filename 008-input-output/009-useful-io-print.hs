-- print takes a value of any type that’s an instance of Show (meaning that
-- we know how to represent it as a string), applies show to that value to
-- “stringify” it, and then outputs that string to the terminal. Basically, it’s
-- just putStrLn . show. It first runs show on a value, and then feeds that to
-- putStrLn, which returns an I/O action that will print out our value.
main = do
  print True
  print 2
  print "haha"
  print 3.2
  print [3, 4, 3]
