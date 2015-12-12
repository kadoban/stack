import StackTest

main :: IO ()
main = do stack ["build"]
          stack ["exec", "以-exe"]
          stackErr ["build", "--flag", "以:लकरतअ"]
          stack    ["build", "--flag", "以:लकरतअ", "--flag", "以:恵比毛勢須"]
