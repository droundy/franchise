import Distribution.Franchise

configure = return ()
main = build [] configure $ executable "tester" "Main.hs" ["test.c"]
