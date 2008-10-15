import Distribution.Franchise

configure = return ()

main = build [] configure $ executable "hello" "Main.hs" []
